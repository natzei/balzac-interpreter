package org.balzaclang

import java.util.concurrent.{BlockingQueue, SynchronousQueue, TimeUnit}

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.pattern.{AskTimeoutException, ask}
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

object Main extends App {

  // Create the 'helloAkka' actor system
  val system: ActorSystem = ActorSystem("helloAkka")

  implicit val timeout: Timeout = Timeout(5 seconds)

  // Create the printer actor
  val conf: ActorRef = system.actorOf(Configuration.props, "configurationActor")
  conf ! "start"

  println("Main thread finished")
}


/*
 * Configuration
 */
object Configuration {
  def props: Props = Props[Configuration]
}

class Configuration extends Actor {

  implicit val timeout: Timeout = Timeout(3 seconds)

  val blockchain: ActorRef = context.actorOf(Blockchain.testBlockchainProps, "blockchainActor")
  val participants: Map[String,ActorRef] = Map(
    ("Alice", context.actorOf(Alice.props("Alice",this), "aliceActor")),
    ("Bob", context.actorOf(Bob.props("Bob", this), "bobActor"))
  )

  override def receive: Receive = {
    case "start" =>

      println("starting all participants")

      val startResults: Iterable[(ActorRef, Future[Any])] = participants.values.map(p => (p, p.?("start")))

      val foo : Iterable[Future[(ActorRef, Future[Any])]] =

        startResults map {
          case (p, startResult) =>
            startResult.mapTo[Future[Any]].map( {             // take the Future returned from 'start'
              f =>                                            // f is the Future triggered by 'start'
                println(s"participant started: $p")
                (p, f)
            })
        }

      val composed = Future.sequence(foo)

      composed onComplete {
        case Success(l) =>
          println("all participants started successfully")
          l foreach {
            case (actor, future) =>
              future onComplete {
                case Success(s) => println(s"actor $actor COMPLETED")
                case Failure(e) => println(s"actor $actor FAILED: $e")
              }
          }
          Future.sequence(l.map({_._2})).andThen{
            case _ => context.system terminate
          } onComplete {
            case Success(_) => println("all participant completed normally")
            case Failure(e) => println("some participant failed: "+e)
          }

        case Failure(e) =>
          println("some participant failed to start: "+e)
          context.system terminate
      }
  }
}

/*
 * Blockchain Actor
 */
object Blockchain {
  def testBlockchainProps: Props = Props[TestBlockchain]

  // permitted messages
  final case class Put(tx: String)
  final case class PutResponse(txid: String)

  final case class Get(txid: String)
}

class TestBlockchain extends Actor with ActorLogging {
  import Blockchain._

  var txs : Map[String,String] = Map()
  var nextTxid = 0

  override def receive: Receive = {
    case Put(tx) =>
      log.info(s"mining transaction $tx")
      val txid = this.nextTxid.toString
      this.nextTxid = this.nextTxid+1
      Future {
        Thread.sleep(5000)
        txs = txs.+((txid,tx))
        log.info(s"($txid,$tx) mined! $txs")
      }
      sender() ! PutResponse(txid)

    case Get(txid) =>
      log.info(s"Get($txid) :${txs.get(txid)}")
      sender() ! txs.get(txid)

    case _ => log.info("unknown message received")
  }
}

/*
 * Participant Actor
 */
object Participant {
  final case class Send(value: AnyVal, from: ActorRef)
}

abstract class Participant(val name: String, configuration: Configuration) extends Actor with ActorLogging {

  import Participant._
  import Blockchain._

  def execute(): Unit
  private val executePromise: Promise[AnyVal] = Promise[AnyVal]()

  private var buffers: Map[String,BlockingQueue[AnyVal]] = Map()

  val blockchain: ActorRef = configuration.blockchain

  def participantByName(name: String): Option[ActorRef] = configuration.participants.get(name)

  def getBuffer(actor: ActorRef): BlockingQueue[AnyVal] = buffers.get(actor.path.name) match {
    case Some(buffer) =>
      log.debug(s"returning buffer $buffer for actor ${actor.path.name}")
      buffer
    case None =>
      val buffer = new SynchronousQueue[AnyVal]()
      buffers = buffers.+((actor.path.name, buffer))
      log.debug(s"returning NEW buffer $buffer for actor ${actor.path.name}")
      buffer
  }

  override def receive: Receive = {

    case "start" =>
      log.info("starting "+self)
      execute()
      sender ! executePromise.future

    case Send(value,from) =>
      log.info("received value {} from {} ", value, from.path)
      getBuffer(from).put(value)
      log.info("value {} saved ", value)
      sender ! true

    case PutResponse(txid) => /* handle response for blockchain ! Put(tx) */
    case msg => log.warning("unknown message {}", msg)
  }

  def put[B](tx: String, continuation: Any => B): Unit = {
    put(tx, continuation, Timeout(30 days))
  }

  def put[B](tx: String, continuation: Any => B, timeout: Timeout): Unit = {
    put(tx, continuation, timeout, _ => {})
  }

  def put[B](tx: String, continuation: Any => B, timeout: Timeout, recoverContinuation: Any => B): Unit = {
    implicit val timeout: Timeout = Timeout(30 days)
    (blockchain ? Put(tx))
      .mapTo[PutResponse]
      .foreach {
        case PutResponse(txid) =>

          val isMinedPromise = Promise[Boolean]()
          val deadline: Deadline = timeout.duration fromNow

          log.info("start polling the blockchain with Get()")
          val cancellable = context.system.scheduler.schedule(0 milliseconds , 1 seconds) {
            (blockchain ? Get(txid))
              .mapTo[Option[String]]
              .foreach {
                case Some(_) => isMinedPromise success true
                case None =>
                  if (deadline.isOverdue()){
                    // not mined in time
                    isMinedPromise success false
                  }
              }
          }

          isMinedPromise.future.foreach {
            isMined =>
              // Neither the tx is mined OR timeout is expired
              cancellable.cancel()
              if (isMined) {
                // mined
                log.info(s"transaction $txid is mined!")
                continuation(txid)
              }
              else {
                // not mined YET (it could be in the future)
                log.info(s"transaction $txid not mined within the deadline")
                recoverContinuation(txid)
              }
          }
      }
  }

  def askTx(txid: String, continuation: Any => Any): Unit = {
    askTx(txid, continuation, Timeout(30 days))
  }

  def askTx(txid: String, continuation: Any => Any, timeout: Timeout = Timeout(30 days)): Unit = {
    askTx(txid, continuation, timeout, _ => {})
  }

  def askTx(txid: String, continuation: Any => Any, timeout: Timeout = Timeout(30 days), recoverContinuation: Any => Any = _ => {}): Unit = {
    implicit val timeout: Timeout = Timeout(30 days)
    val transactionPromise = Promise[Option[String]]()
    val deadline: Deadline = timeout.duration fromNow

    log.info("start polling the blockchain with Get()")
    val cancellable = context.system.scheduler.schedule(0 milliseconds , 1 seconds) {
      (blockchain ? Get(txid))
        .mapTo[Option[String]]
        .foreach {
          case s @ Some(_) => transactionPromise success s
          case None =>
            if (deadline.isOverdue()){
            // not mined in time
              transactionPromise success None
            }
        }
    }

    transactionPromise.future.foreach( x => {
      // Neither the tx is retrieved OR timeout is expired
      cancellable.cancel()
      x match {
        case Some(_) =>
          log.info(s"transaction $txid fetched!")
          continuation(txid)
        case None =>
          // not found (it could be in the future)
          log.info(s"transaction $txid not appeared within the deadline")
          recoverContinuation(txid)
      }
    })
  }

  def send[B](value: AnyVal, recipient: ActorRef, continuation: Any => B): Unit = {
    send(value, recipient, continuation, Timeout(30 days), _ => {})
  }

  def send[B](value: AnyVal, recipient: ActorRef, continuation: Any => B, timeout: Timeout, recoverContinuation: Any => B): Unit = {
    implicit val timeout: Timeout = Timeout(30 days)
    (recipient ? Send(value,self))
      .mapTo[Boolean]
      .recover {case _: AskTimeoutException => false}
      .foreach {sendok =>
        if (sendok)
          continuation(sendok)
        else
          recoverContinuation(sendok)
      }
  }

  def receiveValue[B](from: ActorRef, continuation: AnyVal => B): Unit = {
    receiveValue(from, continuation, Timeout(30 days), () => {})
  }

  def receiveValue[B](from: ActorRef, continuation: AnyVal => B, timeout: Timeout, recoverContinuation: () => B): Unit = {
    val option = Option(getBuffer(from).poll(timeout.duration.toMillis, TimeUnit.MILLISECONDS))
    option match {
      case Some(value) => continuation(value)
      case None => recoverContinuation()
    }
  }

  def conclude(): Unit = executePromise success 0
}

/*
 * Alice
 */
object Alice {
  def props(name:String, configuration: Configuration): Props = Props(new Alice(name, configuration))
}

class Alice(name:String, configuration: Configuration) extends Participant(name, configuration) {

  lazy val Bob: ActorRef = participantByName("Bob").get

  val tx1 = "tx:00002"
  val tx2 = "tx:00004"

  override def execute(): Unit = put_tx1()

  private def put_tx1(): Unit = put(tx1, put_tx1_callback, 2 seconds, put_tx1_callback_ERR )
  private def put_tx2(): Unit = put(tx2, put_tx2_callback)
  private def send_42_to_Bob(): Unit = send(42, Bob, send_42_callback_OK)

  def put_tx1_callback: Any => Any = {
    putResponse =>
      log.info(s"Put completed: $putResponse")
      put_tx2()
  }

  def put_tx1_callback_ERR: Any => Any = {
    putResponse =>
      log.info(s"Put not completed yet: $putResponse")
      send_42_to_Bob()
  }

  def put_tx2_callback: Any => Any = {
    putResponse =>
      log.info(s"Put completed: $putResponse")
      send_42_to_Bob()
  }

  def send_42_callback_OK: Any => Any = _ => {
    log.info(s"Send completed")
    conclude()
  }
}

/*
 * Bob
 */
object Bob {
  def props(name:String, configuration: Configuration): Props = Props(new Bob(name, configuration))
}

class Bob(name:String, configuration: Configuration) extends Participant(name, configuration) {

  import Blockchain._

  val tx1 = "tx:00001"
  val tx2 = "tx:00003"

  implicit val timeout: Timeout = Timeout(3 seconds)

  lazy val Alice: ActorRef = participantByName("Alice").get

  override def execute(): Unit = Future {
    blockchain ! Put(tx1)
    val txid = blockchain ? Put(tx2)

    log.info(s"waiting for $tx2 to be mined")
    txid.mapTo[PutResponse].foreach {
      case PutResponse(txid) =>
        askTx(txid, (x:Any) => {
            log.info(s"$tx2 is mined. Now receive")
            receiveValue(Alice, {
              v =>
                log.info("read value {}", v)
                conclude()
            })
        })
    }
  }
}
