package org.balzaclang

import java.util.concurrent.{BlockingQueue, SynchronousQueue}

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

object Main extends App {

  // Create the 'helloAkka' actor system
  val system: ActorSystem = ActorSystem("helloAkka")

  implicit val timeout = Timeout(5 seconds)

  // Create the printer actor
  val conf: ActorRef = system.actorOf(Configuration.props, "configurationActor")
  conf ! "start"

  println("Main thread finished")
}


/*
 * System
 */
object Configuration {
  def props: Props = Props[Configuration]
}

class Configuration extends Actor {

  implicit val timeout = Timeout(1 seconds)

  val blockchain: ActorRef = context.actorOf(Blockchain.props, "blockchainActor")
  val participants: Map[String,ActorRef] = Map(
    ("Alice", context.actorOf(Alice.props("Alice",this), "aliceActor")),
    ("Bob", context.actorOf(Bob.props("Bob", this), "bobActor"))
  )

  override def receive: Receive = {
    case "start" =>

      println("starting all participants")

      val startResults: Iterable[(ActorRef, Future[Any])] = participants.map(_._2).map(p => (p, p.?("start")))

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
          l foreach println

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
  def props: Props = Props[Blockchain]
  // permitted messages
  final case class Put(tx: String)
  final case class PutResponse(txid: String)

  final case class Get(txid: String)
  final case class GetResponse(tx: Option[String])
}

class Blockchain extends Actor with ActorLogging {
  import Blockchain._

  var txs : Map[String,String] = Map()

  override def receive: Receive = {
    case Put(tx) =>
      log.info(s"mining transaction $tx")
      val txid = txs.size.toString
      txs = txs.+((txid,tx))
      Thread.sleep(3000)
      log.info(s"($txid,$tx) mined!")
      sender() ! PutResponse(txid)

    case Get(txid) =>
      log.info(s"get transaction $txid")
      sender() ! GetResponse(txs.get(txid))

    case _ => log.info("unknown message received")
  }
}

/*
 * Participant Actor
 */
object Participant {
  final case class Send(value: AnyVal)
}

abstract class Participant(val name: String, configuration: Configuration) extends Actor with ActorLogging {

  import Participant._
  import Blockchain._
  implicit val timeout = Timeout(1 day)

  def execute(): Future[_]

  val buffer: BlockingQueue[AnyVal] = new SynchronousQueue[AnyVal](true)

  val blockchain: ActorRef = configuration.blockchain
  def participantByName(name: String): Option[ActorRef] = configuration.participants.get(name)

  override def receive: Receive = {

    case "start" =>
      log.info("starting "+self)
      sender ! execute

    case Send(value) =>
      log.info("received value {} ", value)
      buffer.put(value)
      log.info("value {} saved ", value)
      sender() ! true

    case msg => log.warning("unknown message {}", msg)
  }

  def put[B](tx: String, continuation: PutResponse => B): Future[B] = {
    (blockchain ? Put(tx)).mapTo[PutResponse] map {
      txid =>
        continuation(txid)
    }
  }

  def send[B](value: AnyVal, recipient: ActorRef, continuation: Boolean => B): Future[B] = {
    (recipient ? Send(value)).mapTo[Boolean] map {
      txid =>
        continuation(txid)
    }
  }
}

/*
 * Alice
 */
object Alice {
  def props(name:String, configuration: Configuration): Props = Props(new Alice(name, configuration))
}

class Alice(name:String, configuration: Configuration) extends Participant(name, configuration) {

  import Blockchain._

  lazy val Bob = participantByName("Bob").get

  override def execute(): Future[_] = {

    log.info("ALICE execute()")

    val future = init

    future onComplete {
      _ => log.info(s"$name COMPLETED (onComplete callback)")
    }
    future
  }

  val tx1 = "tx:00002"
  val tx2 = "tx:00004"

  def init = Future {
    log.info("init()")
  }.map {
    _ => put_tx1
  }

  def put_tx1: Future[Any] = put(tx1, put_tx1_callback)
  def put_tx2: Future[Any] = put(tx2, put_tx2_callback)
  def send_42_to_Bob: Future[Any] = send(42, Bob, send_42_callback)

  def put_tx1_callback: PutResponse => Any = {
    putResponse =>
      log.info(s"Put completed: $putResponse")
      put_tx2
  }

  def put_tx2_callback: PutResponse => Any = {
    putResponse =>
      log.info(s"Put completed: $putResponse")
      send_42_to_Bob
  }

  def send_42_callback: Boolean => Any = {
    sendAck =>
      log.info(s"Send completed: $sendAck")
      end
  }

  def end: Any => _ = {
    _: Any =>
      println(s"$name COMPLETED")
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

  override def execute(): Future[_] = {

    val body = Future {
      log.info("BOB execute() - 1")
//      if (name=="Bob") throw new RuntimeException
    }.andThen {
      case Success(_) =>
        log.info("BOB execute() - 2")

        val future = for {
          txid1 <- blockchain ? Put(tx1)
          _ = log.info(s"Put ok ($txid1,$tx1)")

          txid2 <- (blockchain ? Put(tx2)).mapTo[PutResponse]
          _ = log.info(s"Put ok ($txid2,$tx2)")
        } yield txid2

        future.andThen {
          case Success(_) =>
            log.info("reading value from the buffer")
            val value = buffer.take()
            log.info("read value {}",value)
        }
    }
    body
  }
}
