package org.balzaclang

import com.sulacosoft.bitcoindconnector4j.BitcoindApi
import fr.acinq.bitcoin.{Addr, BinaryData, Transaction}

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

abstract class Process {
  def ~ (continuation: Process) = ???
}

object Process {
  sealed case class IfThenElse(e: Expression[_], _then: Process, _else: Process) extends Process {}
  sealed case class Put(tx:Expression[_], next: Process) extends Process
  sealed case class Ask(txid:Expression[_], next: Process) extends Process
  sealed case class Send(p:Participant, v:Expression[_], next: Process) extends Process
  sealed case class Receive(p:Participant, name:String, clz:Class[_], next: Process) extends Process
  case object Nil extends Process
}

object Primitive {

  def put(tx:Transaction)(implicit api: BitcoindApi): Future[String] = Future {
    val txHex = Transaction.write(tx).toString()
    api.sendrawtransaction(txHex)
  }

  def get(txid:BinaryData)(implicit api: BitcoindApi): Future[Transaction] = Future {
    val txhex = api.getrawtransaction(txid.toString())
    Transaction.read(txhex)
  }

  def balance()(implicit api: BitcoindApi): Future[Double] = Future {
    api.getbalance()
  }

  def generate(n: Int)(implicit api: BitcoindApi): Future[List[String]] = Future {
    val txids: List[String] = api.generate(n).asScala.toList
    txids
  }

  def generateToAddress(n: Int, address: Addr)(implicit api: BitcoindApi): Future[List[String]] = Future {
    val txids: List[String] = api.generatetoaddress(n,"").asScala.toList
    txids
  }
}