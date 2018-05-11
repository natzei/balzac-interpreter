import fr.acinq.bitcoin.Crypto.PrivateKey
import fr.acinq.bitcoin._
import org.balzaclang._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

val priv1 = PrivateKey(BinaryData("1e99423a4ed27608a15a2616a2b0e9e52ced330ac530edcc32c8ffc6a526aedd"), compressed = true)
val priv = PrivateKey(BinaryData("1e99423a4ed27608a15a2616a2b0e9e52ced330ac530edcc32c8ffc6a526aedd"), compressed = false)
val pubUncompressed = priv.publicKey
Base58Check.encode(Base58.Prefix.PubkeyAddress, pubUncompressed.hash160)
val pubCompressed = priv.publicKey.copy(compressed = true)
Base58Check.encode(Base58.Prefix.PubkeyAddress, pubCompressed.hash160)
Base58Check.encode(Base58.Prefix.SecretKey, priv.toBin)
Base58Check.encode(Base58.Prefix.SecretKey, priv.copy(compressed = true).toBin)

val pubKey = priv.publicKey

val pkh = priv.publicKey.hash160
val pubKeyScript = OP_DUP :: OP_HASH160 :: OP_PUSHDATA(pkh) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil
val sigScript = OP_PUSHDATA(BinaryData("1234")) :: OP_PUSHDATA(pubKey.toBin) :: Nil

// simple pay to PK tx

// we have a tx that was sent to a public key that we own
val to = "mi1cMMSL9BZwTQZYpweE1nTmwRxScirPp3"
val (Base58.Prefix.PubkeyAddressTestnet, pubkeyHash) = Base58Check.decode(to)
val amount = 10000 satoshi

val privateKey = PrivateKey.fromBase58("cRp4uUnreGMZN8vB7nQFX6XWMHU5Lc73HMAhmcDEwHfbgRS66Cqp", Base58.Prefix.SecretKeyTestnet)
val publicKey = privateKey.publicKey

val previousTx = Transaction.read("0100000001b021a77dcaad3a2da6f1611d2403e1298a902af8567c25d6e65073f6b52ef12d000000006a473044022056156e9f0ad7506621bc1eb963f5133d06d7259e27b13fcb2803f39c7787a81c022056325330585e4be39bcf63af8090a2deff265bc29a3fb9b4bf7a31426d9798150121022dfb538041f111bb16402aa83bd6a3771fa8aa0e5e9b0b549674857fafaf4fe0ffffffff0210270000000000001976a91415c23e7f4f919e9ff554ec585cb2a67df952397488ac3c9d1000000000001976a9148982824e057ccc8d4591982df71aa9220236a63888ac00000000")

// create a transaction where the sig script is the pubkey script of the tx we want to redeem
// the pubkey script is just a wrapper around the pub key hash
// what it means is that we will sign a block of data that contains txid + from + to + amount

// step  #1: creation a new transaction that reuses the previous transaction's output pubkey script
val tx1 = Transaction(
  version = 1L,
  txIn = List(
    TxIn(OutPoint(previousTx, 0), signatureScript = Nil, sequence = 0xFFFFFFFFL)
  ),
  txOut = List(
    TxOut(amount = amount, publicKeyScript = OP_DUP :: OP_HASH160 :: OP_PUSHDATA(pubkeyHash) :: OP_EQUALVERIFY :: OP_CHECKSIG :: Nil)
  ),
  lockTime = 0L
)

// step #2: sign the tx
val sig = Transaction.signInput(tx1, 0, previousTx.txOut(0).publicKeyScript, SIGHASH_ALL, privateKey)
val tx2 = tx1.updateSigScript(0, OP_PUSHDATA(sig) :: OP_PUSHDATA(publicKey) :: Nil)

// redeem the tx
Transaction.correctlySpends(tx2, Seq(previousTx), ScriptFlags.MANDATORY_SCRIPT_VERIFY_FLAGS)

val f = Future {
  2 / 0
}

f onComplete {
  case Success(v) => println("value "+v)
  case Failure(e) => println("I'd be amazed if this printed out.")
}

f onFailure {
  case npe: NullPointerException =>
    println("I'd be amazed if this printed out.")
}

(f failed) foreach {
  case e => println("exception "+e.getMessage)
}

