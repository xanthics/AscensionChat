package wowchat.realm

import org.bouncycastle.crypto.modes.ChaCha20Poly1305
import org.bouncycastle.crypto.params.{AEADParameters, KeyParameter}
import org.bouncycastle.math.ec.rfc7748.X25519

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import scala.util.Random

// looks like they use https://github.com/jedisct1/libsodium

object HandshakeAscension {

  private val DERIVED = "derived".getBytes("UTF-8")
  private val SESSION = "session".getBytes("UTF-8")

  private val KEY_CONSTANT_1 = Array(
    0x36, 0x42, 0xAF, 0x85, 0x23, 0x69, 0x15, 0x4C, 0xFA, 0x11, 0x45, 0x95, 0x08, 0x80, 0x10, 0x82,
    0x80, 0xA4, 0x34, 0x1C, 0x26, 0xA3, 0x76, 0x43, 0x1B, 0x74, 0x1E, 0x2A, 0xAE, 0x9C, 0x29, 0x48
  ).map(_.toByte)

  private val KEY_CONSTANT_2 = Array(
    0xB6, 0xEB, 0x62, 0x41, 0xB5, 0x82, 0x9B, 0x68, 0x9F, 0x07, 0xC9, 0x1F, 0x57, 0x31, 0xBA, 0x1B,
    0xBB, 0x67, 0x1C, 0x3F, 0xA2, 0xF7, 0x00, 0x1C, 0x22, 0x49, 0xE3, 0xFA, 0x52, 0xDD, 0x87, 0x02
  ).map(_.toByte)

  private val KEY_CONSTANT_3 = Array(0x4F, 0x4B).map(_.toByte)

  private def generate_key(): Array[Byte] = {
    val result = Array.fill[Byte](32)(0)
    scala.util.Random.nextBytes(result)
    X25519.clampPrivateKey(result)
    result
  }

  private def calculate_key_public(key_private: Array[Byte]): Array[Byte] = {
    val result = Array.fill[Byte](32)(0)
    X25519.scalarMultBase(key_private, 0, result, 0)
    result
  }

  private def calculate_key_shared(key_1: Array[Byte]): Array[Byte] = {
    val result = Array.fill[Byte](32)(0)
    X25519.scalarMult(key_1, 0, KEY_CONSTANT_1, 0, result, 0)
    result
  }
}

class HandshakeAscension {

  private val random_key_secret = HandshakeAscension.generate_key()
  val key_public = HandshakeAscension.calculate_key_public(random_key_secret)
  private val key_shared = HandshakeAscension.calculate_key_shared(random_key_secret)

  val random_nonce = {
    val result = Array.fill[Byte](12)(0)
    Random.nextBytes(result)
    result
  }

  val key_derived = derive_key(HandshakeAscension.KEY_CONSTANT_2, key_shared, HandshakeAscension.DERIVED, 32)
  val key_session = derive_key(HandshakeAscension.KEY_CONSTANT_2, key_shared, HandshakeAscension.SESSION, 40)

  val proof_2 = calculate_proof(key_derived, HandshakeAscension.KEY_CONSTANT_3)

  private def derive_key(key: Array[Byte], input_1: Array[Byte], input_2: Array[Byte], size: Int): Array[Byte] = {
    val algorithm = "HmacSHA256"
    val hasher_1 = Mac.getInstance(algorithm)
    hasher_1.init(new SecretKeySpec(key, algorithm))
    val interim = hasher_1.doFinal(input_1)
    val hasher_2 = Mac.getInstance(algorithm)
    hasher_2.init(new SecretKeySpec(interim, algorithm))
    hasher_2.update(input_2)
    val result = hasher_2.doFinal(Array[Byte](1))
    if (size == 32) {
      return result
    }
    {
      val hasher_2 = Mac.getInstance(algorithm)
      hasher_2.init(new SecretKeySpec(interim, algorithm))
      hasher_2.update(result)
      hasher_2.update(input_2)
      val result_ = hasher_2.doFinal(Array[Byte](2))
      (result ++ result_).take(size)
    }
  }

  private def calculate_proof(key: Array[Byte], input_1: Array[Byte]): Array[Byte] = {
    val algorithm = "HmacSHA256"
    val hasher = Mac.getInstance(algorithm)
    hasher.init(new SecretKeySpec(key, algorithm))
    hasher.doFinal(input_1)
  }

  def encrypt_password(password: Array[Byte]) : (Array[Byte], Array[Byte]) = {
    val encrypter = new ChaCha20Poly1305
    encrypter.init(
      true,
      new AEADParameters(
        new KeyParameter(key_derived),
        16 * 8,
        random_nonce
      )
    )
    val result = Array.fill[Byte](password.length + 16)(0)
    encrypter.processBytes(password, 0, password.length, result, 0)
    encrypter.doFinal(result, 0)
    (result.take(password.length), result.drop(password.length))
  }
}
