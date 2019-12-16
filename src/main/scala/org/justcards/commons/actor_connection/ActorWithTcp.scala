package org.justcards.commons.actor_connection

import akka.actor.{ActorLogging, ActorRef}
import akka.io.Tcp.{Received, Write}
import akka.serialization.{Serialization, SerializationExtension, Serializers}
import akka.util.ByteString
import org.justcards.commons._
import play.api.libs.json.{Json, OFormat}

/**
 * ActorWithConnection that works with Tcp.
 */
trait ActorWithTcp extends ActorWithConnection with ActorLogging {

  import org.justcards.commons.actor_connection.ActorWithTcp._
  protected implicit val serializer: Serialization = SerializationExtension(context.system)

  override def parse: Receive = {
    case Received(data) =>
      val deserializedMessage = extractMessage(data)
      val msg = if(deserializedMessage isDefined) Outer(deserializedMessage.get) else data
      log.debug("Received from outside message " + msg)
      self ! msg
  }

  override private[actor_connection] def tellWithConnection(actor: ActorRef, message: AppMessage): Unit = {
    val data = serializeMessage(message)
    actor ! Write(TcpMessage(data._1, data._2, data._3))
  }
}

object ActorWithTcp {

  private def serializeMessage(message: AppMessage)(implicit serializator: Serialization): (Array[Byte], Int, String) = {
    val bytes: Array[Byte] = serializator.serialize(message).get
    val serializerId: Int = serializator.findSerializerFor(message).identifier
    val manifest: String = Serializers.manifestFor(serializator.findSerializerFor(message), message)
    (bytes, serializerId, manifest)
  }

  private def deserializeMessage(bytes: Array[Byte], id: Int, manifest: String)
                                (implicit serializator: Serialization): Option[AppMessage] = {
    val data = serializator.deserialize(bytes, id, manifest)
    if(data isSuccess) Some(data.get.asInstanceOf[AppMessage]) else None
  }

  private case class TcpMessage(bytes: Array[Byte], id: Int, manifest: String)
  private[this] implicit val tcpFormat: OFormat[TcpMessage] = Json.format[TcpMessage]

  implicit def fromStringToByteString(msg: String): ByteString = ByteString(msg)
  implicit def fromByteStringToString(msg: ByteString): String = msg.utf8String
  implicit def fromAppMessageToString(msg: TcpMessage): ByteString = fromStringToByteString(Json.toJson(msg).toString())


  /*
   * Extracting the message from a string that HAS TO BE a json object
   * or it will raise an exception because it can't do the conversion.
   */
  def extractMessage(originalMessage: String)(implicit serializator: Serialization): Option[AppMessage] = {
    val elemParsed = Json.parse(originalMessage)
    val elem = Json.fromJson[TcpMessage](elemParsed)
    if(elem.isSuccess) deserializeMessage(elem.get.bytes, elem.get.id, elem.get.manifest)
    else None
  }

}
