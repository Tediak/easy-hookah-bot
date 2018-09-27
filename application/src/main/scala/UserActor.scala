import akka.actor.{Actor, ActorRef, Props}
import DatabaseActor._

class UserActor(nickname: String, dbActor: ActorRef) extends Actor {

  var orderId: Option[Long] = None

  def finishOrdering() = orderId = None

  def receive() = {
    case StartOrdering =>
      orderId match {
        case None =>
          dbActor ! CreateOrder
        case _ =>
          sender() ! DenyOrdering
      }
    case OrderCreated(id) =>
      orderId = Some(id)
      sender() ! AcceptOrdering
    case _ =>
      Unit
  }
}

case object StartOrdering

case class OrderCreated(id: Long)

object UserActor {
  def props(nickname: String, dbActor: ActorRef) = Props(new UserActor(nickname, dbActor))
}
