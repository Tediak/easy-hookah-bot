import akka.actor.{Actor, ActorRef, Props}
import DatabaseActor._
import com.bot4s.telegram.models.Message

class UserActor(id: Int, dbActor: ActorRef) extends Actor {

  var orderId: Option[Long] = None

  def finishOrdering() = orderId = None

  def receive() = {
    case StartOrdering(msg) =>
      orderId match {
        case None =>
          dbActor ! CreateOrder
        case _ =>
          sender() ! DenyOrdering(msg, "предыдущий заказ не ещё не закончен. " +
            "Сделайте предыдущий заказ или отмените его с помощью /cancel")
      }
    case OrderCreated(id) =>
      orderId = Some(id)

    case _ =>
      Unit
  }
}

case class StartOrdering(msg: Message)

case class OrderCreated(id: Long)

object UserActor {
  def props(id: Int, dbActor: ActorRef) = Props(new UserActor(id, dbActor))
}