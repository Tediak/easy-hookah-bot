import java.time.{Instant, LocalDateTime}
import java.util.TimeZone

import akka.actor.{Actor, ActorSelection, Props}
import model._

class UserActor(user: Guest) extends Actor {
  var isFree = true
  var hookahId = 0L
  var hookahTaste: Option[String] = None
  var hookahPower: Option[String] = None
  var when: Option[String] = None
  var optComment: Option[String] = None

  val emptyOrder = Order(user.id, 0, None, None, LocalDateTime.MIN, None)

  val manager: ActorSelection = context.actorSelection("/user/hookah-bot-actor/manager-actor")
  val orderDbActor = context.actorSelection("/user/hookah-bot-actor/order-database-actor")

  def epochToLocalDateTimeConverter(epoch: Int): LocalDateTime =
    LocalDateTime.ofInstant(Instant.ofEpochSecond(epoch), TimeZone.getDefault.toZoneId).plusHours(3)

  def receive: Receive = {
    case StartOrdering(id) =>
      if (isFree) {
        isFree = false
        hookahId = id
        context.parent ! AcceptOrdering(user.id)
      }
      else
        context.parent ! DenyOrdering(user.id, " предыдущий заказ не ещё не обработан. " +
          "Подождите, пока обработается предыдущий заказ.")
    case UpdateTaste(newTaste) =>
      hookahTaste = newTaste
    case UpdatePower(newPower) =>
      hookahPower = newPower
    case UpdateWhen(newWhen) =>
      when = newWhen
    case UpdateComment(newComment) =>
      optComment = newComment
    case FinishOrdering(time: Int) =>
        val orderTime = epochToLocalDateTimeConverter(time)
          .plusMinutes(when.getOrElse("").toLong)
        val order = Order(user.id, hookahId, hookahTaste, hookahPower, orderTime, comment = optComment)
        orderDbActor ! CreateOrder(user, order)
    case CancelOrdering =>
      if(!isFree) {
        isFree = true
        hookahId = 0L
        hookahTaste = None
        hookahPower = None
        when = None
        context.parent ! OrderCancelled(user.id)
      }
      else context.parent ! OrderNotCancelled(user.id)
    case _ =>
      Unit
  }
}

case class StartOrdering(hookahId: Long)

case class UpdateTaste(taste: Option[String])

case class UpdatePower(power: Option[String])

case class UpdateWhen(when: Option[String])

case class UpdateComment(comment: Option[String])

case class FinishOrdering(time: Int)

case object CancelOrdering

case class OrderCreated(id: Long)

object UserActor {
  def props(guest: Guest) = Props(new UserActor(guest))
}