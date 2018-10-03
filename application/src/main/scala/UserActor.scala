import java.time.{Instant, LocalDateTime}
import java.util.TimeZone

import akka.actor.{Actor, ActorRef, Props}
import DatabaseActor._
import model._
import com.bot4s.telegram.models.Message

class UserActor(id: Int, dbActor: ActorRef) extends Actor {
  var isFree = true
  var hookahId = 0L
  var hookahTaste: Option[String] = None
  var hookahPower: Option[String] = None
  var when: Option[String] = None
  var optComment: Option[String] = None

  def finishOrdering() = {
    isFree = true
  }

  def receive() = {
    case StartOrdering(msg, id) =>
      if (isFree) {
        isFree = false
        hookahId = id
        //          Some(Order(
        //            msg.from.map(_.id).getOrElse(0).toLong,
        //            hookahId,
        //            None,
        //            None,
        //            LocalDateTime.ofInstant(Instant.ofEpochSecond(msg.date), TimeZone.getDefault.toZoneId)))
        context.parent ! AcceptOrdering(msg)
      }
      else
        context.parent ! DenyOrdering(msg, "предыдущий заказ не ещё не закончен. " +
          "Сделайте предыдущий заказ или отмените его с помощью /cancel")
    case UpdateTaste(newTaste) =>
      hookahTaste = newTaste
    case UpdatePower(newPower) =>
      hookahPower = newPower
    case UpdateWhen(newWhen) =>
      when = newWhen
    case UpdateComment(newComment) =>
      optComment = newComment
    case FinishOrdering(msg) =>
      if(hookahTaste.isEmpty || hookahPower.isEmpty || when.isEmpty)
        context.parent ! DenyOrdering(msg, "вы не указали какой-то из пунктов. " +
          "Пересмотрите свой заказ, пожалуйста.")
      else {
        val orderTime = LocalDateTime.ofInstant(Instant.ofEpochSecond(msg.date), TimeZone.getDefault.toZoneId)
          .plusMinutes(when.getOrElse("").toLong)
        val order = Order(id, hookahId, hookahTaste, hookahPower, orderTime, comment = optComment)
        context.actorSelection("/user/hookah-bot-actor/manager-actor") ! DirectOrder(order)
      }
    case CancelOrdering(msg) =>
      isFree = true
      hookahId = 0L
      hookahTaste = None
      hookahPower = None
      when = None
    case _ =>
      Unit
  }
}

case class StartOrdering(msg: Message, hookahId: Long)

case class UpdateTaste(taste: Option[String])

case class UpdatePower(power: Option[String])

case class UpdateWhen(when: Option[String])

case class UpdateComment(comment: Option[String])

case class FinishOrdering(msg: Message)

case class CancelOrdering(msg: Message)

case class OrderCreated(id: Long)


object UserActor {
  def props(id: Int, dbActor: ActorRef) = Props(new UserActor(id, dbActor))
}