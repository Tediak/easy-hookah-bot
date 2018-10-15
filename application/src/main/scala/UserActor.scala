import java.time.{Instant, LocalDateTime}
import java.util.TimeZone

import akka.actor.{Actor, ActorSelection, Props}
import datatables._
import slick.jdbc.PostgresProfile.api._
import model._
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}
import scala.concurrent.duration._


class UserActor(user: Guest) extends Actor {
  var hookahId = 0L
  var hookahTaste: Option[String] = None
  var hookahPower: Option[String] = None
  var when: Option[String] = None
  var optComment: Option[String] = None

  val db = Database.forConfig("postgres")

  val hookahRepository = new HookahRepository(db)
  val accountRepository = new AccountRepository(db)
  val orderRepository = new OrderRepository(db)
  val visitRepository = new VisitRepository(db)
  val guestRepository = new GuestRepository(db)

  implicit val ec: ExecutionContext = ExecutionContext.global

  val emptyOrder = Order(user.id, 0, None, None, LocalDateTime.MIN, None)

  def epochToLocalDateTimeConverter(epoch: Int): LocalDateTime =
    LocalDateTime.ofInstant(Instant.ofEpochSecond(epoch), TimeZone.getDefault.toZoneId).plusHours(3)

  def receive: Receive = {
    case StartOrdering(id) =>
        hookahId = id
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
        orderRepository.create(order).onComplete {
          case Success(o) =>
            context.parent ! SendOrderToEmployees(o)
            context.system.scheduler.scheduleOnce(10 minutes, context.parent, OrderTimeout(o.id))
        }
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

object UserActor {
  def props(guest: Guest) = Props(new UserActor(guest))
}