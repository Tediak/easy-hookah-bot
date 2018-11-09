import java.time.{Instant, LocalDateTime}
import java.util.TimeZone

import HookahBotActor.SendOrderToEmployees
import akka.actor.{Actor, ActorSelection, Props}
import datatables._
import slick.jdbc.PostgresProfile.api._
import model._

import scala.concurrent.{ExecutionContext, Future}
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}
import scala.concurrent.duration._
import UserActor._


class UserActor(user: Guest) extends Actor {

  val db = Database.forConfig("postgres")

  val hookahRepository = new HookahRepository(db)
  val accountRepository = new AccountRepository(db)
  val orderRepository = new OrderRepository(db)
  val visitRepository = new VisitRepository(db)
  val guestRepository = new GuestRepository(db)

  implicit val ec: ExecutionContext = ExecutionContext.global

  var emptyOrder = Order(user.id, 0, None, None, LocalDateTime.MIN, None, NOT_ACCEPTED)

  var menu: ListBuffer[Order] = ListBuffer()
  var current: Order = emptyOrder

  var when: Option[String] = None

//  def epochToLocalDateTimeConverter(epoch: Int): LocalDateTime =
//    LocalDateTime.ofInstant(Instant.ofEpochSecond(epoch), TimeZone.getDefault.toZoneId).plusHours(3)

  def receive: Receive = {
    case StartOrdering(id) =>
      current = current.copy(hookahId = id)
      emptyOrder = emptyOrder.copy(hookahId = id)
    case UpdateTaste(newTaste) =>
      current = current.copy(hookahTaste = newTaste)
    case UpdatePower(newPower) =>
      current = current.copy(hookahPower = newPower)
    case UpdateWhen(newWhen) =>
      when = newWhen
    case UpdateComment(newComment) =>
      current = current.copy(comment = newComment)
    case OneMoreOrder =>
      val orderToAdd = current.copy()
      menu += orderToAdd
      current = emptyOrder.copy()
    case FinishOrdering(time: Int) =>
      if (when.nonEmpty) {
        val orderTime = time.toLocalDateTime.plusMinutes(when.getOrElse("0").toLong)
        menu += current.copy()
        val list = menu.result.map(_.copy(time = orderTime))
        val orderSet: List[Future[Order]] = list map { orderRepository.create }
        Future.sequence(orderSet) onComplete {
          case Success(orders) =>
            context.parent ! SendOrderToEmployees(orders)
        }
      }
//        orderRepository.create(order)
    case _ =>
      Unit
  }

  override def postStop(): Unit = println("actor " + user.id + " was killed")

  override def preStart(): Unit = println("actor " + user.id + " was started")
}

object UserActor {
  def props(guest: Guest) = Props(new UserActor(guest))

  case class StartOrdering(hookahId: Long)

  case class UpdateTaste(taste: Option[String])

  case class UpdatePower(power: Option[String])

  case class UpdateWhen(when: Option[String])

  case class UpdateComment(comment: Option[String])

  case object OneMoreOrder

  case class FinishOrdering(time: Int)
}