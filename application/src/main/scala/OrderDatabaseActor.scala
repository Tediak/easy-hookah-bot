import akka.actor.{Actor, Props, Status}
import com.bot4s.telegram.models.Message
import datatables._
import model._
import slick.jdbc.PostgresProfile.api._
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}
import scala.concurrent.duration._

class OrderDatabaseActor(db: Database) extends Actor {

  implicit val ec: ExecutionContext = ExecutionContext.global

  val manager = context.actorSelection("/user/hookah-bot-actor/manager-actor")
  val hookahRepository = new HookahRepository(db)
  val orderRepository = new OrderRepository(db)

  def receive: Receive = {
    case CheckHookahs(userId) => {
      val hookahSet = hookahRepository.getHookahsforUser(userId)
      hookahSet onComplete {
        case Success(set) =>
          if (set.isEmpty)
            context.parent ! EmptyHookahSet(userId)
          else context.parent ! HookahSet(userId, set)
        case Failure(_) =>
          context.parent ! EmptyHookahSet(userId)
      }
    }
    case CreateOrder(from, order) =>
      orderRepository.create(order) onComplete {
        case Success(o) =>
          manager ! DirectOrder(o, from)
          context.system.scheduler.scheduleOnce(10 minutes, self, IsOrderAccepted(o))
      }
    case IsOrderAccepted(order) =>
      orderRepository.getById(order.id) onComplete {
        case Success(value) =>
          value.foreach{ o =>
            if (!o.isAccepted) context.parent ! OrderTimeout(o)
          }
      }
    case _ => Unit
  }
}

object OrderDatabaseActor {
  def props = Props(new OrderDatabaseActor(Database.forConfig("postgres")))
}

case class CheckHookahs(userId: Long)

case class CreateOrder(from: Guest, order: Order)

case class IsOrderAccepted(order: Order)


