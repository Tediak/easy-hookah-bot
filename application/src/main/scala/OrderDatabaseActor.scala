import akka.actor.{Actor, Props, Status}
import com.bot4s.telegram.models.Message
import datatables._
import model._
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.util.{Failure, Success}

class OrderDatabaseActor(db: Database) extends Actor {

  implicit val ec: ExecutionContext = ExecutionContext.global

//  val accountRepository = new AccountRepository(db)
//  val guestRepository = new GuestRepository(db)
  val hookahRepository = new HookahRepository(db)
//  val orderRepository = new OrderRepository(db)
//  val visitRepository = new VisitRepository(db)

  def receive: Receive = {
    case CheckHookahs(id, msg) => {
      val hookahSet = hookahRepository.getHookahsforUser(id.toLong)
      val send = sender()
      hookahSet onComplete {
        case Success(set) =>
          if (set.isEmpty)
            send ! EmptyHookahSet(msg)
          else send ! HookahSet(set, msg)

        case Failure(_) =>
          send ! EmptyHookahSet(msg)
      }
    }
    case _ => Unit
  }
}

object OrderDatabaseActor {
  def props = Props(new OrderDatabaseActor(Database.forConfig("postgres")))
}

case class CheckHookahs(id: Int, msg: Message)


