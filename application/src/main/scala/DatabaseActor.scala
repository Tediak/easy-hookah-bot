import akka.actor.{Actor, Props, Status}
import com.bot4s.telegram.models.Message
import datatables.{OrderTable, VisitTable, _}
import model._
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.util.{Success, Failure}

class DatabaseActor(db: Database) extends Actor {

  implicit val ec: ExecutionContext = ExecutionContext.global

  val accountRepository = new AccountRepository(db)
  val guestRepository = new GuestRepository(db)
  val hookahRepository = new HookahRepository(db)
  val orderRepository = new OrderRepository(db)
  val visitRepository = new VisitRepository(db)

  def orderTable = OrderTable.table

  def visitTable = VisitTable.table

  def guestTable = GuestTable.table

  def hookahTable = HookahTable.table

  def accountTable = AccountTable.table

  def getHookahsforUser(id: Long)(implicit ec: ExecutionContext) =
    db.run((for {
      hookah <- hookahTable
    } yield (hookah.id, hookah.name)).result).map(_.toSet)

  def authorizeEmployee(login: String, password: String)(implicit ec: ExecutionContext) = {
    db.run((for {
      account <- accountTable
      if account.login === login
      if account.password === password

    } yield account.password).result).map(_.toList)
  }

  def accountIsAlreadyAuthorized(login: String)(implicit ec: ExecutionContext) = {
    db.run((for {
      account <- accountTable
      if account.login === login
    } yield account.isLogined).result).map(_.toList.head)
  }

  override def receive: Receive = {
    case CheckHookahs(id, msg) => {
      val hookahSet = getHookahsforUser(id.toLong)
      val send = sender()
      hookahSet onComplete {
        case Success(set) =>
          if (set.isEmpty)
            send ! EmptyHookahSet(msg)
          else send ! HookahSet(set, msg)
          if (set.isEmpty)
            send ! EmptyHookahSet(msg)
          else send ! HookahSet(set, msg)

        case Failure(_) =>
          send ! EmptyHookahSet(msg)
      }
    }
    case VerifyPassword(username, password) =>
      val send = sender()
      accountIsAlreadyAuthorized(username) onComplete {
        case Success(value) =>
          if (!value)
            authorizeEmployee(username, password.text.getOrElse(" ")) onComplete {
              case Success(list) =>
                if (list.nonEmpty)
                  accountRepository.updateByUser(username, true) onComplete {
                    case Success(_) => send ! IsEmployeeAuthorized(password, list)
                    case Failure(_) => send ! IsEmployeeAuthorized(password, Nil)
                  }
                else
                  send ! IsEmployeeAuthorized(password, Nil)
            }
          else
            send ! EmployeeIsAlreadyAuthorized(password)
        case _ => send ! IsEmployeeAuthorized(password, Nil)
      }
    case Logout(username: String, msg) =>
      val send = sender()
      accountIsAlreadyAuthorized(username) onComplete {
        case Success(value) =>
          if (value)
            accountRepository.updateByUser(username, false) onComplete {
              case Success(_) => send ! BotLogout(msg, true)
              case Failure(_) => send ! BotLogout(msg, false)
            }
          else
            send ! EmployeeIsNotAuthorizedYet(msg)
      }

    case IsAlreadyAuthorized(username,msg) =>
      val send = sender()
      accountIsAlreadyAuthorized(username) onComplete {
        case Success(value) =>
          if (!value)
            send ! EmployeeForceAuthorize (msg)
        else
            send ! EmployeeIsAlreadyAuthorized(msg)
        case _ => Unit
      }
  }
}


  case class CreateOrder(msg: Message)

  case class IsAlreadyAuthorized(username: String, msg : Message)

  case class CheckHookahs(id: Int, msg: Message)

  case class VerifyPassword(username: String, password: Message)

  case class Logout(username: String, msg: Message)

  case class SetTaste(taste: String)

  case class SetPower(power: String)


  object DatabaseActor {
    def props = Props(new DatabaseActor(Database.forConfig("postgres")))
  }


