import akka.actor.{Actor, Props, Status}
import com.bot4s.telegram.models.Message
import datatables.{OrderTable, VisitTable, _}
import model._
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.util.{Failure, Success}

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

//  def getHookahMakers(hookahId: Long)(implicit ec: ExecutionContext) =
//    db.run((for {
//      acc <- accountTable if (acc.hookahId === hookahId && acc.isLogined)
//    } yield acc.id).result).map(_.toSet)

  def findPassword(pass: String)(implicit ec: ExecutionContext) =
    db.run((for {
      hookah <- hookahTable if hookah.password === pass
    } yield hookah).result).map(_.toSet)

//  def authorizeEmployee(login: String, password: String)(implicit ec: ExecutionContext) = {
//    db.run((for {
//      account <- accountTable
//      if account.login === login
//      if account.password === password
//
//    } yield account.password).result).map(_.toList)
//  }
//
//  def accountIsAlreadyAuthorized(login: String)(implicit ec: ExecutionContext) = {
//    db.run((for {
//      account <- accountTable
//      if account.login === login
//    } yield account.isLogined).result).map(_.toList.head)
//  }

  override def receive: Receive = {
    case CheckHookahs(id, msg) => {
      val hookahSet = getHookahsforUser(id.toLong)
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
//    case VerifyPassword(username, password) =>
//      val send = sender()
//      accountIsAlreadyAuthorized(username) onComplete {
//        case Success(value) =>
//          if (!value)
//            authorizeEmployee(username, password.text.getOrElse(" ")) onComplete {
//              case Success(list) =>
//                if (list.nonEmpty)
//                  accountRepository.updateByUser(username, true) onComplete {
//                    case Success(_) => send ! IsEmployeeAuthorized(password, list)
//                    case Failure(_) => send ! IsEmployeeAuthorized(password, Nil)
//                  }
//                else
//                  send ! IsEmployeeAuthorized(password, Nil)
//            }
//          else
//            send ! EmployeeIsAlreadyAuthorized(password)
//
//        case _ => send ! IsEmployeeAuthorized(password, Nil)
//      }
//    case Logout(username: String, msg) =>
//      val send = sender()
//      accountIsAlreadyAuthorized(username) onComplete {
//        case Success(value) =>
//          if (value)
//            accountRepository.updateByUser(username, false) onComplete {
//              case Success(_) => send ! BotLogout(msg, true)
//              case Failure(_) => send ! BotLogout(msg, false)
//            }
//          else
//            send ! EmployeeIsNotAuthorizedYet(msg)
//      }
//    case IsAlreadyAuthorized(username, msg) =>
//      val send = sender()
//      accountIsAlreadyAuthorized(username) onComplete {
//        case Success(value) =>
//          if (!value)
//            send ! EmployeeForceAuthorize(msg)
//          else
//            send ! EmployeeIsAlreadyAuthorized(msg)
//        case _ => Unit
//      }
    case CheckLogin(msg, password) =>
      val send = sender()
      val accSet = findPassword(password) onComplete {
        case Success(set) =>
          if (set.isEmpty) send ! FailedLogin(msg.source, " пароль неверный")
          else {
            accountRepository.getById(msg.source) onComplete {
              case Success(account) =>
                if (account.isEmpty) {
                  val hookah = set.head
                  val acc = msg.from
                  accountRepository
                    .create(Account(hookah.id, acc.map(_.firstName).getOrElse(""), acc.map(_.username).getOrElse(None), id=msg.source))
                  send ! SuccessfulLogin(msg.source)
                }
                else send ! FailedLogin(msg.source, " вы уже авторизованы")
            }
          }
      }
    case CheckLogout(chatId) =>
      val send = sender()
      db.run((for{
        acc <- accountTable if acc.id === chatId
      } yield acc).result.headOption) onComplete {
        case Success(account) =>
          if (account.isEmpty) send ! FailedLogout(chatId, " сейчас вы не авторизованы")
          else send ! SuccessfulLogout(chatId)
      }
  }
}

case class CreateOrder(msg: Message)

//case class IsAlreadyAuthorized(username: String, msg: Message)

case class CheckHookahs(id: Int, msg: Message)

//case class VerifyPassword(username: String, password: Message)

case class SetTaste(taste: String)

case class SetPower(power: String)

case class FindHookahMaker(hookahId: Long)

case class CheckLogin(msg: Message, password: String)

case class CheckLogout(chatId: Long)

object DatabaseActor {
  def props = Props(new DatabaseActor(Database.forConfig("postgres")))
}


