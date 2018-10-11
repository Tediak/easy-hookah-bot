import akka.actor.{Actor, Props}
import com.bot4s.telegram.models.Message
import datatables.{AccountRepository, AccountTable, HookahRepository, HookahTable, OrderRepository, OrderTable}
import model.{Account, Guest, Order}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext
import scala.util.Success

object EmployeeDatabaseActor {
  def props = Props(new EmployeeDatabaseActor(Database.forConfig("postgres")))
}

class EmployeeDatabaseActor(db: Database) extends Actor {

  implicit val ec: ExecutionContext = ExecutionContext.global

  val bot = context.actorSelection("/user/hookah-bot-actor")
  val promocodeDbActor = context.actorSelection ("/user/hookah-bot-actor/promocode-database-actor")

  val hookahRepository = new HookahRepository(db)
  val accountRepository = new AccountRepository(db)
  val orderRepository = new OrderRepository(db)


  val hookahTable = HookahTable.table
  val accountTable = AccountTable.table
  val orderTable = OrderTable.table

  def receive: Receive = {
    case CheckLogin(msg, password) =>
      val send = sender()
      hookahRepository.findPassword(password) onComplete {
        case Success(set) =>
          if (set.isEmpty) send ! FailedLogin(msg.source, " пароль неверный")
          else {
            accountRepository.getById(msg.source) onComplete {
              case Success(account) =>
                if (account.isEmpty) {
                  val hookah = set.head
                  val acc = msg.from
                  accountRepository
                    .create(Account(hookah.id, acc.map(_.firstName).getOrElse(""),
                      acc.flatMap(_.username), id = msg.source))
                  send ! SuccessfulLogin(msg.source)
                }
                else send ! FailedLogin(msg.source, " вы уже авторизованы")
            }
          }
      }
    case CheckLogout(chatId) =>
      val send = sender()
      db.run((for {
        acc <- accountTable if acc.id === chatId
      } yield acc).result.headOption) onComplete {
        case Success(account) =>
          if (account.isEmpty) send ! FailedLogout(chatId, " сейчас вы не авторизованы")
          else {
            accountRepository.delete(chatId)
            send ! SuccessfulLogout(chatId)
          }
      }
    case GetPromocode(chatId) =>
      accountRepository.getById(chatId) onComplete {
        case Success(acc) =>
          if (acc.isEmpty) bot ! DenyPromocode(chatId)
          else {
            val hookahId = acc.map(_.hookahId).getOrElse(0L)
            db.run((for {
              hookah <- HookahTable.table if hookah.id === hookahId
            } yield (hookah.code, hookah.id)).result.headOption) onComplete {
              case Success(code) => {
                bot ! AcceptPromocode(chatId, code.map(_._1).getOrElse(""))
//                promocodeDbActor ! ChangePromocode (code.map(_._2).getOrElse(0))
              }
            }
          }
        case _ => Unit
      }
    case GetEmployeeSet(order, guest) =>
      accountRepository.getAllEmployees(order.hookahId) onComplete {
        case Success(set) =>
          context.parent ! EmployeeSet(order, set, guest)
      }
    case AcceptOrder(accountId, orderId) =>
      db.run((for{
        acc <- accountTable if acc.id === accountId
        order <- orderTable if order.id === orderId && !order.isAccepted
      } yield (acc, order)).result).map(_.headOption) onComplete {
        case Success(value) =>
          value match {
            case Some(v) =>
              orderRepository.update(Order(v._2.guestId, v._2.hookahId, v._2.hookahTaste,
                v._2.hookahPower, v._2.time, v._2.comment, true, v._2.id))
              bot ! OrderWasAccepted(v._2.guestId, v._2)
            case None =>
              bot ! OrderAlreadyAccepted(accountId, orderId)
          }
      }
  }
}

case class CheckLogin(msg: Message, password: String)

case class CheckLogout(chatId: Long)

case class GetPromocode(chatId: Long)

case class GetEmployeeSet(order: Order, guest: Guest)

case class AcceptOrder(accountId: Long, orderId: Long)