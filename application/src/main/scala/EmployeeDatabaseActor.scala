import akka.actor.{Actor, Props}
import com.bot4s.telegram.models.Message
import datatables.{AccountRepository, AccountTable, HookahRepository, HookahTable}
import model.Account
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext
import scala.util.Success

object EmployeeDatabaseActor {
  def props = Props(new EmployeeDatabaseActor(Database.forConfig("postgres")))
}

class EmployeeDatabaseActor(db: Database) extends Actor {

  implicit val ec: ExecutionContext = ExecutionContext.global

  val hookahRepository = new HookahRepository(db)
  val accountRepository = new AccountRepository(db)


  val hookahTable = HookahTable.table
  val accountTable = AccountTable.table

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
          if (acc.isEmpty) context.parent ! DenyPromocode(chatId)
          else {
            val hookahId = acc.map(_.hookahId).getOrElse(0L)
            db.run((for {
              hookah <- HookahTable.table if hookah.id === hookahId
            } yield hookah.code).result.headOption) onComplete {
              case Success(code) =>
                context.parent ! AcceptPromocode(chatId, code)
            }
          }
        case _ => Unit
      }

  }
}

case class CheckLogin(msg: Message, password: String)

case class CheckLogout(chatId: Long)

case class GetPromocode(chatId: Long)