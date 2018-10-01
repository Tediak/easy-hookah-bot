import akka.actor.{Actor, Props}
import com.bot4s.telegram.models.Message
import datatables.{OrderTable, VisitTable, _}
import model._
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}

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
    }yield (hookah.id, hookah.name)).result).map(_.toSet)


  override def receive: Receive = {
    case CheckHookahs(id, msg) =>
      val hookahSet = getHookahsforUser(id.toLong)
      val send = sender()
      hookahSet onSuccess {
        case set =>
        if(set.isEmpty)
          send ! EmptyHookahSet(msg)
        else send ! HookahSet(set, msg)
      }
    case CreateOrder(msg) =>
    case _ => Unit
  }
}


case class CreateOrder(msg: Message)

case class CheckHookahs(id: Int, msg: Message)

case class SetTaste(taste: String)
case class SetPower(power: String)

object DatabaseActor {
  def props = Props(new DatabaseActor(Database.forConfig("postgres")))
}
