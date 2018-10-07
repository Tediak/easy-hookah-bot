import akka.actor.{Actor, Props}
import com.bot4s.telegram.models.{Message, User}
import model.{Guest, Order}

class EmployeeManagerActor() extends Actor {

//  val dbActor =  context.actorSelection("/user/hookah-bot-actor/tediak-database-actor")
  val emplDbActor = context.actorOf(EmployeeDatabaseActor.props, "employee-database-actor")

//  def getHookahMaker(chatId: Long) =
//    context.child(chatId.toString).getOrElse{
//      context.actorOf(HookahMakerActor.props(chatId))
//    }

  def receive: Receive = {
    case Login(msg, password) =>
      emplDbActor ! CheckLogin(msg, password)
    case SuccessfulLogin(chatId) =>
      context.parent ! IsLogined(chatId)
    case FailedLogin(chatId, because) =>
      context.parent ! CantLogin(chatId, because)
    case Logout(chatId)  =>
      emplDbActor ! CheckLogout(chatId)
    case SuccessfulLogout(chatId) =>
      context.parent ! IsLogout(chatId)
    case FailedLogout(chatId, because) =>
      context.parent ! NotLogout(chatId, because)
    case DirectOrder(order, guest) =>
      emplDbActor ! GetEmployeeSet(order, guest)
    case EmployeeSet(order, set, guest) =>
      if (set.isEmpty)
        context.parent ! DenyOrdering(guest.id, " сейчас нету свободных кальянщиков. Попробуйте заказать кальян позже")
      else context.parent ! SendOrderToEmployees(order, set, guest)
    case _ => Unit
  }
}

//case class OrderReceived()

case class Login(msg: Message, password: String)

case class SuccessfulLogin(chatId: Long)

case class FailedLogin(chatId: Long, because: String)

case class Logout(chatId: Long)

case class FailedLogout(chatId: Long, because: String)

case class SuccessfulLogout(chatId: Long)

case class DirectOrder(order: Order, from: Guest)

case class EmployeeSet(order: Order, set: Set[Long], guest: Guest)

case class EmptyEmployeeSet(order: Order)

object EmployeeManagerActor {
  def props(): Props = Props(new EmployeeManagerActor())
}
