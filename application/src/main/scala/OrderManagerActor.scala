import akka.actor.{Actor, Props}
import com.bot4s.telegram.models.Message
import model.Order

case class DirectOrder(order: Order)

class OrderManagerActor() extends Actor {

  val dbActor =  context.actorSelection("/user/hookah-bot-actor/tediak-database-actor")

  def getHookahMaker(chatId: Long) =
    context.child(chatId.toString).getOrElse{
      context.actorOf(HookahMakerActor.props(chatId))
    }

  def receive: Receive = {
    case Login(msg, password) =>
      dbActor ! CheckLogin(msg, password)
    case SuccessfulLogin(chatId) =>
      getHookahMaker(chatId) ! StartWorking
      context.parent ! IsLogined(chatId)
    case FailedLogin(chatId, because) =>
      context.parent ! CantLogin(chatId, because)
    case Logout(chatId)  =>
      dbActor ! CheckLogout(chatId)
    case SuccessfulLogout(chatId) =>
      context.parent ! IsLogout(chatId)
    case FailedLogout(chatId, because) =>
      context.parent ! NotLogout(chatId, because)
    case DirectOrder(order) =>
    case _ => Unit
  }
}

case class OrderReceived()

case class Login(msg: Message, password: String)

case class SuccessfulLogin(chatId: Long)

case class FailedLogin(chatId: Long, because: String)

case class Logout(chatId: Long)

case class FailedLogout(chatId: Long, because: String)

case class SuccessfulLogout(chatId: Long)

object OrderManagerActor {
  def props(): Props = Props(new OrderManagerActor())
}
