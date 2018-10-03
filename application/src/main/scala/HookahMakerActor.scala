import akka.actor.{Actor, Props}
import com.bot4s.telegram.models.Message

class HookahMakerActor(id: Long) extends Actor {
   var isLogined = false
  def receive: Receive = {
//    case ReceiveOrder =>
//      dbActor ! AddOrder
//      context.parent ! OrderReceived
    case _ => Unit
  }
}

case class ReceiveOrder()

object HookahMakerActor {
  def props(id: Long): Props = Props(new HookahMakerActor(id))
}
