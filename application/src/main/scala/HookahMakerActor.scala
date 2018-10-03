import akka.actor.{Actor, Props}
import com.bot4s.telegram.models.Message

class HookahMakerActor(msg: Message) extends Actor {
   var isLogined = false
  def receive: Receive = {
    case _ => Unit
  }
}

case object Logined

object HookahMakerActor {
  def props(msg: Message): Props = Props(new HookahMakerActor(msg))
}
