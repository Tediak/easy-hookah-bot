import akka.actor.{Actor, Props}
import com.bot4s.telegram.models.Message

class OrderManagerActor() extends Actor {
  def receive: Receive = {
    case _ => Unit
  }
}

object OrderManagerActor {
  def props(): Props = Props(new OrderManagerActor())
}
