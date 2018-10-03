import akka.actor.{Actor, Props}
import com.bot4s.telegram.models.Message

class OrderManagerActor() extends Actor {

//  def findHookahMaker(hookahId: Long) = {
//    dbActor ! FindHookahMaker(hookahId)
//  }

  def receive: Receive = {
//    case ReceiveOrder => Unit
//    case OrderReceived =>
//      context.parent ! SendOrderMessage(orderId)
    case _ => Unit
  }
}

case class ReceiveOrder()

case class OrderReceived()

object OrderManagerActor {
  def props(): Props = Props(new OrderManagerActor())
}
