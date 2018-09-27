import akka.actor.{Actor, Props}
import slick.jdbc.PostgresProfile.api._

class DatabaseActor() extends Actor {

  override def receive: Receive = {
    case _ => Unit
  }
}


case object CreateOrder

case class SetTaste(taste: String)
case class SetPower(power: String)

object DatabaseActor {
  def props = Props(new DatabaseActor(Database.forConfig("postgres")))
}
