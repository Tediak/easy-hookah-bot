import akka.actor.ActorSystem
import datatables._
import model._
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main {

  def main(args: Array[String]): Unit = {
//    init()
    val system = ActorSystem("hookah-system")
    system.actorOf(HookahBotActor.props(), "hookah-bot-actor")

  }

  def init(): Unit = {
    val db = Database.forConfig("postgres")
    Await.result(db.run(HookahTable.table.schema.create), Duration.Inf)
    Await.result(db.run(GuestTable.table.schema.create), Duration.Inf)
    Await.result(db.run(AccountTable.table.schema.create), Duration.Inf)
    Await.result(db.run(OrderTable.table.schema.create), Duration.Inf)
    Await.result(db.run(VisitTable.table.schema.create), Duration.Inf)
    val hookahRepository = new HookahRepository(db)

    Await.result(hookahRepository.create(Hookah("Caffetoria", "cf000", "1234")), Duration.Inf)
  }
}