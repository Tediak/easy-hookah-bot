import java.time.{Instant, LocalDateTime}
import java.util.TimeZone

import akka.actor.{Actor, Props}
import com.bot4s.telegram.models.{Message, User}
import datatables._
import model._
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Random}

class PromocodeDatabaseActor(db: Database) extends Actor {
  implicit val ec: ExecutionContext = ExecutionContext.global

  def createPromocode (predCode : String) : String = {

    val newCode = predCode.take(2) ++ scala.util.Random.nextInt.abs.toString.take(3)
    if (predCode == newCode)
      createPromocode (predCode)
    else newCode
  }

  val guestRepository = new GuestRepository(db)
  val hookahRepository = new HookahRepository(db)
  val visitRepository = new VisitRepository(db)

  val hookahTable = HookahTable.table

  def epochToLocalDateTimeConverter(epoch: Int): LocalDateTime =
    LocalDateTime.ofInstant(Instant.ofEpochSecond(epoch), TimeZone.getDefault.toZoneId)

  def generateRandomCode(predCode: String): String = {
    val newCode = predCode.take(2) + Random.nextInt(10).toString + Random.nextInt(10).toString + Random.nextInt(10).toString
    if (newCode == predCode)
      generateRandomCode(predCode)
    else newCode
  }


  def receive: Receive = {
    case CheckPromocode(chatId, user, code) =>
      db.run((for {
        hookah <- hookahTable if hookah.code === code
      } yield hookah).result.headOption) onComplete {
        case Success(hookah) =>
          hookah.foreach { h =>
            user foreach { u =>
              guestRepository.create(Guest(u.username, u.firstName, u.lastName, chatId))
            }
            context.parent ! RightPromocode(chatId, h.id)
            hookahRepository.update(Hookah(h.name, generateRandomCode(h.code), h.password, h.freeHookahNumber, h.id))
          }
          if (hookah.isEmpty) context.parent ! WrongPromocode(chatId)
      }
    case NewVisit(guestId, hookahId, time, stars) =>
      visitRepository.create(Visit(guestId, hookahId, epochToLocalDateTimeConverter(time), stars))
    case ChangePromocode (hookahId) =>
      db.run ((for {
        hookah <- hookahTable if hookah.id === hookahId
      } yield hookah).result.headOption) onComplete {
        case Success (hookah) =>
          db.run(hookahTable.filter(_.id === hookahId).map(_.code).result.head)
            .onComplete {
              case Success(code) =>
                db.run(hookahTable.filter(_.id === hookahId).map(_.code)
                  .update(createPromocode(code)))
            }
      }
    case GetStats(userId) =>
      visitRepository.getUserStats(userId).onComplete {
        case Success(set) =>
          context.parent ! UserStats(userId, set)
      }
    case _ => Unit
  }
}

object PromocodeDatabaseActor {
  def props = Props(new PromocodeDatabaseActor(Database.forConfig("postgres")))
}

case class CheckPromocode(chatId: Long, user: Option[User], promocode: String)

case class NewVisit(guestId: Long, hookahId: Long, time: Int, stars: Int)

case class ChangePromocode (hookahId : Long)

case class GetStats(userId: Long)