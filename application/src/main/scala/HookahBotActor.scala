import akka.actor.{Actor, Props}
import com.bot4s.telegram.api.declarative.{Callbacks, Commands}
import com.bot4s.telegram.api.{Polling, TelegramBot}
import com.bot4s.telegram.clients.ScalajHttpClient
import com.bot4s.telegram.methods.{EditMessageReplyMarkup, EditMessageText, SendMessage}
import com.bot4s.telegram.models._
import slick.jdbc.PostgresProfile.api._
import UserActor._

import scala.util.Try

class HookahBotActor() extends TelegramBot with Polling with Commands
  with Callbacks with Actor {

  val client = new ScalajHttpClient("626944613:AAFOedBtg34Kl7g3NV1a4w7XeixM0AgIfg8")
  val db = Database.forConfig("postgres")
  val dbActor = context.actorOf(DatabaseActor.props, "tediak_database_actor")

  def greetings(name: String) = "Привет, " + name +
    "! Это бот-кальянщик, который упростит тебе твою жизнь :)"

  def start = "Нажмите любую из кнопок, чтобы сделать новый заказ."

  val startMarkup = Some(ReplyKeyboardMarkup.singleRow(
    Seq(
      KeyboardButton.text("Заказать кальян"),
      KeyboardButton.text("Ввести промокод")),
    oneTimeKeyboard = Some(true),
    resizeKeyboard = Some(true)
  ))

  onCommand("/start") {
    implicit msg =>
      reply(greetings(msg.from.map(_.firstName).getOrElse("Неизвестный")),
        replyMarkup = startMarkup)
  }

//  onCallbackWithTag("order"){
//      implicit cbq =>
//        for (msg <- cbq.message) reply("Для начала, выберем вкус"
//        , replyMarkup = Some(InlineKeyboardMarkup.singleColumn(
//            Seq(
//              InlineKeyboardButton.callbackData("Кислый", cbq.data + "t1"),
//              InlineKeyboardButton.callbackData("Сладкий", cbq.data + "t2"),
//              InlineKeyboardButton.callbackData("Кислосладкий :)", cbq.data + "t3")
//            ))))(msg)
//        ackCallback(Some("Ураааа"))
//    }

  onMessage{ implicit msg =>
    using(_.text) {
      case "Заказать кальян" =>
        msg.from.foreach { from =>
          val nickname = from.username.getOrElse("")
          val userActor = context.child(nickname).getOrElse{
            context.actorOf(UserActor.props(nickname, dbActor), nickname)
          }
          userActor ! StartOrdering
        }
      case "Ввести промокод" =>
        reply("Введите промокод, который сказал вам кальянщик", replyMarkup = Some(ForceReply()))
      case _ =>
        "Извините, не понимаю Вас"
    }
    using(_.replyToMessage) {
      rpl =>
        rpl.text match {
          case Some("Отдельные пожелания") => reply("Ваш заказ принят!")
          case Some("Введите промокод, который сказал вам кальянщик") =>
            reply("Спасибо!!!")
          case _ => reply("Извините, не понимаю вас")
        }
    }
  }

  override def preStart(): Unit = {
    run()
  }

  override def receive: Receive = {
    case _ => Unit
  }
}

case object DenyOrdering
case object AcceptOrdering

object HookahBotActor {
  def props(): Props = Props(new HookahBotActor())
}