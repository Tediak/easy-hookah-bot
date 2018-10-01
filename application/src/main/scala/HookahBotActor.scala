import akka.actor.{Actor, Props}
import com.bot4s.telegram.methods.DeleteMessage

import scala.util.Try
//import info.mukel.telegrambot4s.api.declarative.{Callbacks, Commands}
//import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
//import info.mukel.telegrambot4s.models.{ForceReply, KeyboardButton, ReplyKeyboardMarkup}
import slick.jdbc.PostgresProfile.api._
import scala.concurrent.Future
//import UserActor._
//import DatabaseActor._

import com.bot4s.telegram.api.declarative.{Callbacks, Commands}
import com.bot4s.telegram.api.{Polling, TelegramBot}
import com.bot4s.telegram.clients.ScalajHttpClient
import com.bot4s.telegram.methods.{EditMessageReplyMarkup, EditMessageText, SendMessage}
import com.bot4s.telegram.models._

class HookahBotActor() extends TelegramBot with Polling with Commands
  with Callbacks with Actor {

  val client = new ScalajHttpClient("626944613:AAFOedBtg34Kl7g3NV1a4w7XeixM0AgIfg8")
  val db = Database.forConfig("postgres")
  val dbActor = context.actorOf(DatabaseActor.props, "tediak_database_actor")

  val order = "order"

  val orderTag = prefixTag(order) _

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
        replyMarkup = startMarkup)(msg)
  }

  onCommand ("/login") {
    implicit msg =>
      reply("Введите пароль, чтобы авторизироваться", replyMarkup = Some(ForceReply()))

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
        msg.from.foreach{
          from =>
            dbActor ! CheckHookahs(from.id, msg)
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
          case Some ("Введите пароль, чтобы авторизироваться") => msg.from.foreach{
            from =>
              dbActor ! VerifyPassword (from.username.get, msg)

          }
          case _ => reply("Извините, не понимаю вас")
        }
    }
  }

  override def preStart(): Unit = {
    run()
  }

  override def receive: Receive = {
    case DenyOrdering(msg, because) =>
      reply("Извините, не могу принять ваш заказ, потому что" + because)(msg)
    case AcceptOrdering(msg) =>
      reply("OK")(msg)
    case EmptyHookahSet(msg) =>
      reply("К сожалению, вы еще не пользовались услугами нашего бота. " +
        "Как только вы посетите одну из кальянных, вы сможете делать в ней заказы.")(msg)
    case HookahSet(set, msg) =>
      reply("Выберите кальянную из списка: ",
        replyMarkup = Some(InlineKeyboardMarkup.singleColumn(
          set.map(s => InlineKeyboardButton.callbackData(s, orderTag(s))).toSeq
        )))(msg)
    case IsEmployeeAuthorized (msg, list) =>
      if (list.nonEmpty)
        reply ("Вы авторизованы")(msg)
      else reply ("Пароль неверен")(msg)
    case _ => Unit
  }

  onCallbackWithTag(order) {
    cbd =>

  }
}

case class IsEmployeeAuthorized (msg : Message, list : List[String] )

case class DenyOrdering(msg: Message, because: String)
case class AcceptOrdering(msg: Message)

case class EmptyHookahSet(msg: Message)
case class HookahSet(set: Set[String], msg: Message)

object HookahBotActor {
  def props(): Props = Props(new HookahBotActor())
}