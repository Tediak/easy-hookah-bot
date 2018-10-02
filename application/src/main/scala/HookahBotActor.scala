import akka.actor.{Actor, Props}
import com.bot4s.telegram.methods.DeleteMessage
import com.bot4s.telegram.{methods, models}
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

  // unique actor for per user
  def userActor(id: Int) =
    context.child(id.toString).getOrElse {
      context.actorOf(UserActor.props(id, dbActor), id.toString)
    }

  val order = "order"
  val taste = "taste"
  val power = "power"
  val comment = "comment"
  val finish = "finish"

  val orderTag = prefixTag(order) _
  val tasteTag = prefixTag(taste) _
  val powerTag = prefixTag(power) _
  val commentTag = prefixTag(comment) _
  val finishTag = prefixTag(finish) _

  def greetings(name: String) = "Привет, " + name +
    "! Это бот-кальянщик, который упростит тебе твою жизнь :)"

  // message for start menu
  val start = "Нажмите любую из кнопок, чтобы сделать новый заказ."

  // two main buttons for starting using bot
  val startMarkup = Some(ReplyKeyboardMarkup.singleRow(
    Seq(
      KeyboardButton.text("Заказать кальян"),
      KeyboardButton.text("Ввести промокод")),
    oneTimeKeyboard = Some(true),
    resizeKeyboard = Some(true)
  ))

  // buttons with three main tastes of hookah
  val tasteMarkup = InlineKeyboardMarkup.singleColumn(Seq(
    InlineKeyboardButton.callbackData("Кислый", tasteTag("Кислый")),
    InlineKeyboardButton.callbackData("Сладкий", tasteTag("Сладкий")),
    InlineKeyboardButton.callbackData("Кисло-сладкий", tasteTag("Кисло-сладкий"))
  ))

  // buttons with power of hookah
  val powerMarkup = InlineKeyboardMarkup.singleColumn(Seq(
    InlineKeyboardButton.callbackData("Слабый", powerTag("Слабый")),
    InlineKeyboardButton.callbackData("Средний", powerTag("Средний")),
    InlineKeyboardButton.callbackData("Сильный", powerTag("Сильный"))
  ))

  // optional comment button
  val commentMarkup = InlineKeyboardMarkup.singleColumn(Seq(
    InlineKeyboardButton.callbackData("Добавить комментарий", commentTag("add")),
    InlineKeyboardButton.callbackData("Нет, спасибо", commentTag("not_need"))
  ))

  // finishing an order
  val finishMarkup = InlineKeyboardMarkup.singleRow(Seq(
    InlineKeyboardButton.callbackData("Отправить", finishTag("accept")),
    InlineKeyboardButton.callbackData("Отменить", finishTag("deny")))
  )

  def startOrdering(msg: Message) =
    reply("Для начала, выберем приблизительный вкус кальяна:",
      replyMarkup = Some(tasteMarkup))(msg)

  // after pressing on the taste button:
  onCallbackWithTag(taste) { implicit cbq =>
    cbq.message.foreach { implicit msg =>
      userActor(cbq.from.id) ! UpdateTaste(cbq.data)
      request(EditMessageText(
        Some(msg.chat.id),
        Some(msg.messageId),
        text = "Вкус: " + cbq.data.getOrElse("")))
      reply("Теперь выберем жёсткость:",
        replyMarkup = Some(powerMarkup))
    }
  }

  // ... power button:
  onCallbackWithTag(power) { implicit cbq =>
    cbq.message.foreach { implicit msg =>
      userActor(cbq.from.id) ! UpdatePower(cbq.data)
      request(EditMessageText(
        Some(msg.chat.id),
        Some(msg.messageId),
        text = "Жесткость: " + cbq.data.getOrElse("")))
      reply("Вы можете добавить необязательный комментарий с особыми пожеланиями :)",
        replyMarkup = Some(commentMarkup))
    }
  }

  // ... comment button
  onCallbackWithTag(comment) { implicit cbq =>
    cbq.data match {
      case Some("add") =>
        cbq.message.foreach { implicit msg =>
          request(DeleteMessage(msg.chat.id, msg.messageId))
          reply("Напишите комментарий в ответ на это сообщение",
            replyMarkup = Some(ForceReply()))
        }
      case Some("not_need") =>
        cbq.message.foreach { implicit msg =>
          request(DeleteMessage(msg.chat.id, msg.messageId))
          reply("Завершите заказ, или отмените, если что-то не так:",
            replyMarkup = Some(finishMarkup))
        }
    }
  }

  onCallbackWithTag(finish) { implicit cbq =>
    cbq.data match {
      case Some("accept") =>
        cbq.message.foreach{ msg =>
          request(DeleteMessage(msg.chat.id, msg.messageId))
          reply("Супер! Ваш заказ был отправлен кальянщику. Ожидайте сообщения...")(msg)
          userActor(cbq.from.id) ! FinishOrdering(msg)
        }
      case Some("deny") =>
        cbq.message.foreach{ msg =>
          userActor(cbq.from.id) ! CancelOrdering(msg)
          request(DeleteMessage(msg.chat.id, msg.messageId))
          reply("Ваш заказ был успешно отменен.")(msg)
        }
    }
  }

  onCommand("/start") {
    implicit msg =>
      reply(greetings(msg.from.map(_.firstName).getOrElse("Неизвестный")),
        replyMarkup = startMarkup)(msg)
  }

  onMessage { implicit msg =>
    using(_.text) {
      case "Заказать кальян" =>
        msg.from.foreach {
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
          case Some("Напишите комментарий в ответ на это сообщение") =>
            userActor(msg.from.map(_.id).getOrElse(0)) ! UpdateComment(msg.text)
            request(DeleteMessage(msg.chat.id, msg.replyToMessage.map(_.messageId).getOrElse(0)))
            reply("Комментарий принят. Завершите заказ, или отмените, если что-то не так:",
              replyMarkup = Some(finishMarkup))
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
    case DenyOrdering(msg, because) =>
      reply("Извините, не могу принять ваш заказ, потому что" + because)(msg)
    case AcceptOrdering(msg) =>
      startOrdering(msg)
      request(DeleteMessage(msg.chat.id, msg.messageId))
    case EmptyHookahSet(msg) =>
      reply("К сожалению, вы еще не пользовались услугами нашего бота. " +
        "Как только вы посетите одну из кальянных, вы сможете делать в ней заказы.")(msg)
    case HookahSet(set, msg) =>
      reply("Выберите из списка: ",
        replyMarkup = Some(InlineKeyboardMarkup.singleColumn(
          set.map(s => InlineKeyboardButton.callbackData(s._2, orderTag(s._1.toString))).toSeq
        )))(msg)
    case _ => Unit
  }

  onCallbackWithTag(order) { implicit cbq =>
    println("Hello")
    println(cbq.data)
    val usrActor = userActor(cbq.from.id)
    cbq.message.foreach { msg => usrActor ! StartOrdering(msg, cbq.data.map(_.toLong).getOrElse(0L)) }
  }
}

case class DenyOrdering(msg: Message, because: String)

case class AcceptOrdering(msg: Message)

case class EmptyHookahSet(msg: Message)

case class HookahSet(set: Set[(Long, String)], msg: Message)

object HookahBotActor {
  def props(): Props = Props(new HookahBotActor())
}