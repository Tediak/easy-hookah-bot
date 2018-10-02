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

  val start = "Нажмите любую из кнопок, чтобы сделать новый заказ."

  val startMarkup = Some(ReplyKeyboardMarkup.singleRow(
    Seq(
      KeyboardButton.text("Заказать кальян"),
      KeyboardButton.text("Ввести промокод")),
    oneTimeKeyboard = Some(true),
    resizeKeyboard = Some(true)
  ))

  val tasteMarkup = InlineKeyboardMarkup.singleColumn(Seq(
    InlineKeyboardButton.callbackData("Кислый", tasteTag("Кислый")),
    InlineKeyboardButton.callbackData("Сладкий", tasteTag("Сладкий")),
    InlineKeyboardButton.callbackData("Кисло-сладкий", tasteTag("Кисло-сладкий"))
  ))

  val powerMarkup = InlineKeyboardMarkup.singleColumn(Seq(
    InlineKeyboardButton.callbackData("Слабый", powerTag("Слабый")),
    InlineKeyboardButton.callbackData("Средний", powerTag("Средний")),
    InlineKeyboardButton.callbackData("Сильный", powerTag("Сильный"))
  ))

  val commentMarkup = InlineKeyboardMarkup.singleButton(
    InlineKeyboardButton.callbackData("Отдельные пожелания", commentTag("add")))

  val Markup = InlineKeyboardMarkup.singleRow(Seq(
    InlineKeyboardButton.callbackData("Отправить", finishTag("accept")),
    InlineKeyboardButton.callbackData("Отменить", finishTag("deny")))
  )

  def orderMenu(msg: Message) = {
    reply("Вкус", replyMarkup = Some(tasteMarkup))(msg)
    reply("Жёсткость", replyMarkup = Some(powerMarkup))(msg)
    reply("Отдельные пожелания", replyMarkup = Some(commentMarkup))(msg)
  }

  onCommand("/start") {
    implicit msg =>
      reply(greetings(msg.from.map(_.firstName).getOrElse("Неизвестный")),
        replyMarkup = startMarkup)(msg)
  }

  onCommand ("/login") {
    implicit msg =>
      reply("Введите пароль, чтобы авторизироваться", replyMarkup = Some(ForceReply()))
  }

  onCommand ("/logout")
  {
    implicit msg =>
      msg.from.foreach{
        from =>dbActor ! Logout (from.username.get,msg)
      }

  }

  onMessage{ implicit msg =>
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
            reply("Ваш комментарий принят", replyMarkup = Some(
              InlineKeyboardMarkup.singleColumn(Seq(
                InlineKeyboardButton.callbackData("Изменить комментарий", commentTag("change")),
                InlineKeyboardButton.callbackData("Удалить комментарий", commentTag("delete"))
              ))
            ))
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
      orderMenu(msg)
      request(DeleteMessage(msg.chat.id, msg.messageId))
    case EmptyHookahSet(msg) =>
      reply("К сожалению, вы еще не пользовались услугами нашего бота. " +
        "Как только вы посетите одну из кальянных, вы сможете делать в ней заказы.")(msg)
    case HookahSet(set, msg) =>
      reply("Выберите кальянную из списка: ",
        replyMarkup = Some(InlineKeyboardMarkup.singleColumn(
          set.map(s => InlineKeyboardButton.callbackData(s._2, orderTag(s._1.toString))).toSeq
        )))(msg)
    case IsEmployeeAuthorized (msg, list) =>
      if (list.nonEmpty) {
        reply("Вы авторизованы\nНе забудьте выйти из аккаунта с помощью комманды /logout")(msg)
      }
      else reply ("Пароль неверен")(msg)
    case BotLogout (msg,isLogout : Boolean) =>
      if (isLogout)
        reply ("Вы успешно вышли из аккаунта")(msg)
      else
        reply ("Вы еще не авторизовались")(msg)
    case EmployeeIsAlreadyAuthorized (msg) =>
      reply ("Вы уже авторизованы\nВы можете выйти из аккаунта с помощью комманды /logout ")(msg)
    case EmployeeIsNotAuthorizedYet (msg) =>
      reply ("Вы еще не авторизовались")(msg)
    case _ => Unit
  }

  onCallbackWithTag(order) { implicit cbq =>
    println(cbq.data)
    val usrActor = userActor(cbq.from.id)
    cbq.message.foreach { msg => usrActor ! StartOrdering(msg, cbq.data.map(_.toLong).getOrElse(0)) }
  }

  onCallbackWithTag(taste) { implicit cbq =>
    cbq.data match {
      case Some("cancel") =>
        cbq.message.foreach { msg =>
          request(EditMessageReplyMarkup(
            Some(msg.chat.id),
            Some(msg.messageId),
            replyMarkup = Some(tasteMarkup)
          ))
        }
      case _ =>
        cbq.message.foreach { msg =>
          userActor(cbq.from.id) ! UpdateTaste(cbq.data)
          request(EditMessageText(
            Some(msg.chat.id),
            Some(msg.messageId),
            text = "Вкус: " + cbq.data.getOrElse(""),
            replyMarkup = Some(InlineKeyboardMarkup.singleButton(
              InlineKeyboardButton.callbackData("Изменить", tasteTag("cancel"))
            ))))
        }
    }
  }

  onCallbackWithTag(power) { implicit cbq =>
    cbq.data match {
      case Some("cancel") =>
        cbq.message.foreach { msg =>
          request(EditMessageReplyMarkup(
            Some(msg.chat.id),
            Some(msg.messageId),
            replyMarkup = Some(powerMarkup)
          ))
        }
      case _ =>
        cbq.message.foreach { msg =>
          userActor(cbq.from.id) ! UpdatePower(cbq.data)
          request(EditMessageText(
            Some(msg.chat.id),
            Some(msg.messageId),
            text = "Жесткость: " + cbq.data.getOrElse(""),
            replyMarkup = Some(InlineKeyboardMarkup.singleButton(
              InlineKeyboardButton.callbackData("Изменить", powerTag("cancel"))
            ))))
        }
    }
  }

  onCallbackWithTag(comment) { implicit cbq =>
    cbq.data match {
      case Some("add") =>
        cbq.message.foreach { implicit msg =>
          request(DeleteMessage(msg.chat.id, msg.messageId))
          reply("Напишите комментарий в ответ на это сообщение",
            replyMarkup = Some(ForceReply()))
        }
      case Some("change") =>
        cbq.message.foreach { implicit msg =>
          request(DeleteMessage(msg.chat.id, msg.messageId))
          reply("Напишите комментарий в ответ на это сообщение",
            replyMarkup = Some(ForceReply()))
        }
      case Some("delete") =>
        cbq.message.foreach { implicit msg =>
          request(DeleteMessage(msg.chat.id, msg.messageId))
          reply("Отдельные пожелания", replyMarkup = Some(commentMarkup))
        }
    }
  }

//  onCallbackWithTag(finish) { implicit cbq =>
//    cbq.data match {
//      case Some("accept") =>
//        cbq.message.foreach{ msg =>
//          userActor(cbq.from.id) ! FinishOrdering(msg)
//        }
//      case Some("deny") =>
//        cbq.message.foreach{
//          userActor(cbq.from.id) ! CancelOrdering(msg)
//        }
//    }
//  }
}

case class EmployeeIsNotAuthorizedYet (msg: Message)

case class EmployeeIsAlreadyAuthorized (msg : Message)

case class IsEmployeeAuthorized (msg : Message, list : List[String] )

case class BotLogout (msg: Message, isLogout : Boolean)

case class DenyOrdering(msg: Message, because: String)

case class AcceptOrdering(msg: Message)

  case class EmptyHookahSet(msg: Message)

  case class HookahSet(set: Set[(Long, String)], msg: Message)

  object HookahBotActor {
    def props(): Props = Props(new HookahBotActor())
  }