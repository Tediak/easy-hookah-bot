import akka.actor.{Actor, ActorRef, ActorSelection, Props}
import com.bot4s.telegram.methods.{DeleteMessage, EditMessageReplyMarkup, EditMessageText, SendMessage}
import slick.jdbc.PostgresProfile.api._
import com.bot4s.telegram.api.declarative.{Callbacks, Commands}
import com.bot4s.telegram.api.{Polling, TelegramBot}
import com.bot4s.telegram.clients.ScalajHttpClient
import com.bot4s.telegram.models._
import model.{Guest, Order}

import scala.concurrent.Future

class HookahBotActor() extends TelegramBot with Polling with Commands
  with Callbacks with Actor {

  val client = new ScalajHttpClient("626944613:AAFOedBtg34Kl7g3NV1a4w7XeixM0AgIfg8")

  val manager: ActorRef = context.actorOf(EmployeeManagerActor.props, "manager-actor")

  val orderDbActor: ActorRef = context.actorOf(OrderDatabaseActor.props, "order-database-actor")
  val promocodeDbActor: ActorRef = context.actorOf(PromocodeDatabaseActor.props, "promocode-database-actor")
  val employeeDbActor: ActorSelection = context.actorSelection("/user/hookah-bot-actor/manager-actor/employee-database-actor")

  // unique actor for per user
  def userActor(id: Long, user: Option[User]): ActorRef =
    context.child(id.toString).getOrElse {
      val userGuest = (for( u <- user )
        yield Guest(u.username, u.firstName, u.lastName, id)).getOrElse(Guest(None, "", None, 0L))
      context.actorOf(UserActor.props(userGuest), id.toString)
    }

  val order = "order"
  val taste = "taste"
  val power = "power"
  val comment = "comment"
  val finish = "finish"
  val when = "when"
  val stars = "stars"

  val orderTag = prefixTag(order) _
  val tasteTag = prefixTag(taste) _
  val powerTag = prefixTag(power) _
  val commentTag = prefixTag(comment) _
  val whenTag = prefixTag(when) _
  val finishTag = prefixTag(finish) _
  val starsTag = prefixTag(stars) _

  def greetings(name: String) = "Привет, " + name +
    "! Это бот-кальянщик, который упростит тебе твою жизнь :)"

  // message for start menu
  val start = "Нажмите любую из кнопок, чтобы сделать новый заказ."

  // two main buttons for starting using bot
  val userMarkup = Some(ReplyKeyboardMarkup.singleRow(
    Seq(
      KeyboardButton.text("Заказать кальян"),
      KeyboardButton.text("Ввести промокод")),
    oneTimeKeyboard = Some(true),
    resizeKeyboard = Some(true)
  ))


  val hookahMarkup = Some(ReplyKeyboardMarkup.singleRow(
    Seq(
      KeyboardButton.text("Принять заказ"),
      KeyboardButton.text("Отвергнуть заказ")),
    oneTimeKeyboard = Some(true),
    resizeKeyboard = Some(true)
  ))

  val accountMarkup = Some(ReplyKeyboardMarkup.singleButton(
    KeyboardButton.text("/promocode")
  ))

  /* START MENU */

  onCommand("/start") {
    implicit msg =>
      reply(greetings(msg.from.map(_.firstName).getOrElse("Неизвестный")),
        replyMarkup = userMarkup)(msg)
  }

  /* ORDERING */

  // buttons with three main tastes of hookah
  val tasteMarkup: InlineKeyboardMarkup = InlineKeyboardMarkup.singleColumn(Seq(
    InlineKeyboardButton.callbackData("Кислый", tasteTag("Кислый")),
    InlineKeyboardButton.callbackData("Сладкий", tasteTag("Сладкий")),
    InlineKeyboardButton.callbackData("Кисло-сладкий", tasteTag("Кисло-сладкий"))
  ))

  // buttons with power of hookah
  val powerMarkup: InlineKeyboardMarkup = InlineKeyboardMarkup.singleColumn(Seq(
    InlineKeyboardButton.callbackData("Слабый", powerTag("Слабый")),
    InlineKeyboardButton.callbackData("Средний", powerTag("Средний")),
    InlineKeyboardButton.callbackData("Сильный", powerTag("Сильный")),
    InlineKeyboardButton.callbackData("Назад", powerTag("back"))
  ))

  // optional comment button
  val commentMarkup = InlineKeyboardMarkup.singleColumn(Seq(
    InlineKeyboardButton.callbackData("Добавить комментарий", commentTag("add")),
    InlineKeyboardButton.callbackData("Нет, спасибо", commentTag("not_need")),
    InlineKeyboardButton.callbackData("Назад", commentTag("back"))
  ))

  val whenMarkup = InlineKeyboardMarkup.singleColumn(Seq(
    InlineKeyboardButton.callbackData("15 минут", whenTag("15")),
    InlineKeyboardButton.callbackData("30 минут", whenTag("30")),
    InlineKeyboardButton.callbackData("45 минут", whenTag("45")),
    InlineKeyboardButton.callbackData("Один час", whenTag("60")),
    InlineKeyboardButton.callbackData("Назад", whenTag("back"))
  ))

  // finishing an order
  val finishMarkup = InlineKeyboardMarkup.singleRow(Seq(
    InlineKeyboardButton.callbackData("Отправить", finishTag("accept")),
    InlineKeyboardButton.callbackData("Отменить", finishTag("deny"))))


  def starsMarkup(hookahId: Long) = InlineKeyboardMarkup.singleRow(Seq(
    InlineKeyboardButton.callbackData("1", starsTag("1"+hookahId.toString)),
    InlineKeyboardButton.callbackData("2", starsTag("2"+hookahId.toString)),
    InlineKeyboardButton.callbackData("3", starsTag("3"+hookahId.toString)),
    InlineKeyboardButton.callbackData("4", starsTag("4"+hookahId.toString)),
    InlineKeyboardButton.callbackData("5", starsTag("5"+hookahId.toString))
  ))

  def orderMessage(order: Order, guest: Guest): String =
  "Заказ от: " + guest.firstName + " " + guest.lastName.getOrElse("") + " (" + guest.nickname.map("@" + _).getOrElse("без никнейма") + ")\n" +
  "Время прибытия (примерно): " + order.time.toString + "\n" +
  order.hookahTaste.getOrElse("") + ", " + order.hookahPower.getOrElse("") + "\n" +
  "Дополнительный комментарий: " + order.comment.getOrElse("Нет\n")

  onCallbackWithTag(order) { implicit cbq =>
    cbq.message.foreach { implicit msg =>
      request(EditMessageText(Some(msg.source), Some(msg.messageId),
      text = "Кальянная: " + cbq.data.getOrElse("").split(" ").tail.mkString(" "),
        replyMarkup = None))
      userActor(msg.source, Some(cbq.from)) ! StartOrdering(cbq.data.getOrElse("").split(" ").head.toLong)
    }
  }

  def startOrdering(userId: Long) = {
    request(SendMessage(userId,
      text = "Теперь вы можете составить заказ (или, в случае чего, отменить его с помощью комманды /cancel)" +
        "\nВкус кальяна:", replyMarkup = Some(tasteMarkup)))
  }

  // after pressing on the taste button:
  onCallbackWithTag(taste) { implicit cbq =>
    cbq.message.foreach { implicit msg =>
      context.child(msg.source.toString) foreach { _ ! UpdateTaste(cbq.data) }
      request(EditMessageText(
        Some(msg.chat.id),
        Some(msg.messageId),
        text = msg.text.getOrElse("") + " " + cbq.data.getOrElse("") +
          "\nЖёсткость:", replyMarkup = Some(powerMarkup)))
    }
  }

  // ... power button:
  onCallbackWithTag(power) { implicit cbq =>
    cbq.message.foreach { implicit msg =>
      if (cbq.data.contains("back"))
        request(EditMessageText(
          Some(msg.chat.id), Some(msg.messageId),
          text = msg.text.getOrElse("").split('\n').dropRight(2).mkString("\n") + "\nВкус кальяна:",
          replyMarkup = Some(tasteMarkup)
        ))
      else {
        context.child(msg.source.toString) foreach { _ ! UpdatePower(cbq.data) }
        request(EditMessageText(
          Some(msg.chat.id),
          Some(msg.messageId),
          text = msg.text.getOrElse("") + " " + cbq.data.getOrElse("") +
            "\nПриблизительное время прибытия:", replyMarkup = Some(whenMarkup)))
      }
    }
  }

  onCallbackWithTag(when) { implicit cbq =>
    cbq.message.foreach { implicit msg =>
      if (cbq.data.contains("back"))
        request(EditMessageText(
          Some(msg.chat.id), Some(msg.messageId),
          text = msg.text.getOrElse("").split("\n").dropRight(2).mkString("\n") + "\nЖёсткость:",
          replyMarkup = Some(powerMarkup)
        ))
      else {
        context.child(msg.source.toString) foreach { _ ! UpdateWhen(cbq.data) }
        request(EditMessageText(
          Some(msg.chat.id),
          Some(msg.messageId),
          text = msg.text.getOrElse("") + " через " + cbq.data.getOrElse("") + " минут" +
            "\nДополнительный комментарий для кальянщика(по желанию)", replyMarkup = Some(commentMarkup)))
      }
    }
  }

  // ... comment button
  onCallbackWithTag(comment) { implicit cbq =>
    cbq.data match {
      case Some("back") =>
        cbq.message.foreach{ implicit msg =>
          request(EditMessageText(
            Some(msg.chat.id), Some(msg.messageId),
            text = msg.text.getOrElse("").split("\n").dropRight(2).mkString("\n") + "\nПриблизительное время прибытия:",
            replyMarkup = Some(whenMarkup)
          ))
        }
      case Some("add") =>
        cbq.message.foreach { implicit msg =>
          request(EditMessageReplyMarkup(Some(msg.chat.id), Some(msg.messageId), replyMarkup = None))
          reply("Напишите комментарий в ответ на это сообщение",
            replyMarkup = Some(ForceReply()))
        }
      case Some("not_need") =>
        cbq.message.foreach { implicit msg =>
          request(EditMessageText(Some(msg.chat.id), Some(msg.messageId),
            text = msg.text.getOrElse("") + ": без комментариев",
            replyMarkup = None))
          reply("Завершите заказ, или отмените, если что-то не так:",
            replyMarkup = Some(finishMarkup))
        }
    }
  }

  onCallbackWithTag(finish) { implicit cbq =>
    cbq.data match {
      case Some("accept") =>
        cbq.message.foreach { msg =>
          request(EditMessageText(Some(msg.chat.id), Some(msg.messageId),
            text = "Супер! Ваш заказ был отправлен кальянщику. Ожидайте сообщения..."))
          context.child(msg.source.toString) foreach { _ ! FinishOrdering(msg.date) }
        }
      case Some("deny") =>
        cbq.message.foreach { msg =>
          context.child(msg.source.toString) foreach { _ ! CancelOrdering }
          request(DeleteMessage(msg.chat.id, msg.messageId))
        }
    }
  }

  onCallbackWithTag(stars) { implicit cbq =>
    cbq.message.foreach { msg =>
      request(EditMessageText(Some(msg.source), Some(msg.messageId),
        text = "", replyMarkup = userMarkup))
      request(DeleteMessage(msg.source, msg.messageId))
      ackCallback(Some("Спасибо за визит!!!"))
      cbq.data.foreach{ d =>
        promocodeDbActor ! NewVisit(msg.source, d.tail.toLong, msg.date, d.head.toString.toInt)
      }
    }
  }

  /* MESSAGE HANDLER */

  onCommand("/login") {
    implicit msg =>
      reply("Введите пароль, чтобы авторизироваться",
        replyMarkup = Some(ForceReply()))
  }

  onCommand("/logout") {
    implicit msg =>
      request(EditMessageText(Some(msg.source), Some(msg.messageId),
        text = "/logout", replyMarkup = userMarkup))
      manager ! Logout(msg.source)
  }

  onCommand("/promocode") { implicit msg =>
    employeeDbActor ! GetPromocode(msg.source)

  }

  onCommand("/cancel") { implicit msg =>

  }

  onMessage { implicit msg =>
    using(_.text) {
      case "Заказать кальян" =>
        msg.from.foreach { _ => orderDbActor ! CheckHookahs(msg.source) }
      case "Ввести промокод" =>
        reply("Введите промокод, который сказал вам кальянщик", replyMarkup = Some(ForceReply()))
      case _ =>
        "Извините, не понимаю Вас"
    }
    using(_.replyToMessage) {
      rpl =>
        rpl.text match {
          case Some("Напишите комментарий в ответ на это сообщение") =>
            userActor(msg.source, msg.from) ! UpdateComment(msg.text)
            request(DeleteMessage(msg.chat.id, msg.replyToMessage.map(_.messageId).getOrElse(0)))
            reply("Комментарий принят. Завершите заказ, или отмените, если что-то не так:",
              replyMarkup = Some(finishMarkup))
          case Some("Введите промокод, который сказал вам кальянщик") =>
            promocodeDbActor ! CheckPromocode(msg.source, msg.from, msg.text.getOrElse(""))
          case Some("Введите пароль, чтобы авторизироваться") => msg.from.foreach { _ =>
            manager ! Login(msg, msg.text.getOrElse(""))
          }
          case _ => reply("Извините, не понимаю вас")
        }
    }
  }

  override def preStart(): Unit = {
    run()
  }

  /* RECEIVE MESSAGES FROM OTHER ACTORS */

  override def receive: Receive = {
    // from UserActor after starting ordering when previous order isn't completed, or other
    case DenyOrdering(userId, because) =>
      request(SendMessage(userId, text = "Извините, не могу принять ваш заказ, потому что" + because))
      context.child(userId.toString) foreach { user =>
        user ! CancelOrdering }
    // from UserActor when it can start making order
    case AcceptOrdering(userId) =>
      startOrdering(userId)
    // from DatabaseActor, when user didn't use bot earlier
    case EmptyHookahSet(userId) =>
      request(SendMessage(userId, "К сожалению, вы еще не пользовались услугами нашего бота. " +
        "Как только вы посетите одну из кальянных, вы сможете делать в ней заказы."))
    // from DatabaseActor, set of hookahs in which HookahBot was used
    case HookahSet(userId, set) =>
      request(SendMessage(userId, "Выберите заведение из списка:",
        replyMarkup = Some(InlineKeyboardMarkup.singleColumn(
          set.map(s => InlineKeyboardButton.callbackData(s._2, orderTag(s._1.toString + " " + s._2))).toSeq))))
    case OrderCancelled(userId) =>
      request(SendMessage(userId, "Ваш заказ был успешно отменен."))
    case OrderNotCancelled(userId) =>
      request(SendMessage(userId, "Не могу отменить ваш заказ."))
    case SendOrderToEmployees(userOrder, emplSet, guest) =>
      emplSet foreach { id =>
        request(SendMessage(id, orderMessage(userOrder, guest), replyMarkup = hookahMarkup))
      }
    //    case SendOrderMessage(msg) =>
    //      reply("Вам пришел заказ ... от ... ",
    //        replyMarkup = Some(InlineKeyboardMarkup.singleColumn(Seq(
    //          InlineKeyboardButton.callbackData()
    //        ))))

    case CantLogin(chatId, because) =>
      request(SendMessage(chatId, "Не могу войти, потому что" + because))
    case IsLogined(chatId) =>
      request(SendMessage(chatId, "Вы успешно вошли в систему"))
    case IsLogout(chatId) =>
      request(SendMessage(chatId, "Вы успешно вышли из системы",
        replyMarkup = userMarkup))
    case NotLogout(chatId, because) =>
      request(SendMessage(chatId, "Вы не вышли из системы, потому что" + because))
    case AcceptPromocode(chatId, promocode) =>
      request(SendMessage(chatId, "Промокод: " + promocode))
    case DenyPromocode(chatId) =>
      request(SendMessage(chatId, "У вас нет прав"))
    case RightPromocode(chatId, hookahId) =>
      request(SendMessage(chatId, "Вы успешно ввели промокод!\nПоставьте оценку заведению:",
        replyMarkup = Some(starsMarkup(hookahId))))
    case WrongPromocode(chatId) =>
      request(SendMessage(chatId, "Неправильный промокод!"))
    case _ => Unit
  }
}

// Ordering

case class AcceptOrdering(userId: Long)

case class DenyOrdering(userId: Long, because: String)

case class HookahSet(userId: Long, set: Set[(Long, String)])

case class EmptyHookahSet(userId: Long)

case class OrderCancelled(userId: Long)

case class OrderNotCancelled(userId: Long)

case class SendOrderToEmployees(order: Order, emplSet: Set[Long], guest: Guest)

// Login

case class IsLogined(chatId: Long)

case class CantLogin(chatId: Long, because: String)

case class IsLogout(chatId: Long)

case class NotLogout(chatId: Long, because: String)

// Promocode

case class AcceptPromocode(chatId: Long, promocode: String)

case class DenyPromocode(chatId: Long)

case class RightPromocode(chatId: Long, hookahId: Long)

case class WrongPromocode(chatId: Long)

object HookahBotActor {
  def props(): Props = Props(new HookahBotActor())
}