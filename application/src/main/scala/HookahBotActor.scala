import akka.actor.{Actor, ActorRef, ActorSelection, Props}
import com.bot4s.telegram.methods.{DeleteMessage, EditMessageReplyMarkup, EditMessageText, SendMessage}
import slick.jdbc.PostgresProfile.api._
import com.bot4s.telegram.api.declarative.{Callbacks, Commands}
import com.bot4s.telegram.api.{Polling, TelegramBot}
import com.bot4s.telegram.clients.ScalajHttpClient
import com.bot4s.telegram.models._

class HookahBotActor() extends TelegramBot with Polling with Commands
  with Callbacks with Actor {

  val client = new ScalajHttpClient("626944613:AAFOedBtg34Kl7g3NV1a4w7XeixM0AgIfg8")

  val manager: ActorRef = context.actorOf(EmployeeManagerActor.props, "manager-actor")

  val orderDbActor: ActorRef = context.actorOf(OrderDatabaseActor.props, "order-database-actor")
  val promocodeDbActor: ActorRef = context.actorOf(PromocodeDatabaseActor.props, "promocode-database-actor")
  val employeeDbActor: ActorSelection = context.actorSelection("/user/hookah-bot-actor/manager-actor/employee-database-actor")

  // unique actor for per user
  def userActor(id: Int) =
    context.child(id.toString).getOrElse {
      context.actorOf(UserActor.props(id), id.toString)
    }

  val order = "order"
  val taste = "taste"
  val power = "power"
  val comment = "comment"
  val finish = "finish"
  val when = "when"

  val orderTag = prefixTag(order) _
  val tasteTag = prefixTag(taste) _
  val powerTag = prefixTag(power) _
  val commentTag = prefixTag(comment) _
  val whenTag = prefixTag(when) _
  val finishTag = prefixTag(finish) _

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
    InlineKeyboardButton.callbackData("Сильный", powerTag("Сильный"))
  ))

  // optional comment button
  val commentMarkup = InlineKeyboardMarkup.singleColumn(Seq(
    InlineKeyboardButton.callbackData("Добавить комментарий", commentTag("add")),
    InlineKeyboardButton.callbackData("Нет, спасибо", commentTag("not_need"))
  ))

  val whenMarkup = InlineKeyboardMarkup.singleColumn(Seq(
    InlineKeyboardButton.callbackData("15 минут", whenTag("15")),
    InlineKeyboardButton.callbackData("30 минут", whenTag("30")),
    InlineKeyboardButton.callbackData("45 минут", whenTag("45")),
    InlineKeyboardButton.callbackData("Один час", whenTag("60"))
  ))

  // finishing an order
  val finishMarkup = InlineKeyboardMarkup.singleRow(Seq(
    InlineKeyboardButton.callbackData("Отправить", finishTag("accept")),
    InlineKeyboardButton.callbackData("Отменить", finishTag("deny"))))

  onCallbackWithTag(order) { implicit cbq =>
    val usrActor = userActor(cbq.from.id)
    cbq.message.foreach { msg => usrActor ! StartOrdering(msg, cbq.data.map(_.toLong).getOrElse(0L)) }
  }

  def startOrdering(msg: Message) =
    reply("Вкус кальяна: ",
      replyMarkup = Some(tasteMarkup))(msg)

  // after pressing on the taste button:
  onCallbackWithTag(taste) { implicit cbq =>
    cbq.message.foreach { implicit msg =>
      userActor(cbq.from.id) ! UpdateTaste(cbq.data)
      request(EditMessageText(
        Some(msg.chat.id),
        Some(msg.messageId),
        text = msg.text.getOrElse("") + cbq.data.getOrElse("") +
          "\nЖёсткость: ", replyMarkup = Some(powerMarkup)))
    }
  }

  // ... power button:
  onCallbackWithTag(power) { implicit cbq =>
    cbq.message.foreach { implicit msg =>
      userActor(cbq.from.id) ! UpdatePower(cbq.data)
      request(EditMessageText(
        Some(msg.chat.id),
        Some(msg.messageId),
        text = msg.text.getOrElse("") + cbq.data.getOrElse("") +
          "\nПриблизительное время прибытия: ", replyMarkup = Some(whenMarkup)))
    }
  }

  onCallbackWithTag(when) { implicit cbq =>
    cbq.message.foreach { implicit msg =>
      userActor(cbq.from.id) ! UpdateWhen(cbq.data)
      request(EditMessageText(
        Some(msg.chat.id),
        Some(msg.messageId),
        text = msg.text.getOrElse("") + " через " + cbq.data.getOrElse("") + " минут" +
          "\nДополнительный комментарий для кальянщика(по желанию)", replyMarkup = Some(commentMarkup)))
    }
  }

  // ... comment button
  onCallbackWithTag(comment) { implicit cbq =>
    cbq.data match {
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
          request(DeleteMessage(msg.chat.id, msg.messageId))
          reply("Супер! Ваш заказ был отправлен кальянщику. Ожидайте сообщения...")(msg)
          userActor(cbq.from.id) ! FinishOrdering(msg)
        }
      case Some("deny") =>
        cbq.message.foreach { msg =>
          userActor(cbq.from.id) ! CancelOrdering(msg)
          request(DeleteMessage(msg.chat.id, msg.messageId))
          reply("Ваш заказ был успешно отменен.")(msg)
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
      manager ! Logout(msg.source)
  }

  onCommand("/promocode") { implicit msg =>
    employeeDbActor ! GetPromocode(msg.source)
  }

  onMessage { implicit msg =>
    using(_.text) {
      case "Заказать кальян" =>
        msg.from.foreach {
          from =>
            orderDbActor ! CheckHookahs(from.id, msg)
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
            promocodeDbActor ! CheckPromocode(msg.source, msg.text.getOrElse(""))
          case Some("Введите пароль, чтобы авторизироваться") => msg.from.foreach {
            from =>
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
    case DenyOrdering(msg, because) =>
      reply("Извините, не могу принять ваш заказ, потому что" + because)(msg)
    // from UserActor when it can start making order
    case AcceptOrdering(msg) =>
      startOrdering(msg)
      request(DeleteMessage(msg.chat.id, msg.messageId))
    // from DatabasActor, when user didn't use bot earlier
    case EmptyHookahSet(msg) =>
      reply("К сожалению, вы еще не пользовались услугами нашего бота. " +
        "Как только вы посетите одну из кальянных, вы сможете делать в ней заказы.")(msg)
    // from DatabaseActor, set of hookahs in which HookahBot was used
    case HookahSet(set, msg) =>
      reply("Выберите из списка: ",
        replyMarkup = Some(InlineKeyboardMarkup.singleColumn(
          set.map(s => InlineKeyboardButton.callbackData(s._2, orderTag(s._1.toString))).toSeq
        )))(msg)
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
      request(SendMessage(chatId, "Промокод: " + promocode.getOrElse("")))
    case DenyPromocode(chatId) =>
      request(SendMessage(chatId, "У вас нет прав"))
    case RightPromocode(chatId) =>
      request(SendMessage(chatId, "Вы успешно ввели промокод! Поставьте оценку заведению"))
    case WrongPromocode(chatId) =>
      request(SendMessage(chatId, "Неправильный промокод!"))
    case _ => Unit
  }
}

// Ordering

case class AcceptOrdering(msg: Message)

case class DenyOrdering(msg: Message, because: String)

case class HookahSet(set: Set[(Long, String)], msg: Message)

case class EmptyHookahSet(msg: Message)

// Login

case class IsLogined(chatId: Long)

case class CantLogin(chatId: Long, because: String)

case class IsLogout(chatId: Long)

case class NotLogout(chatId: Long, because: String)

// Promocode

case class AcceptPromocode(chatId: Long, promocode: Option[String])

case class DenyPromocode(chatId: Long)

case class RightPromocode(chatId: Long)

case class WrongPromocode(chatId: Long)

object HookahBotActor {
  def props(): Props = Props(new HookahBotActor())
}