import java.time.LocalDateTime

import akka.actor.{Actor, ActorRef, ActorSelection, PoisonPill, Props}
import com.bot4s.telegram.methods._
import com.bot4s.telegram.api.declarative.{Callbacks, Commands}
import com.bot4s.telegram.api.{Polling, TelegramBot}
import com.bot4s.telegram.clients.ScalajHttpClient
import com.bot4s.telegram.models._
import model.{Guest, Order}

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.io.Source

class HookahBotActor() extends TelegramBot with Polling with Commands
  with Callbacks with Actor {

  val client = new ScalajHttpClient(Source.fromFile("bot.token").mkString)

  val manager = context.actorOf(EmployeeManagerActor.props, "manager-actor")

  val orderDbActor: ActorRef = context.actorOf(OrderDatabaseActor.props, "order-database-actor")
  val promocodeDbActor: ActorRef = context.actorOf(
    PromocodeDatabaseActor.props,
    name = "promocode-database-actor")
  val employeeDbActor: ActorSelection = context.actorSelection("/user/hookah-bot-actor/manager-actor/employee-database-actor")

  // unique actor for per user
  def userActor(id: Long, user: Option[User]): ActorRef =
    context.child(id.toString).getOrElse {
      val userGuest = (for (u <- user)
        yield Guest(u.username, u.firstName, u.lastName, id)).getOrElse(Guest(None, "", None, 0L))
      context.actorOf(UserActor.props(userGuest), id.toString)
    }

  def dateFormatter(date: LocalDateTime) = date.toLocalTime.toString

  val order = "order"
  val taste = "taste"
  val power = "power"
  val comment = "comment"
  val finish = "finish"
  val when = "when"
  val stars = "stars"
  val receiveOrder = "receive"

  val orderTag = prefixTag(order) _
  val tasteTag = prefixTag(taste) _
  val powerTag = prefixTag(power) _
  val commentTag = prefixTag(comment) _
  val whenTag = prefixTag(when) _
  val finishTag = prefixTag(finish) _
  val starsTag = prefixTag(stars) _

  def receiveOrderTag(orderId: Long) = prefixTag(receiveOrder + orderId.toString + ":") _

  def greetings(name: String) = "*Привет,* *" + name +
    "*! Я бот-кальянщик, который немного упростит тебе твою жизнь, и поможет " +
    "заказать кальян прямо из своего телефона \uD83C\uDF2A"

  // message for start menu
  val start = "Нажмите любую из кнопок, чтобы сделать новый заказ."

  // two main buttons for starting using bot
  val userMarkup = Some(ReplyKeyboardMarkup.singleColumn(
    Seq(
      KeyboardButton.text("Заказать кальян\uD83C\uDF2A"),
      KeyboardButton.text("Ввести промокод\uD83D\uDD20"),
      KeyboardButton.text("Посмотреть статистику\uD83D\uDCC8")),
    oneTimeKeyboard = Some(true),
    resizeKeyboard = Some(true)
  ))


  def receiveOrderMarkup(orderId: Long) = Some(InlineKeyboardMarkup.singleRow(
    Seq(
      InlineKeyboardButton.callbackData("✅Принять заказ", receiveOrderTag(orderId)("accept")),
      InlineKeyboardButton.callbackData("\uD83D\uDEABОтвергнуть заказ", receiveOrderTag(orderId)("deny")))
  ))

  val accountMarkup = Some(ReplyKeyboardMarkup.singleButton(
    KeyboardButton.text("\uD83D\uDD20Получить промокод")
  ))

  /* START MENU */

  onCommand("/start") {
    implicit msg =>
      reply(greetings(msg.from.map(_.firstName).getOrElse("Неизвестный")),
        replyMarkup = userMarkup,
        parseMode = Some(ParseMode.Markdown))(msg)
  }

  /* ORDERING */

  // buttons with three main tastes of hookah
  val tasteMarkup: InlineKeyboardMarkup = InlineKeyboardMarkup.singleColumn(Seq(
    InlineKeyboardButton.callbackData("\uD83C\uDF4BКислый", tasteTag("Кислый")),
    InlineKeyboardButton.callbackData("\uD83C\uDF6DСладкий", tasteTag("Сладкий")),
    InlineKeyboardButton.callbackData("\uD83C\uDF4EКисло-сладкий", tasteTag("Кисло-сладкий"))
  ))

  // buttons with power of hookah
  val powerMarkup: InlineKeyboardMarkup = InlineKeyboardMarkup.singleColumn(Seq(
    InlineKeyboardButton.callbackData("\uD83D\uDD25", powerTag("Слабый")),
    InlineKeyboardButton.callbackData("\uD83D\uDD25 \uD83D\uDD25", powerTag("Средний")),
    InlineKeyboardButton.callbackData("\uD83D\uDD25 \uD83D\uDD25 \uD83D\uDD25", powerTag("Сильный")),
    InlineKeyboardButton.callbackData("⬅️Назад", powerTag("back"))
  ))

  // optional comment button
  val commentMarkup = InlineKeyboardMarkup.singleColumn(Seq(
    InlineKeyboardButton.callbackData("✉️Добавить комментарий", commentTag("add")),
    InlineKeyboardButton.callbackData("Нет, спасибо", commentTag("not_need")),
    InlineKeyboardButton.callbackData("⬅️Назад", commentTag("back"))
  ))

  val whenMarkup = InlineKeyboardMarkup.singleColumn(Seq(
    InlineKeyboardButton.callbackData("\uD83D\uDD5215 минут", whenTag("15")),
    InlineKeyboardButton.callbackData("\uD83D\uDD5530 минут", whenTag("30")),
    InlineKeyboardButton.callbackData("\uD83D\uDD5845 минут", whenTag("45")),
    InlineKeyboardButton.callbackData("\uD83D\uDD5BОдин час", whenTag("60")),
    InlineKeyboardButton.callbackData("⬅️Назад", whenTag("back"))
  ))

  // finishing an order
  val finishMarkup = InlineKeyboardMarkup.singleRow(Seq(
    InlineKeyboardButton.callbackData("✅Отправить", finishTag("accept")),
    InlineKeyboardButton.callbackData("\uD83D\uDEABОтменить", finishTag("deny"))))


  def starsMarkup(hookahId: Long) = InlineKeyboardMarkup.singleRow(Seq(
    InlineKeyboardButton.callbackData("1", starsTag("1" + hookahId.toString)),
    InlineKeyboardButton.callbackData("2", starsTag("2" + hookahId.toString)),
    InlineKeyboardButton.callbackData("3", starsTag("3" + hookahId.toString)),
    InlineKeyboardButton.callbackData("4", starsTag("4" + hookahId.toString)),
    InlineKeyboardButton.callbackData("5", starsTag("5" + hookahId.toString))
  ))

  def orderMessage(order: Order, guest: Guest): String =
    "Заказ *#" + order.id + "* от: *" + guest.firstName + " " + guest.lastName.getOrElse("") + "* (" + guest.nickname.map("@" + _).getOrElse("без никнейма") + ")\n" +
      "_Время прибытия (примерно):_ " + dateFormatter(order.time) + "\n" +
      "_Вкус: " + order.hookahTaste.getOrElse("") +"_\n" +
      "_Сила: " + order.hookahPower.getOrElse("") + "_\n" +
      "_Дополнительный комментарий: _" + order.comment.getOrElse("Нет\n")

  onCallbackWithTag(order) { implicit cbq =>
    cbq.message.foreach { implicit msg =>
      request(EditMessageText(Some(msg.source), Some(msg.messageId),
        text = "Кальянная: " + cbq.data.getOrElse("").split(" ").tail.mkString(" "),
        replyMarkup = None))
      userActor(msg.source, Some(cbq.from)) ! StartOrdering(cbq.data.getOrElse("").split(" ").head.toLong)
    }
  }

  onCallbackWithTag(receiveOrder) { implicit cbq => //"receive:1:accept" -- cbq.data == Some(1:accept)
    cbq.message.foreach { msg =>
      cbq.data foreach { data =>
        data.split(":")(1) match {
          case "accept" =>
            employeeDbActor ! AcceptOrder(msg.source, data.split(":").head.toLong)
            request(EditMessageText(Some(msg.source), Some(msg.messageId),
              text = "Заказ *#" + data.split(":").head + "* был принят.",
              replyMarkup = None,
              parseMode = Some(ParseMode.Markdown)))
          case "deny" =>
            request(EditMessageText(Some(msg.source), Some(msg.messageId),
              text = "Заказ *#" + data.split(":").head + "* был отменен",
              replyMarkup = None,
              parseMode = Some(ParseMode.Markdown)))

        }
      }
    }
  }

  def startOrdering(userId: Long) = {
    request(SendMessage(userId,
      text = "_Давай составим заказ_" +
        "\n*Вкус кальяна*:", replyMarkup = Some(tasteMarkup), parseMode = Some(ParseMode.Markdown)))
  }

  // after pressing on the taste button:
  onCallbackWithTag(taste) { implicit cbq =>
    cbq.message.foreach { implicit msg =>
      context.child(msg.source.toString) foreach {
        _ ! UpdateTaste(cbq.data)
      }
      request(EditMessageText(
        Some(msg.chat.id),
        Some(msg.messageId),
        text = msg.text.getOrElse("") + " " + cbq.data.getOrElse("") +
          "\nЖёсткость:", replyMarkup = Some(powerMarkup), parseMode = Some(ParseMode.Markdown)))
    }
  }

  // ... power button:
  onCallbackWithTag(power) { implicit cbq =>
    cbq.message.foreach { implicit msg =>
      if (cbq.data.contains("back"))
        request(EditMessageText(
          Some(msg.chat.id), Some(msg.messageId),
          text = msg.text.getOrElse("").split('\n').dropRight(2).mkString("\n") + "\nВкус кальяна:",
          replyMarkup = Some(tasteMarkup),
          parseMode = Some(ParseMode.Markdown)
        ))
      else {
        context.child(msg.source.toString) foreach {
          _ ! UpdatePower(cbq.data)
        }
        request(EditMessageText(
          Some(msg.chat.id),
          Some(msg.messageId),
          text = msg.text.getOrElse("") + " " + cbq.data.getOrElse("") +
            "\nПриблизительное время прибытия:",
          replyMarkup = Some(whenMarkup),
          parseMode = Some(ParseMode.Markdown)))
      }
    }
  }

  onCallbackWithTag(when) { implicit cbq =>
    cbq.message.foreach { implicit msg =>
      if (cbq.data.contains("back"))
        request(EditMessageText(
          Some(msg.chat.id), Some(msg.messageId),
          text = msg.text.getOrElse("").split("\n").dropRight(2).mkString("\n") + "\nЖёсткость:",
          replyMarkup = Some(powerMarkup),
          parseMode = Some(ParseMode.Markdown)
        ))
      else {
        context.child(msg.source.toString) foreach {
          _ ! UpdateWhen(cbq.data)
        }
        request(EditMessageText(
          Some(msg.chat.id),
          Some(msg.messageId),
          text = msg.text.getOrElse("") + " через " + cbq.data.getOrElse("") + " минут" +
            "\nДополнительный комментарий для кальянщика(по желанию)",
          replyMarkup = Some(commentMarkup),
          parseMode = Some(ParseMode.Markdown)))
      }
    }
  }

  // ... comment button
  onCallbackWithTag(comment) { implicit cbq =>
    cbq.data match {
      case Some("back") =>
        cbq.message.foreach { implicit msg =>
          request(EditMessageText(
            Some(msg.chat.id), Some(msg.messageId),
            text = msg.text.getOrElse("").split("\n").dropRight(2).mkString("\n") + "\nПриблизительное время прибытия:",
            replyMarkup = Some(whenMarkup),
            parseMode = Some(ParseMode.Markdown)
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
            replyMarkup = Some(finishMarkup),
            parseMode = Some(ParseMode.Markdown))
        }
    }
  }

  onCallbackWithTag(finish) { implicit cbq =>
    cbq.data match {
      case Some("accept") =>
        cbq.message.foreach { msg =>
          request(EditMessageText(Some(msg.chat.id), Some(msg.messageId),
            text = "Супер! Ваш заказ был отправлен кальянщику. Ожидайте сообщения...",
            parseMode = Some(ParseMode.Markdown)))
          context.child(msg.source.toString) foreach {
            _ ! FinishOrdering(msg.date)
          }
        }
      case Some("deny") =>
        cbq.message.foreach { msg =>
          context.child(msg.source.toString) foreach {
            _ ! CancelOrdering
          }
          request(DeleteMessage(msg.chat.id, msg.messageId))
        }
    }
  }

  onCallbackWithTag(stars) { implicit cbq =>
    cbq.message.foreach { msg =>
      request(EditMessageText(Some(msg.source), Some(msg.messageId),
        text = "", replyMarkup = userMarkup))
      request(EditMessageText(Some(msg.source), Some(msg.messageId),
        text = msg.text.getOrElse(""),
        replyMarkup = userMarkup))
      request(DeleteMessage(msg.source, msg.messageId))
      ackCallback(Some("Оценка отправлена"))
      request(SendMessage(msg.source, "Спасибо за визит!",
        replyMarkup = userMarkup))
      cbq.data.foreach { d =>
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
      case "Заказать кальян\uD83C\uDF2A" =>
        if (context.child(msg.source.toString).nonEmpty)
          reply("Пока что вы не можете заказать кальян.")
        else
        orderDbActor ! CheckHookahs(msg.source)
      case "Ввести промокод\uD83D\uDD20" =>
        reply("Введите промокод, который сказал вам кальянщик", replyMarkup = Some(ForceReply()))
      case "Посмотреть статистику\uD83D\uDCC8" =>
        promocodeDbActor ! GetStats(msg.source)
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
      context.child(userId.toString) foreach { _ ! PoisonPill }
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
      context.child(userId.toString).foreach{ _ ! PoisonPill }
    case OrderNotCancelled(userId) =>
      request(SendMessage(userId, "Не могу отменить ваш заказ."))
    case SendOrderToEmployees(userOrder, emplSet, guest) =>
      emplSet foreach { id =>
        request(SendMessage(id, orderMessage(userOrder, guest),
          replyMarkup = receiveOrderMarkup(userOrder.id),
          parseMode = Some(ParseMode.Markdown)))
      }
    case OrderTimeout(userOrder) =>
      request(SendMessage(userOrder.guestId, "Ответа от кальянщиков не было на протяжении 10 минут, поэтому заказ #" +
        userOrder.id.toString + " был отменен. Приношу извинения за неудобства"))
      context.child(userOrder.guestId.toString) foreach { _ ! PoisonPill }
    case OrderWasAccepted(userId, userOrder) =>
      request(SendMessage(userId, "Ваш заказ (#" + userOrder.id.toString + ") был принят! Ожидаем вас в " + dateFormatter(userOrder.time)))
      context.child(userId.toString) foreach { context.system.scheduler.scheduleOnce(60 minutes, _, PoisonPill) }
    case CantLogin(chatId, because) =>
      request(SendMessage(chatId, "\uD83D\uDD34Не могу войти, потому что" + because))
    case IsLogined(chatId) =>
      request(SendMessage(chatId, "\uD83D\uDD34Вы успешно вошли в систему"))
    case IsLogout(chatId) =>
      request(SendMessage(chatId, "\uD83D\uDD34Вы успешно вышли из системы",
        replyMarkup = userMarkup))
    case NotLogout(chatId, because) =>
      request(SendMessage(chatId, "\uD83D\uDD34Вы не вышли из системы, потому что" + because))
    case AcceptPromocode(chatId, promocode) =>
      request(SendMessage(chatId, "\uD83D\uDD34Промокод: " + promocode))
    case DenyPromocode(chatId) =>
      request(SendMessage(chatId, "\uD83D\uDD34У вас нет прав"))
    case RightPromocode(chatId, hookahId) =>
      request(SendMessage(chatId, "Вы успешно ввели промокод!\nПоставьте оценку заведению:",
        replyMarkup = Some(starsMarkup(hookahId))))
    case WrongPromocode(chatId) =>
      request(SendMessage(chatId, "‼️Неправильный промокод!", replyMarkup = userMarkup))
    case OrderAlreadyAccepted(accId, orderId) =>
      request(SendMessage(accId, "\uD83D\uDD34\uD83D\uDD34\uD83D\uDD34" +
        "\nЗаказ #" + orderId.toString + " уже принят другим кальянщиком, его делать не нужно."))
    case UserStats(userId, statsSet) =>
      if(statsSet.isEmpty) request(SendMessage(userId, "К сожалению, вы еще не пользовались услугами нашего бота."))
      else
        request(SendMessage(userId,
        "\uD83D\uDCC8*Статистика*\n\n" + statsSet.map{ value =>
          val count = value._2._2
          val isFree = count % value._2._3 == 0
          "_Кальянная:_ " + value._1 + "\n" +
          "_Средняя оценка:_ " + value._2._1.toString +"\n" +
          "_Всего посещений:_ " + count + "\n" +
          { if (isFree) "\uD83D\uDD34*У вас бесплатный кальян!*\n" else "" } +
          "_Осталось кальянов до бесплатного:_ " + (value._2._3 - (count % value._2._3)).toString + "\n"
        }.mkString("\n"),
          parseMode = Some(ParseMode.Markdown)))
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

case class OrderWasAccepted(userId: Long, order: Order)

case class OrderTimeout(order: Order)

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

case class OrderAlreadyAccepted(accId: Long, orderId: Long)

case class UserStats(userId: Long, statsSet: Set[(String, (Double, Int, Int))])

object HookahBotActor {
  def props(): Props = Props(new HookahBotActor())
}