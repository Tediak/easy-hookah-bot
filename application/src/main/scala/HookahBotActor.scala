import java.time.{Instant, LocalDateTime}
import java.util.TimeZone

import akka.actor.{Actor, ActorRef, ActorSelection, PoisonPill, Props}
import com.bot4s.telegram.methods._
import com.bot4s.telegram.api.declarative.{Callbacks, Commands}
import com.bot4s.telegram.api.{Polling, TelegramBot}
import com.bot4s.telegram.clients.ScalajHttpClient
import com.bot4s.telegram.models._
import datatables._
import model._

import scala.util.{Failure, Random, Success}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.duration._
import scala.io.Source

class HookahBotActor() extends TelegramBot with Polling with Commands
  with Callbacks with Actor {

  type PrefixTag = String => String

  val client = new ScalajHttpClient(Source.fromFile("bot.token").mkString)

  val db = Database.forConfig("postgres")

  val hookahRepository = new HookahRepository(db)
  val accountRepository = new AccountRepository(db)
  val orderRepository = new OrderRepository(db)
  val visitRepository = new VisitRepository(db)
  val guestRepository = new GuestRepository(db)

  // unique actor for per user
  def userActor(id: Long, user: Option[User]): ActorRef =
    context.child(id.toString).getOrElse {
      val userGuest = (for (u <- user)
        yield Guest(u.username, u.firstName, u.lastName, id)).getOrElse(Guest(None, "", None, 0L))
      context.actorOf(UserActor.props(userGuest), id.toString)
    }

  def dateFormatter(date: LocalDateTime) = date.toLocalTime.toString.dropRight(3)

  def epochToLocalDateTimeConverter(epoch: Int): LocalDateTime =
    LocalDateTime.ofInstant(Instant.ofEpochSecond(epoch), TimeZone.getDefault.toZoneId).plusHours(3)

  def generateRandomCode(hookahCode: String): String = {
    val newCode = hookahCode.take(2) +
      Random.nextInt(10).toString + Random.nextInt(10).toString + Random.nextInt(10).toString
    if (hookahCode == newCode) generateRandomCode(hookahCode)
    else newCode
  }

  val order = "order"
  val taste = "taste"
  val power = "power"
  val comment = "comment"
  val finish = "finish"
  val when = "when"
  val stars = "stars"
  val receiveOrder = "receive"

  val orderTag: PrefixTag = prefixTag(order) _
  val tasteTag: PrefixTag = prefixTag(taste) _
  val powerTag: PrefixTag = prefixTag(power) _
  val commentTag: PrefixTag = prefixTag(comment) _
  val whenTag: PrefixTag = prefixTag(when) _
  val finishTag: PrefixTag = prefixTag(finish) _
  val starsTag: PrefixTag = prefixTag(stars) _

  def receiveOrderTag(orderId: Long) = prefixTag(receiveOrder + orderId.toString + ":") _

  def greetings(name: String) = "*Привет,* *" + name +
    "*! Я бот-кальянщик, который немного упростит тебе твою жизнь, и поможет " +
    "заказать кальян прямо из своего телефона \uD83C\uDF2A"

  // message for start menu
  val start = "Нажмите любую из кнопок, чтобы сделать новый заказ."

  // three main buttons
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
    InlineKeyboardButton.callbackData("✔️Заказ готов!", commentTag("not_need")),
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
      "_Вкус:_ " + order.hookahTaste.getOrElse("") + "\n" +
      "_Сила:_ " + order.hookahPower.getOrElse("") + "\n" +
      "_Дополнительный комментарий:_ " + order.comment.getOrElse("Нет\n")

  onCallbackWithTag(receiveOrder) { implicit cbq => //"receive:1:accept" -- cbq.data == Some(1:accept)
    cbq.message.foreach { msg =>
      cbq.data foreach { data =>
        data.split(":")(1) match {
          case "accept" =>
            orderRepository.getById(data.split(":").head.toLong).onComplete {
              case Success(userOrder) =>
                userOrder.foreach { o =>
                  if (o.isAccepted)
                    request(EditMessageText(Some(msg.source), Some(msg.messageId),
                      text = msg.text.getOrElse("") + "\n\uD83D\uDD34*УЖЕ ПРИНЯТ ДРУГИМ КАЛЬЯНЩИКОМ*",
                      replyMarkup = None,
                      parseMode = Some(ParseMode.Markdown)))
                  else {
                    orderRepository.update(
                      Order(o.guestId, o.hookahId, o.hookahTaste, o.hookahPower, o.time, o.comment, true, o.id))
                    request(EditMessageText(Some(msg.source), Some(msg.messageId),
                      text = msg.text.getOrElse("") + "\n✅*ПРИНЯТ*",
                      replyMarkup = None,
                      parseMode = Some(ParseMode.Markdown)))
                    request(SendMessage(
                      o.guestId,
                      "Ваш заказ (*#" + o.id.toString + "*) был принят! Ожидаем вас в `" + dateFormatter(o.time) + "` ;)",
                      parseMode = Some(ParseMode.Markdown)))
                    context.child(o.guestId.toString).foreach{ _ ! PoisonPill }
                  }
                }
            }
          case "deny" =>
            request(EditMessageText(Some(msg.source), Some(msg.messageId),
              text = msg.text.getOrElse("") + "\n\uD83D\uDEAB*ОТМЕНЁН*",
              replyMarkup = None,
              parseMode = Some(ParseMode.Markdown)))
        }
      }
    }
  }

  onCallbackWithTag(order) { implicit cbq =>
    cbq.message.foreach { implicit msg =>
      userActor(msg.source, Some(cbq.from)) ! StartOrdering(cbq.data.getOrElse("").split(" ").head.toLong)
      request(EditMessageText(Some(msg.source), Some(msg.messageId),
        text = "Кальянная: " + cbq.data.getOrElse("").split(" ").tail.mkString(" ") +
          "\n\uD83D\uDD34\uD83D\uDD34\uD83D\uDD34\uD83D\uDD34*МЕНЮ*\uD83D\uDD34\uD83D\uDD34\uD83D\uDD34\uD83D\uDD34" +
          "\n*Вкус кальяна*:",
        parseMode = Some(ParseMode.Markdown),
        replyMarkup = Some(tasteMarkup)))
    }
  }

  // after pressing on the taste button:
  onCallbackWithTag(taste) { implicit cbq =>
    cbq.message.foreach { implicit msg =>
      context.child(msg.source.toString) foreach {
        _ ! UpdateTaste(cbq.data)
      }
      request(EditMessageText(
        Some(msg.chat.id), Some(msg.messageId),
        text = msg.text.getOrElse("") + cbq.data.getOrElse("") +
          "\n*Жёсткость:*", replyMarkup = Some(powerMarkup), parseMode = Some(ParseMode.Markdown)))
    }
  }

  // ... power button:
  onCallbackWithTag(power) { implicit cbq =>
    cbq.message.foreach { implicit msg =>
      if (cbq.data.contains("back"))
        request(EditMessageText(
          Some(msg.chat.id), Some(msg.messageId),
          text = msg.text.getOrElse("").split('\n').dropRight(2).mkString("\n") + "\n*Вкус кальяна:*",
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
            "\n*Время прибытия:*",
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
          text = msg.text.getOrElse("").split("\n").dropRight(2).mkString("\n") + "\n*Жёсткость:*",
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
            "\n*Дополнительный комментарий для кальянщика(по желанию)*",
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
            text = msg.text.getOrElse("").split("\n").dropRight(2).mkString("\n") + "\n*Время прибытия:*",
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
          context.child(msg.source.toString) foreach { _ ! FinishOrdering(msg.date) }
        }
      case Some("deny") =>
        cbq.message.foreach { msg =>
          context.child(msg.source.toString) foreach {
            _ ! PoisonPill
          }
          request(EditMessageText(Some(msg.chat.id),
            Some(msg.messageId),
            text = "Заказ был отменён.",
            replyMarkup = userMarkup))
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
        visitRepository.create(
          Visit(
            msg.source,
            d.tail.toLong,
            epochToLocalDateTimeConverter(msg.date),
            d.head.toString.toInt
          )
        )
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
      accountRepository.getById(msg.source).onComplete {
        case Success(acc) =>
          acc match {
            case Some(a) =>
              accountRepository.delete(a.id)
              reply("\uD83D\uDD34Вы успешно вышли из системы.",
                replyMarkup = userMarkup)
            case None =>
              reply("\uD83D\uDD34Вы не можете выйти из системы, потому что вы не залогинились.",
                replyMarkup = userMarkup)
          }
      }
  }

  onCommand("/promocode") { implicit msg =>
    accountRepository.getById(msg.source).onComplete {
      case Success(acc) =>
        acc match {
          case Some(a) =>
            hookahRepository.getPromocode(a.hookahId).onComplete {
              case Success(code) =>
                reply("\uD83D\uDD34Промокод: `" + code + "`",
                  replyMarkup = None,
                  parseMode = Some(ParseMode.Markdown))
            }
          case None =>
            reply("\uD83D\uDD34У вас нет прав",
              replyMarkup = userMarkup)
        }
    }
  }

  onMessage { implicit msg =>
    using(_.text) {
      case "Заказать кальян\uD83C\uDF2A" =>
        if (context.child(msg.source.toString).nonEmpty)
          reply("Пока что вы не можете заказать кальян.")
        else
          hookahRepository.getHookahsforUser(msg.source).onComplete {
            case Success(hookahSet) =>
              if (hookahSet.isEmpty)
                reply("К сожалению, вы ещё не пользовались услугами нашего бота." +
                  "Как только вы посетите одну из наших кальянных, вы сможете делать в ней заказы.")
              else
                reply("Выберите заведение из списка:",
                  replyMarkup = Some(InlineKeyboardMarkup.singleColumn(
                    hookahSet.map(s =>
                      InlineKeyboardButton.callbackData(s._2 + " (" + s._3.toString + "⭐️)",
                        orderTag(s._1.toString + " " + s._2))).toSeq)))

          }
      case "Ввести промокод\uD83D\uDD20" =>
        reply("Введите промокод, который сказал вам кальянщик", replyMarkup = Some(ForceReply()))
      case "Посмотреть статистику\uD83D\uDCC8" =>
        visitRepository.getUserStats(msg.source).onComplete {
          case Success(statsSet) =>
            if (statsSet.isEmpty) request(SendMessage(msg.source, "К сожалению, вы еще не пользовались услугами нашего бота."))
            else
              request(SendMessage(msg.source,
                "\uD83D\uDCC8*Статистика*\n\n" + statsSet.map { value =>
                  val count = value._2._2
                  val isFree = count % value._2._3 == 0
                  "_Кальянная:_ " + value._1 + "\n" +
                    "_Средняя оценка:_ " + value._2._1.toString + "\n" +
                    "_Всего посещений:_ " + count + "\n" + {
                    if (isFree) "\uD83D\uDD34*У вас бесплатный кальян!*\n" else ""
                  } +
                    "_Осталось кальянов до бесплатного:_ " + (value._2._3 - (count % value._2._3)).toString + "\n"
                }.mkString("\n"),
                parseMode = Some(ParseMode.Markdown),
                replyMarkup = userMarkup))
        }
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
            request(DeleteMessage(rpl.source, rpl.messageId))
            hookahRepository.checkPromocode(msg.text.getOrElse("")) onComplete {
              case Success(hookah) =>
                if (hookah.nonEmpty)
                  hookah.foreach { h =>
                    hookahRepository.update(
                      Hookah(
                        h.name,
                        generateRandomCode(h.code),
                        h.password,
                        h.freeHookahNumber,
                        h.id
                      ))
                    msg.from.foreach{ u =>
                      guestRepository.create(
                        Guest(
                          u.username,
                          u.firstName,
                          u.lastName,
                          msg.source)).onComplete{
                        case _ =>
                          reply("Вы успешно ввели промокод!\nПоставьте оценку заведению:",
                            replyMarkup = Some(starsMarkup(h.id)))
                      }
                    }
                  }
                else
                  reply("Неправильный промокод!",
                    replyMarkup = userMarkup)
            }
          case Some("Введите пароль, чтобы авторизироваться") =>
            accountRepository.checkPassword(msg.text.getOrElse("")) onComplete {
              case Success(hookah) =>
                hookah match {
                  case Some(h) =>
                    accountRepository.getById(msg.source) onComplete {
                      case Success(acc) =>
                        if (acc.isEmpty) {
                          reply("\uD83D\uDD34Вы успешно вошли в систему",
                            replyMarkup = None)
                          msg.from.foreach { u =>
                            accountRepository.create(Account(h.id, u.firstName, u.username, msg.source))
                          }
                        }
                      else
                      reply("\uD83D\uDD34Вы не можете войти, поскольку вы уже в системе",
                        replyMarkup = None)
                    }
                  case None =>
                    reply("\uD83D\uDD34Неправильный пароль!",
                      replyMarkup = userMarkup)
                }
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
    case SendOrderToEmployees(userOrder) =>
      accountRepository.getAllEmployees(userOrder.hookahId).onComplete {
        case Success(set) =>
          if (set.isEmpty) {
            request(SendMessage(
              userOrder.guestId,
              "К сожалению, сейчас нету свободных кальянщиков, поэтому ваш заказ был отменен." +
                "Попробуйте сделать заказ позже, или обратиться в другое заведение.",
              replyMarkup = userMarkup
            ))
            context.child(userOrder.guestId.toString).foreach {
              _ ! PoisonPill
            }
          }
          else {
            guestRepository.getById(userOrder.guestId).onComplete {
              case Success(guest) =>
                set.foreach { id =>
                  guest.foreach { g =>
                    request(SendMessage(id,
                      orderMessage(userOrder, g),
                      parseMode = Some(ParseMode.Markdown),
                      replyMarkup = receiveOrderMarkup(userOrder.id)))
                  }
                }
            }
          }
      }
    case OrderTimeout(orderId) =>
      orderRepository.getById(orderId).onComplete {
        case Success(userOrder) =>
          userOrder.foreach { o =>
            if (!o.isAccepted) {
              request(SendMessage(o.guestId,
                "Ответа на заказ не было на протяжении 10 минут, поэтому " +
                  "заказ *#" + o.id.toString + "* был отменен." +
                  "Приносим извинения за неудобства.",
                parseMode = Some(ParseMode.Markdown)))
              context.child(o.guestId.toString) foreach { _ ! PoisonPill }
            }
          }
      }
    case _ => Unit
  }
}


case class SendOrderToEmployees(order: Order)

case class OrderTimeout(orderId: Long)

object HookahBotActor {
  def props(): Props = Props(new HookahBotActor())
}