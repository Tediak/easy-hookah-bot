import java.time.{Instant, LocalDateTime}
import java.util.{Calendar, Date, TimeZone}

import HookahBotActor._
import akka.actor.{Actor, ActorRef, ActorSelection, PoisonPill, Props}
import com.bot4s.telegram.methods._
import com.bot4s.telegram.api.declarative.{Callbacks, Commands}
import com.bot4s.telegram.api.{Polling, TelegramBot}
import com.bot4s.telegram.clients.ScalajHttpClient
import com.bot4s.telegram.models._
import datatables._
import model._
import UserActor._

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

  def dateFormatter(date: LocalDateTime): String = date.toLocalTime.toString.dropRight(3)

  val order = "order"
  val taste = "taste"
  val power = "power"
  val comment = "comment"
  val finish = "finish"
  val when = "when"
  val stars = "stars"
  val receiveOrder = "receive"
  val createHookah = "createHookah"
  val delete = "delete"
  val menu = "menu"

  val orderTag: PrefixTag = prefixTag(order)
  val tasteTag: PrefixTag = prefixTag(taste)
  val powerTag: PrefixTag = prefixTag(power)
  val commentTag: PrefixTag = prefixTag(comment)
  val whenTag: PrefixTag = prefixTag(when)
  val finishTag: PrefixTag = prefixTag(finish)
  val starsTag: PrefixTag = prefixTag(stars)
  val createHookahTag: PrefixTag = prefixTag(createHookah)
  val deleteTag: PrefixTag = prefixTag(delete)

  def receiveOrderTag(orderId: Long): String => String = prefixTag(receiveOrder + orderId.toString + ":")

  // message for start menu

  // three main buttons
  val userMarkup = Some(ReplyKeyboardMarkup.singleColumn(
    Seq(
      KeyboardButton.text("Заказать кальян\uD83C\uDF2A"),
      KeyboardButton.text("Ввести промокод\uD83D\uDD20"),
      KeyboardButton.text("Посмотреть статистику\uD83D\uDCC8")),
    oneTimeKeyboard = Some(true),
    resizeKeyboard = Some(true)
  ))

  def addHookahMarkup(implicit info: List[String]) = Some(InlineKeyboardMarkup.singleRow(Seq(
    InlineKeyboardButton.callbackData("Добавить", createHookahTag(info.mkString(";"))),
    InlineKeyboardButton.callbackData("Удалить", deleteTag("add_hookah")))))


  def receiveOrderMarkup(orderId: Long) = Some(InlineKeyboardMarkup.singleRow(
    Seq(
      InlineKeyboardButton.callbackData("✅Принять заказ", receiveOrderTag(orderId)("accept")),
      InlineKeyboardButton.callbackData("\uD83D\uDEABОтвергнуть заказ", receiveOrderTag(orderId)("deny")))
  ))

  val accountMarkup = Some(ReplyKeyboardMarkup.singleButton(
    KeyboardButton.text("\uD83D\uDD20Получить промокод")
  ))

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

  val whenMarkup: InlineKeyboardMarkup = InlineKeyboardMarkup.singleColumn(Seq(
    InlineKeyboardButton.callbackData("\uD83D\uDD5215 минут", whenTag("15")),
    InlineKeyboardButton.callbackData("\uD83D\uDD5530 минут", whenTag("30")),
    InlineKeyboardButton.callbackData("\uD83D\uDD5845 минут", whenTag("45")),
    InlineKeyboardButton.callbackData("\uD83D\uDD5BОдин час", whenTag("60")),
    InlineKeyboardButton.callbackData("⬅️Назад", whenTag("back"))
  ))

  // optional comment button
  val commentMarkup: InlineKeyboardMarkup = InlineKeyboardMarkup.singleColumn(Seq(
    InlineKeyboardButton.callbackData("✉️Добавить комментарий", commentTag("add")),
    InlineKeyboardButton.callbackData("✔️Заказ готов, всё ок!", commentTag("not_need")),
    InlineKeyboardButton.callbackData("⬅️Назад", commentTag("back"))
  ))

  // finishing an order
  val finishMarkup: InlineKeyboardMarkup = InlineKeyboardMarkup.singleRow(Seq(
    InlineKeyboardButton.callbackData("✅Отправить", finishTag("ready")),
    InlineKeyboardButton.callbackData("\uD83D\uDEABОтменить", finishTag("cancel")),
    InlineKeyboardButton.callbackData("➕Буду ещё один!", finishTag("one_more"))))


  def starsMarkup(hookahId: Long): InlineKeyboardMarkup = InlineKeyboardMarkup.singleRow(Seq(
    InlineKeyboardButton.callbackData("1", starsTag("1" + hookahId.toString)),
    InlineKeyboardButton.callbackData("2", starsTag("2" + hookahId.toString)),
    InlineKeyboardButton.callbackData("3", starsTag("3" + hookahId.toString)),
    InlineKeyboardButton.callbackData("4", starsTag("4" + hookahId.toString)),
    InlineKeyboardButton.callbackData("5", starsTag("5" + hookahId.toString))
  ))

  def orderMessage(orders: List[Order], guest: Guest): String =
    "От: " + guest.firstName.markdown("*") + " " + guest.lastName.getOrElse("").markdown("*") +
      " (" + guest.nickname.map("@" + _).getOrElse("без никнейма").markdown("*") + ")\n\n" +
      orders.map { order =>
        "Заказ " + ("#" + order.id.toString).monospaceMarkdown + "\n" +
          "Время прибытия (примерно): " + dateFormatter(order.time) + "\n" +
          "Вкус: " + order.hookahTaste.getOrElse("") + "\n" +
          "Крепкость: " + order.hookahPower.getOrElse("") + "\n" +
          "Дополнительный комментарий: " + order.comment.getOrElse("Нет\n").markdown("_")
      }.mkString("\n\n")


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
                      Order(o.guestId, o.hookahId, o.hookahTaste, o.hookahPower, o.time, o.comment, ACCEPTED, o.id))
                    request(EditMessageText(Some(msg.source), Some(msg.messageId),
                      text = msg.text.getOrElse("") + "\n✅*ПРИНЯТ*",
                      replyMarkup = None,
                      parseMode = Some(ParseMode.Markdown)))
                    request(SendMessage(
                      o.guestId,
                      "Твой заказ (*#" + o.id.toString + "*) был принят! Тебя ждут в `" + dateFormatter(o.time) + "` ;)",
                      parseMode = Some(ParseMode.Markdown)))
                    // 60 minutes!!!!
                    context.child(o.guestId.toString).foreach {
                      _ ! PoisonPill
                    }
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
        text = "Кальянная: " + cbq.data.getOrElse("").split(" ").tail.mkString(" ").markdown("_") +
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
          "\n*Крепкость:*", replyMarkup = Some(powerMarkup), parseMode = Some(ParseMode.Markdown)))
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
            "\n*Дополнительный комментарий для кальянщика(по желанию):*",
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
            text = msg.text.getOrElse("").split("\n").dropRight(2).mkString("\n") + "\n*Крепкость:*",
            replyMarkup = Some(powerMarkup),
            parseMode = Some(ParseMode.Markdown)
          ))
        }
      case Some("add") =>
        cbq.message.foreach { implicit msg =>
          request(EditMessageText(
            Some(msg.chat.id),
            Some(msg.messageId),
            text = msg.text.getOrElse("") + " [с комментарием]",
            replyMarkup = None))
          reply("Чтобы добавить дополнительные пожелания, просто ответь мне реплаем на это сообщение." +
            "Кальянщик постарается воплотить твои самые разные пожелания:)",
            replyMarkup = Some(ForceReply()))
        }
      case Some("not_need") =>
        cbq.message.foreach { implicit msg =>
          request(EditMessageText(Some(msg.chat.id), Some(msg.messageId),
            text = msg.text.getOrElse("") + " [без комментария]"))
          reply("*А сейчас нужно решить, что делаем дальше. Что выберешь ты? ;)*",
            replyMarkup = Some(finishMarkup),
            parseMode = Some(ParseMode.Markdown))
        }
    }
  }

  onCallbackWithTag(when) { implicit cbq =>
    cbq.message.foreach { implicit msg =>
      if (cbq.data.contains("back"))
        request(EditMessageText(
          Some(msg.chat.id), Some(msg.messageId),
          text = "*А сейчас нужно решить, что делаем дальше. Что выберешь ты? ;)*",
          replyMarkup = Some(finishMarkup),
          parseMode = Some(ParseMode.Markdown)
        ))
      else {
        context.child(msg.source.toString) foreach {
          _ ! UpdateWhen(cbq.data)
        }
        request(EditMessageText(
          Some(msg.chat.id),
          Some(msg.messageId),
          text = "*Отлично!* Я отправил твой заказ кальянщикам на обработку, теперь тебе нужно только подождать " +
            "сообщения о подтверждении)",
          replyMarkup = None,
          parseMode = Some(ParseMode.Markdown))) foreach { _ =>
          context.child(msg.source.toString) foreach {
            _ ! FinishOrdering(Instant.now.getEpochSecond.toInt)
          }
        }
      }
    }
  }

  onCallbackWithTag(finish) { implicit cbq =>
    cbq.data match {
      case Some("ready") =>
        cbq.message.foreach { msg =>
          request(EditMessageText(Some(msg.chat.id), Some(msg.messageId),
            text = "*Последний вопрос, через сколько примерно тебя ждать?)*",
            replyMarkup = Some(whenMarkup),
            parseMode = Some(ParseMode.Markdown)))
          //          val currentTime = Instant.now.getEpochSecond.toInt
          //          context.child(msg.source.toString) foreach { _ ! FinishOrdering(currentTime) }
        }
      case Some("cancel") =>
        cbq.message.foreach { msg =>
          context.child(msg.source.toString) foreach {
            _ ! PoisonPill
          }
          request(EditMessageText(
            Some(msg.chat.id),
            Some(msg.messageId),
            text = "_Заказ был отменён_",
            replyMarkup = None,
            parseMode = Some(ParseMode.Markdown))) foreach { either =>
            either.foreach { msg =>
              request(EditMessageText(
                Some(msg.chat.id),
                Some(msg.messageId),
                text = msg.text.getOrElse("").markdown("_"),
                replyMarkup = userMarkup
              ))
            }
          }
        }
      case Some("one_more") =>
        cbq.message.foreach { msg =>
          request(EditMessageText(
            Some(msg.source),
            Some(msg.messageId),
            text =
              s"""
                 |💨💨💨💨💨💨
                 |*+1 кальян*
                 |
                 |*Вкус кальяна:*
                 """.stripMargin,
            replyMarkup = Some(tasteMarkup),
            parseMode = Some(ParseMode.Markdown)))
          context.child(msg.source.toString) foreach {
            _ ! OneMoreOrder
          }
        }
    }
  }

  onCallbackWithTag(stars) { implicit cbq =>
    cbq.message.foreach { msg =>
      request(EditMessageText(Some(msg.source), Some(msg.messageId),
        text = "Спасибо за визит!",
        replyMarkup = userMarkup, /*Some(
          InlineKeyboardMarkup.singleButton(
            InlineKeyboardButton.callbackData("Главное меню", "menu")
          )*/
        parseMode = Some(ParseMode.Markdown)))
      ackCallback(Some("Оценка отправлена"))
      cbq.data.foreach { d =>
        visitRepository.create(
          Visit(
            msg.source,
            d.tail.toLong,
            Instant.now.getEpochSecond.toInt.toLocalDateTime,
            d.head.toString.toInt
          )
        )
      }
    }
  }

  onCallbackWithTag(createHookah) { cbq =>
    cbq.data.foreach { info =>
      val free :: pass :: name :: Nil = info.split(";").toList
      hookahRepository.addNew(name, pass, free.toInt) onComplete {
        case Success((_, id)) =>
          cbq.message.foreach { msg =>
            request(EditMessageText(
              Some(msg.chat.id),
              Some(msg.messageId),
              text = s"Кальянная ${name.markdown("*")} была успешно создана id:${id.toString.monospaceMarkdown}",
              replyMarkup = userMarkup,
              parseMode = Some(ParseMode.Markdown)
            ))
          }
        case Failure(e) =>
          cbq.message.foreach { msg =>
            request(EditMessageText(
              Some(msg.source),
              Some(msg.messageId),
              text =
                s"""
                   |Ошибка создания кальянной
                   |${e.getMessage.monospaceMarkdown}
               """.stripMargin,
              parseMode = Some(ParseMode.Markdown),
              replyMarkup = Some(InlineKeyboardMarkup
                .singleButton(InlineKeyboardButton.callbackData("Закрыть", deleteTag("add_error"))))))
          }
      }
    }
  }

  onCallbackWithTag(delete) {
    _.message.foreach { msg => request(DeleteMessage(msg.chat.id, msg.messageId)) }
  }

  onCallbackWithTag(menu) {
    _.message.foreach { implicit msg =>
      request(DeleteMessage(msg.source, msg.messageId)) foreach { _ =>
        reply("_Главное меню_", replyMarkup = userMarkup, parseMode = Some(ParseMode.Markdown))
      }
    }
  }

  /* MESSAGE HANDLER */

  onCommand("/start") { implicit msg =>
    msg.from foreach (u => println(u.firstName))
    reply(greetings(msg.from.map(_.firstName).getOrElse("Неизвестный")),
      replyMarkup = userMarkup,
      parseMode = Some(ParseMode.Markdown))
  }

  onCommand("/login") { implicit msg =>
    reply("_Введи пароль, чтобы авторизироваться_",
      replyMarkup = Some(ForceReply()),
      parseMode = Some(ParseMode.Markdown))
  }

  onCommand("/logout") { implicit msg =>
    accountRepository.getById(msg.source).onComplete {
      case Success(acc) =>
        acc match {
          case Some(a) =>
            accountRepository.delete(a.id)
            reply("\uD83D\uDD34_Ты успешно вышел из системы._",
              replyMarkup = userMarkup,
              parseMode = Some(ParseMode.Markdown))
          case None =>
            reply("\uD83D\uDD34_Ты не можешь выйти из системы, потому что ты не залогинился._",
              parseMode = Some(ParseMode.Markdown))
        }
    }
  }

  onCommand("/menu") { implicit msg =>
    reply("_Главное меню_",
      replyMarkup = userMarkup,
      parseMode = Some(ParseMode.Markdown))
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
            reply("\uD83D\uDD34_У тебя нет прав_",
              replyMarkup = userMarkup,
              parseMode = Some(ParseMode.Markdown))
        }
    }
  }

  onCommand("/admin") { implicit msg =>
    val admins = Source.fromFile("admins").mkString.split("\n").map(_.toLong)
    admins.find(_ == msg.source).foreach { _ =>
      withArgs { args =>
        args.toList match {
          case "add" :: newHookah =>
            implicit val info@promo :: pass :: hookahName :: Nil =
              List(
                newHookah.reverse.head,
                newHookah.reverse(1).markdown("*"),
                newHookah.dropRight(2).mkString(" ").markdown("*")
              )
            if (promo.forall(_.isDigit) && info.forall(_.nonEmpty)) {
              reply(
                s"""
                   |🔴*Имя:* $hookahName
                   |🔴*Пароль:* $pass
                   |🔴_Номер бесплатного кальяна:_ *${promo.toInt}*
                   |""".stripMargin,
                replyMarkup = addHookahMarkup,
                parseMode = Some(ParseMode.Markdown))
            }
            else
              reply("Неправильные данные _(должно быть минимум 3 аргумента;" +
                "последним аргументом должно быть число)_",
                parseMode = Some(ParseMode.Markdown))
          case "ls" :: Nil =>
            db.run(hookahRepository.hookahTable.result).foreach { table =>
              reply(
                s"""
                   |*СПИСОК ВСЕХ КАЛЬЯННЫХ*
                   |`ID|ИМЯ|ПАРОЛЬ|КОД|БОНУС|`
                   |${
                  table.sortBy(_.id).map { h =>
                    s"`[${"0" * (2 - h.id.toString.length) + h.id}]`" +
                      s"**${h.name}**|pass:${h.password.monospaceMarkdown}" +
                      s"|code:${h.code.monospaceMarkdown} " +
                      s"|*(${h.freeHookahNumber})*"
                  }.mkString("\n")
                }""".stripMargin,
                parseMode = Some(ParseMode.Markdown),
                replyMarkup = Some(InlineKeyboardMarkup.singleButton(
                  InlineKeyboardButton.callbackData("Удалить", deleteTag("list"))
                )))
            }
          case "pass" :: id :: newPassword :: Nil =>
            hookahRepository.getById(id.toLong).onComplete {
              case Success(option) =>
                option match {
                  case None =>
                    reply("Кальянной с таким id не найдено.", replyMarkup = userMarkup)
                  case Some(hookah) =>
                    hookahRepository.update(hookah.changePassword(newPassword))
                      .foreach { _ => reply("Пароль был успешно изменён.") }
                }
              case Failure(exception) =>
                reply(
                  s"""
                     |Ошибка при смене пароля
                     |${exception.getMessage.monospaceMarkdown}
                  """.stripMargin,
                  parseMode = Some(ParseMode.Markdown),
                  replyMarkup = userMarkup)
            }

        }
      }
    }
  }

  onMessage { implicit msg =>
    using(_.text) {
      case "Заказать кальян\uD83C\uDF2A" =>
        if (context.child(msg.source.toString).nonEmpty)
          reply("_Пока что ты не можешь заказать кальян :( _",
            parseMode = Some(ParseMode.Markdown))
        else
          hookahRepository.getHookahsforUser(msg.source).onComplete {
            case Success(hookahSet) =>
              if (hookahSet.isEmpty) {
                reply("_К сожалению, ты ещё не посетил ни одну из кальянных, в которых я работаю :(" +
                  "Как только посетишь — сможешь делать в ней заказы._", parseMode = Some(ParseMode.Markdown))
                context.child(msg.source.toString).foreach {
                  _ ! PoisonPill
                }
              }
              else
                reply("*Выбери кальянную ниже*",
                  replyMarkup = Some(InlineKeyboardMarkup.singleColumn(
                    hookahSet.toSeq.sortWith(_._3 > _._3).map(s =>
                      InlineKeyboardButton.callbackData(s._2 + " (" + s._3.toString.take(3) + "⭐️)",
                        orderTag(s._1.toString + " " + s._2))))),
                  parseMode = Some(ParseMode.Markdown))

          }

      case "Ввести промокод\uD83D\uDD20" =>
        reply("_Введи промокод, который сказал твой кальянщик_",
          replyMarkup = Some(ForceReply()),
          parseMode = Some(ParseMode.Markdown))

      case "Посмотреть статистику\uD83D\uDCC8" =>
        visitRepository.getUserStats(msg.source).map(_.toList) foreach {
          case Nil =>
            request(
              SendMessage(
                msg.source,
                "_К сожалению, ты еще не посещал кальянные, где я начал работать :(_",
                parseMode = Some(ParseMode.Markdown)))
          case lst =>
            reply(
              "\uD83D\uDCC8*Статистика*\n\n" +
                lst.map { case (name, (avg, count, promo)) =>
                  val isFree = count % promo == 0
                  "_Кальянная:_ " + name.markdown("*") + "\n" +
                    "_Средняя оценка:_ " + avg.toString.take(3).markdown("*") + "⭐️\n" +
                    "_Всего посещений:_ " + count.toString.monospaceMarkdown + "\n" + {
                    if (isFree) "\uD83D\uDD34*У тебя здесь бесплатный кальян!*\n" else ""
                  } + "_Осталось кальянов до бесплатного:_ " + (promo - (count % promo)).toString + "\n"
                }.mkString("\n"),
              parseMode = Some(ParseMode.Markdown),
              replyMarkup = userMarkup)
        }
      case other =>
        if (!other.startsWith("/") && msg.replyToMessage.isEmpty)
          reply("Извини, не понимаю тебя...", parseMode = Some(ParseMode.Markdown))
    }
    using(_.replyToMessage) {
      rpl =>
        rpl.text match {
          case Some("Чтобы добавить дополнительные пожелания, просто ответь мне реплаем на это сообщение.Кальянщик постарается воплотить твои самые разные пожелания:)") =>
            userActor(msg.source, msg.from) ! UpdateComment(msg.text)
            request(DeleteMessage(msg.chat.id, msg.replyToMessage.map(_.messageId).getOrElse(0)))
            reply("Комментарий принят. Что будем делать с заказом?)",
              replyMarkup = Some(finishMarkup),
              parseMode = Some(ParseMode.Markdown))
          case Some("Введи промокод, который сказал твой кальянщик") =>
            request(DeleteMessage(rpl.source, rpl.messageId))
            hookahRepository.checkPromocode(msg.text.getOrElse("")) onComplete {
              case Success(hookah) =>
                if (hookah.nonEmpty)
                  hookah.foreach { h =>
                    hookahRepository.update(h updatePromocode())
                    msg.from.foreach { u =>
                      guestRepository.create(
                        Guest(
                          u.username,
                          u.firstName,
                          u.lastName,
                          msg.source)).onComplete { _ =>
                        reply("_Твой промокод успешно засчитан!_" +
                          "Поставь оценку заведению (и !обязательно! обьективную)",
                          replyMarkup = Some(starsMarkup(h.id)),
                          parseMode = Some(ParseMode.Markdown))
                      }
                    }
                  }
                else
                  reply("Этот промокод неправильный!",
                    replyMarkup = userMarkup,
                    parseMode = Some(ParseMode.Markdown)
                  )
            }
          case Some("Введи пароль, чтобы авторизироваться") =>
            request(DeleteMessage(rpl.source, rpl.messageId))
            accountRepository.checkPassword(msg.text.getOrElse("")) onComplete {
              case Success(hookah) =>
                hookah match {
                  case Some(h) =>
                    accountRepository.getById(msg.source) onComplete {
                      case Success(acc) =>
                        if (acc.isEmpty) {
                          reply("\uD83D\uDD34_Ты успешно вошёл в систему_",
                            parseMode = Some(ParseMode.Markdown),
                            replyMarkup = None)

                          msg.from.foreach { u =>
                            accountRepository.create(Account(h.id, u.firstName, u.username, msg.source))
                          }
                        }
                        else
                          reply("\uD83D\uDD34_Ты не можешь войти, поскольку ты уже авторизовался_",
                            replyMarkup = None,
                            parseMode = Some(ParseMode.Markdown))
                    }
                  case None =>
                    reply("\uD83D\uDD34_Неправильный пароль!_",
                      replyMarkup = userMarkup,
                      parseMode = Some(ParseMode.Markdown))
                }
            }
          case other =>
            other.foreach { o =>
              if (!o.startsWith("/")) reply("Извини, не понимаю тебя....", parseMode = Some(ParseMode.Markdown))
            }
        }
    }
  }

  override def preStart(): Unit = {
    run()
  }

  /* RECEIVE MESSAGES FROM OTHER ACTORS */

  override def receive: Receive = {
    case SendOrderToEmployees(orders) =>
      accountRepository.getAllEmployees(orders.head.hookahId).onComplete {
        case Success(set) =>
          if (set.isEmpty) {
            request(SendMessage(
              orders.head.guestId,
              "К сожалению, сейчас нету свободных кальянщиков _(как такое вообще может быть?!)_, поэтому твой заказ был отменен." +
                "Попробуй сделать заказ позже, или в другой кальянной.",
              replyMarkup = userMarkup,
              parseMode = Some(ParseMode.Markdown)
            ))
            context.child(orders.head.guestId.toString).foreach {
              _ ! PoisonPill
            }
          }
          else {
            val userId = orders.head.guestId
            val ordersId = orders.map(_.id).toSet
            val mainOrder = orders.head
            guestRepository.getById(userId).onComplete {
              case Success(guest) =>
                set.foreach { id =>
                  guest.foreach { g =>
                    request(SendMessage(id,
                      orderMessage(orders, g),
                      parseMode = Some(ParseMode.Markdown),
                      replyMarkup = receiveOrderMarkup(ordersId.head)))
                  }
                }

                context.system.scheduler.scheduleOnce(delay = 10 minutes, receiver = self, message = OrderTimeout(mainOrder.id))
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
                  "Приношу извинения за неудобства, надеюсь, такого больше не повторится!",
                parseMode = Some(ParseMode.Markdown)))
              context.child(o.guestId.toString) foreach {
                _ ! PoisonPill
              }
            }
          }
      }
    case _ => Unit
  }
}


object HookahBotActor {
  def props(): Props = Props(new HookahBotActor())

  case class SendOrderToEmployees(orders: List[Order])

  case class OrderTimeout(orderId: Long)

}