import java.time.{Instant, LocalDateTime}
import java.util.TimeZone

import scala.util.Random

package object model {

  val NOT_ACCEPTED = false
  val ACCEPTED = true

  case class Hookah(
                     name: String,
                     code: String,
                     password: String,
                     freeHookahNumber: Int,
                     id: Long = 0L
                   ) {

    def updatePromocode(): Hookah  = {
      def newCode(c: String): String = Some(
        code.take(3) +
          Random.nextInt(10) +
          Random.nextInt(10))
        .filterNot(_ == c).getOrElse(newCode(c))

      Hookah(
        name,
        newCode(code),
        password,
        freeHookahNumber,
        id
      )
    }

      def changePassword(newPass: String): Hookah = Hookah(name, code, newPass, freeHookahNumber, id)
    }

  case class Account(
                      hookahId: Long,
                      name: String,
                      nickname: Option[String],
                      id: Long
                    )

  case class Guest(
                    nickname: Option[String],
                    firstName: String,
                    lastName: Option[String],
                    id: Long
                  )

  case class Visit(
                    guestId: Long,
                    hookahId: Long,
                    time: LocalDateTime,
                    stars: Int,
                    id: Long = 0L
                  )

  case class Order(
                    guestId: Long,
                    hookahId: Long,
                    hookahTaste: Option[String],
                    hookahPower: Option[String],
                    time: LocalDateTime,
                    comment: Option[String] = None,
                    isAccepted: Boolean = false,
                    id: Long = 0L
                  ) {
    def updateGuestId(newGuestId: Long): Order = this.copy(guestId = newGuestId)
  }

  val markupSymbols = Set('_', '`', '*')

  implicit class StringMarkdownFormatter(val string: String) {
    def markdown(markdownSymbol: String): String = {
      if(string.forall(!markupSymbols.contains(_))) markdownSymbol + string + markdownSymbol
      else
        markdownSymbol * 2 +
          string.map(c => markupSymbols.find(_ == c).map("\\" + _).getOrElse(c)).mkString +
          markdownSymbol * 2
    }

    val monospaceMarkdown: String =
      if(string.forall(_ != '`')) "`" + string + "`"
      else "``" + string.map(c => markupSymbols.find(_ == c).map("\\" + _).getOrElse(c)).mkString  + "``"
  }

  implicit class IntegerToLocalDateTime(i: Int) {
    def toLocalDateTime: LocalDateTime =
      LocalDateTime
        .ofInstant(
          Instant.ofEpochSecond(i),
          TimeZone.getDefault.toZoneId)
  }

//  implicit class ArgsToHookahInfoFormatter(args: Seq[String]) {
//    def getHookahInfo: List[String] = {
//      val revargs.reverse.toList
//  }


  def greetings(name: String): String = "*Привет,* " + name.markdown("*") +
    "! Я бот-кальянщик, который немного упростит тебе твою жизнь, и поможет " +
    "заказать кальян прямо из своего телефона \uD83C\uDF2A"
}