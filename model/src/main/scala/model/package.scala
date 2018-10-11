import java.time.LocalDateTime

package object model {

  case class Hookah(
                     name: String,
                     code: String,
                     password: String,
                     id: Long = 0L
                   )

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
                  )
}