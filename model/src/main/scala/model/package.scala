import java.time.LocalDateTime

package object model {

  case class Hookah(
                     name: String,
                     code: String,
                     id: Long = 0L
                   )

  case class Account(
                      hookahId: Long,
                      login: String,
                      password: String,
                      isLogined: Boolean,
                      id: Long = 0L
                    )

  case class Guest(
                    nickname: String,
                    firstName: String,
                    lastName: Option[String],
                    id: Long = 0L
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
                    hookahTaste: String,
                    hookahPower: String,
                    time: LocalDateTime,
                    comment: Option[String] = None,
                    id: Long = 0L
                  )
}