import java.time.{LocalDateTime}
import slick.jdbc.PostgresProfile.api._

package object datatables {

  implicit val dataTimeStringMapper = MappedColumnType.base[LocalDateTime, String](
    (l: LocalDateTime) => l.toString,
    (s: String) => LocalDateTime.parse(s)
  )
}
