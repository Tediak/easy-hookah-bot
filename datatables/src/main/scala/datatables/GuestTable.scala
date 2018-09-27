package datatables

import model.Guest
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Tag

import scala.concurrent.Future

class GuestTable(tag: Tag) extends Table[Guest](tag, "guests"){
  val id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  val nickname = column[String]("nickname")
  val firstName = column[String]("first_name")
  val lastName = column[Option[String]]("last_name")

  def * = (nickname, firstName, lastName, id) <> (Guest.apply _ tupled, Guest.unapply)
}

object GuestTable {
  val table = TableQuery[GuestTable]
}

class GuestRepository(db: Database) {
  val guestTable = GuestTable.table

  def create(guest: Guest): Future[Guest] =
    db.run(guestTable returning guestTable += guest)

  def update(guest: Guest): Future[Int] =
    db.run(guestTable.filter(_.id === guest.id).update(guest))

  def delete(guestId: Long): Future[Int] =
    db.run(guestTable.filter(_.id === guestId).delete)

  def getById(guestId: Long): Future[Option[Guest]] =
    db.run(guestTable.filter(_.id === guestId).result.headOption)
}
