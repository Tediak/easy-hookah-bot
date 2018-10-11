package datatables

import java.time.LocalDateTime

import model.Order
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Tag

import scala.concurrent.Future

class OrderTable(tag: Tag) extends Table[Order](tag, "orders"){
  val id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  val guestId = column[Long]("guest_id")
  val hookahId = column[Long]("hookah_id")
  val hookahTaste = column[Option[String]]("hookah_taste")
  val hookahPower = column[Option[String]]("hookah_power")
  val time = column[LocalDateTime]("time")
  val comment = column[Option[String]]("comment")
  val isAccepted = column[Boolean]("is_accepted")

  val guestIdFk = foreignKey("guest_id_fk", guestId, GuestTable.table)(_.id, ForeignKeyAction.Cascade)
  val hookahIdFk = foreignKey("hookah_id_fk", hookahId, HookahTable.table)(_.id, ForeignKeyAction.Cascade)

  def * = (guestId, hookahId, hookahTaste, hookahPower, time, comment, isAccepted, id) <> (Order.apply _ tupled, Order.unapply)
}

object OrderTable {
  val table = TableQuery[OrderTable]
}

class OrderRepository(db: Database) {
  val orderTable = OrderTable.table

  def create(order: Order): Future[Order] =
    db.run(orderTable returning orderTable += order)

  def update(order: Order): Future[Int] =
    db.run(orderTable.filter(_.id === order.id).update(order))

  def delete(orderId: Long): Future[Int] =
    db.run(orderTable.filter(_.id === orderId).delete)

  def getById(orderId: Long): Future[Option[Order]] =
    db.run(orderTable.filter(_.id === orderId).result.headOption)
}
