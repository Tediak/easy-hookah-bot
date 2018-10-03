package datatables

import model.Hookah
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Tag

import scala.concurrent.Future

class HookahTable(tag: Tag) extends Table[Hookah](tag, "hookahs"){
  val id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  val name = column[String]("name", O.Unique)
  val code = column[String]("code", O.Unique)
  val password = column[String]("password", O.Unique)

  def * = (name, code, password, id) <> (Hookah.apply _ tupled, Hookah.unapply)
}

object HookahTable {
  val table = TableQuery[HookahTable]
}

class HookahRepository(db: Database) {
  val hookahTable = HookahTable.table

  def create(hookah: Hookah): Future[Hookah] =
    db.run(hookahTable returning hookahTable += hookah)

  def update(hookah: Hookah): Future[Int] =
    db.run(hookahTable.filter(_.id === hookah.id).update(hookah))

  def delete(hookahId: Long): Future[Int] =
    db.run(hookahTable.filter(_.id === hookahId).delete)

  def getById(hookahId: Long): Future[Option[Hookah]] =
    db.run(hookahTable.filter(_.id === hookahId).result.headOption)
}
