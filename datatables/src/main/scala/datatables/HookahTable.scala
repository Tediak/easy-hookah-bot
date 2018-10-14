package datatables

import model.Hookah
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Tag

import scala.concurrent.{ExecutionContext, Future}

class HookahTable(tag: Tag) extends Table[Hookah](tag, "hookahs"){
  val id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  val name = column[String]("name", O.Unique)
  val code = column[String]("code", O.Unique)
  val password = column[String]("password", O.Unique)
  val freeHookahNumber = column[Int]("free_hookah")

  def * = (name, code, password, freeHookahNumber ,id) <> (Hookah.apply _ tupled, Hookah.unapply)
}

object HookahTable {
  val table = TableQuery[HookahTable]
}

class HookahRepository(db: Database) {
  val hookahTable = HookahTable.table

  def create(hookah: Hookah): Future[Hookah] =
    db.run(hookahTable returning  hookahTable.map(_.id) into ((hookahTable,id) => hookahTable.copy (id = id)) += hookah)

  def update(hookah: Hookah): Future[Int] =
    db.run(hookahTable.filter(_.id === hookah.id).update(hookah))

  def delete(hookahId: Long): Future[Int] =
    db.run(hookahTable.filter(_.id === hookahId).delete)

  def getById(hookahId: Long): Future[Option[Hookah]] =
    db.run(hookahTable.filter(_.id === hookahId).result.headOption)

  def findPassword(pass: String)(implicit ec: ExecutionContext): Future[Set[Hookah]] =
    db.run((for {
      hookah <- hookahTable if hookah.password === pass
    } yield hookah).result).map(_.toSet)

  def getHookahsforUser(id: Long)(implicit ec: ExecutionContext): Future[Set[(Long, String)]] =
    db.run((for {
      visit <- VisitTable.table if visit.guestId === id
      hookah <- hookahTable if hookah.id === visit.hookahId
    } yield (hookah, visit.stars)).result).map(
      _.groupBy(_._1)
        .mapValues { value =>
          val sum = value.map(_._2).sum
          val avg = sum.toDouble / value.length
          sum.toDouble / value.length
        }
        .toVector.sortBy(_._2)
        .map(v => (v._1.id, v._1.name))
        .toSet)
}
