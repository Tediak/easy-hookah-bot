package datatables

import java.time.LocalDateTime

import model.Visit
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Tag

import scala.concurrent.{ExecutionContext, Future}

class VisitTable(tag: Tag) extends Table[Visit](tag, "visits"){
  val id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  val guestId = column[Long]("guest_id")
  val hookahId = column[Long]("hookah_id")
  val time = column[LocalDateTime]("time")
  val stars = column[Int]("stars")

  val guestIdFk = foreignKey("guest_id_fk", guestId, GuestTable.table)(_.id, ForeignKeyAction.Cascade)

  def * = (guestId, hookahId, time, stars, id) <> (Visit.apply _ tupled, Visit.unapply)
}

object VisitTable {
  val table = TableQuery[VisitTable]
}

class VisitRepository(db: Database) {
  val visitTable = VisitTable.table

  def create(visit: Visit): Future[Visit] = {
    db.run(visitTable returning visitTable += visit)
  }

  def update(visit: Visit): Future[Int] =
    db.run(visitTable.filter(_.id === visit.id).update(visit))

  def delete(visitId: Long): Future[Int] =
    db.run(visitTable.filter(_.id === visitId).delete)

  def getById(visitId: Long): Future[Option[Visit]] =
    db.run(visitTable.filter(_.id === visitId).result.headOption)

  def getUserStats(userId: Long)(implicit ec: ExecutionContext): Future[Set[(String, (Double, Int, Int))]] =
    db.run((for{
      visit <- visitTable if visit.guestId === userId
      hookah <- HookahTable.table if hookah.id === visit.hookahId
    } yield (hookah, visit))
        .map(v => (v._1.id, v._1.name, v._1.freeHookahNumber, v._2.stars))
      .result)
    .map(_
      .groupBy(_._2)
    .mapValues{ visit =>
      val avgStar = visit.map(_._4).sum.toDouble / visit.size
      val freeHookahNumber = visit.map(_._3).head
      (avgStar, visit.size, freeHookahNumber)
    }.toSet)
}
