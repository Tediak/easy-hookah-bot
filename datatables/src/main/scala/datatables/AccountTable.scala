package datatables

import model.{Account, Hookah}
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Tag

import scala.concurrent.{ExecutionContext, Future}

class AccountTable(tag: Tag) extends Table[Account](tag, "accounts"){
  val id = column[Long]("id", O.PrimaryKey)
  val hookahId = column[Long]("hookah_id")
  val name = column[String]("name")
  val nickname = column[Option[String]]("nickname")

  val hookahIdFk = foreignKey("hookah_id_fk", hookahId, HookahTable.table)(_.id, ForeignKeyAction.Cascade)

  def * = (hookahId, name, nickname, id) <> (Account.apply _ tupled, Account.unapply)
}

object AccountTable {
  val table = TableQuery[AccountTable]
}

class AccountRepository(db: Database) {
  val accountTable = AccountTable.table

  def create(account: Account): Future[Account] =
    db.run(accountTable returning accountTable += account)

  def update(account: Account): Future[Int] =
    db.run(accountTable.filter(_.id === account.id).update(account))

  def delete(accountId: Long): Future[Int] =
    db.run(accountTable.filter(_.id === accountId).delete)

  def getById(accountId: Long): Future[Option[Account]] =
    db.run(accountTable.filter(_.id === accountId).result.headOption)

  def getAllEmployees(hookahId: Long)(implicit ec: ExecutionContext): Future[Set[Long]] =
    db.run(accountTable.filter(_.hookahId === hookahId).map(_.id).result).map(_.toSet)

  def checkPassword(pass: String)(implicit ec: ExecutionContext): Future[Option[Hookah]] =
    db.run((for{
      hookah <- HookahTable.table if hookah.password === pass
    } yield hookah).result).map(_.headOption)

//  def updateByUser(login : String, isAuthorized : Boolean) =
//    db.run(accountTable.filter(x=> x.login === login).map(x=> x.isLogined).update(isAuthorized))
  }
