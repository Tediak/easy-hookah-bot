package datatables

import model.Account
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Tag

import scala.concurrent.Future

class AccountTable(tag: Tag) extends Table[Account](tag, "accounts"){
  val id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  val hookahId = column[Long]("hookah_id")
  val login = column[String]("login", O.Unique)
  val password = column[String]("password")
  val isLogined = column[Boolean]("is_logined")

  val hookahIdFk = foreignKey("hookah_id_fk", hookahId, HookahTable.table)(_.id, ForeignKeyAction.Cascade)

  def * = (hookahId, login, password, isLogined, id) <> (Account.apply _ tupled, Account.unapply)
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

  def updateByUser(login : String, password: String, isAuthorized : Boolean) =
    db.run(accountTable.filter(x=> x.login === login && x.password === password).map(x=> x.isLogined).update(true))
  }
