package nz.rd.frolic.entity

final class Field[A](name: Option[String]) {
  override def toString(): String = {
    name match {
      case None => super.toString()
      case Some(n) => s"Field($n)"
    }
  }
}

object Field {
  def apply[A](name: String) = new Field[A](Some(name))
  final case class Get[A](field: Field[A]) extends Message[Unit,A]
  final case class Set[A](field: Field[A]) extends Message[A,Unit]

  def addSimpleFieldToModel[A](field: Field[A], defaultValue: A, entityModel: EntityModel): EntityModel = {
    val slot: Slot[A] = new Slot[A]
    val getHandler = MessageHandler.singleMessage(Get(field)) { (arg: Unit, tx: EntityTransaction) =>
      tx.get[A](slot) match {
        case null => defaultValue
        case v => v
      }
    }
    val setHandler = MessageHandler.singleMessage(Set(field)) { (arg: A, tx: EntityTransaction) =>
        tx.set(slot, arg)
        Some(())
    }
    entityModel.addHandler(getHandler).addHandler(setHandler)
  }

  implicit class EntityWithFields(val entity: Entity) extends AnyVal {
    def get[A](field: Field[A]): A = entity.sendRead(Get(field), ())
    def set[A](field: Field[A], value: A): Entity = entity.sendWrite(Set(field), value)
  }
  implicit class EntityModelWithFields(val entityModel: EntityModel) extends AnyVal {
    def addSimpleField[A](field: Field[A], defaultValue: A): EntityModel = addSimpleFieldToModel[A](field, defaultValue, entityModel)
  }
}