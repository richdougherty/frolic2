package nz.rd.frolic.http

import nz.rd.frolic.entity.Entity

class Response(entity: Entity) extends Message(entity) {

  override protected type NewInstance = Response

  override protected def newInstance(newEntity: Entity): Response = new Response(newEntity)

}