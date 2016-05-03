package nz.rd.frolic.http

import nz.rd.frolic.entity.EntityModel

final class Models(
              val requestModel: EntityModel,
              val responseModel: EntityModel
            ) {
  def updateRequestModel(f: EntityModel => EntityModel): Models = new Models(f(requestModel), responseModel)
  def updateResponseModel(f: EntityModel => EntityModel): Models = new Models(requestModel, f(responseModel))
  def updateModels(f: EntityModel => EntityModel): Models = new Models(f(requestModel), f(responseModel))

  final def emptyRequest: Request = new Request(requestModel.create)
  final def emptyResponse: Response = new Response(responseModel.create)
}
