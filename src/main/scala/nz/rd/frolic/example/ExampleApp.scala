package nz.rd.frolic.example

import nz.rd.frolic.FrolicApp
import nz.rd.frolic.async.AFunc
import nz.rd.frolic.entity.Field.EntityWithFields
import nz.rd.frolic.http.{Response, Message, RequestHandler}

object ExampleApp extends FrolicApp {
  override def start(defaultLogic: FrolicApp.Logic): FrolicApp.Logic = {
    defaultLogic.copy(
      requestHandler = new RequestHandler(AFunc.fromFunction { req =>
        if (req.rawPath == "/") {
          new Response(defaultLogic.httpModels.emptyResponse.entity.set(Message.content, s"Hello world?: ${req.rawPath}"))
        } else {
          new Response(defaultLogic.httpModels.emptyResponse.entity.set(Message.content, s"Unknown path: ${req.rawPath}"))
        }
      })
    )
  }
}