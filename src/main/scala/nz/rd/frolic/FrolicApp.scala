package nz.rd.frolic

import nz.rd.frolic.http.{RequestHandler, Models}

trait FrolicApp {
  def start(defaultLogic: FrolicApp.Logic): FrolicApp.Logic // TODO: AFunc
  def main(args: Array[String]): Unit = Frolic.start(this)
}

object FrolicApp {
  case class Logic(
                  httpModels: Models,
                  requestHandler: RequestHandler
                  )
}
