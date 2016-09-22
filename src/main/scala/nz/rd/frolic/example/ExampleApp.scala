package nz.rd.frolic.example

import io.netty.buffer.Unpooled
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.http.HttpHeaderNames._
import io.netty.handler.codec.http.HttpResponseStatus._
import io.netty.handler.codec.http.HttpVersion._
import io.netty.handler.codec.http.{DefaultFullHttpResponse, FullHttpResponse, HttpRequest}
import nz.rd.frolic.Frolic
import nz.rd.frolic.async.Task

object ExampleApp {
  def main(args: Array[String]): Unit = {
    Frolic.start(Task.f { (ctx: ChannelHandlerContext, req: HttpRequest) =>
      val response: FullHttpResponse = new DefaultFullHttpResponse(
        HTTP_1_1, OK, Unpooled.wrappedBuffer("Hello world".getBytes("utf-8"))
      )
      response.headers().set(CONTENT_TYPE, "text/plain")
      response.headers().setInt(CONTENT_LENGTH, response.content().readableBytes())
      Task.Return(response)
    })
  }
}