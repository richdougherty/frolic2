package nz.rd.frolic

import io.netty.bootstrap.ServerBootstrap
import io.netty.buffer.Unpooled
import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.handler.codec.http._
import io.netty.handler.logging.{LogLevel, LoggingHandler}
import io.netty.handler.ssl.SslContext
import io.netty.handler.ssl.util.SelfSignedCertificate
import nz.rd.frolic.async.AFunc
import nz.rd.frolic.entity.{Message, _}
import nz.rd.frolic.http._


object Frolic {
//  val textContentField = new Field[Option[String]](Some("textContent"))
//  val messageEntityDef = Field.addSimpleField[Option[String]](textContentField, None, EntityModel.empty)
//}
//
//class Frolic(handler: AFunc[Request, Response]) {

  def start(app: FrolicApp): Unit = {

    import Field._


    val logic: FrolicApp.Logic = {

      val defaultLogic: FrolicApp.Logic = {
        val messageModel = EntityModel.empty.addSimpleField(Message.content, "")
        val defaultModels = new Models(messageModel, messageModel)
        val defaultHandler = new RequestHandler(AFunc.fromFunction { req =>
          defaultModels.emptyResponse.setContent("Hello world!")
        })

        FrolicApp.Logic(
          httpModels = defaultModels,
          requestHandler = defaultHandler
        )
      }

      app.start(defaultLogic)
    }

    def serverHandler = new ChannelHandlerAdapter {

      override def channelReadComplete(ctx: ChannelHandlerContext): Unit = {
        ctx.flush()
      }

      override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = {

        import io.netty.handler.codec.http.HttpHeaderNames._
        import io.netty.handler.codec.http.HttpResponseStatus._
        import io.netty.handler.codec.http.HttpVersion._

        msg match {
          case req: HttpRequest =>
            if (HttpHeaderUtil.is100ContinueExpected(req)) {
              ctx.write(new DefaultFullHttpResponse(HTTP_1_1, CONTINUE))
            }
            val keepAlive = HttpHeaderUtil.isKeepAlive(req)

            val NettyRequest = Field[HttpRequest]("NettyRequest")

            val frolicNettyModels: Models = logic.httpModels.updateRequestModel { rm0 =>
              val rm1 = addSimpleFieldToModel(NettyRequest, null, rm0) // FIXME: Make it an error if not set
              val rm2 = rm1.addHandler(MessageHandler.singleMessage(Get(Request.rawUri)) { (arg: Unit, tx: EntityTransaction) =>
                val nettyRequest: HttpRequest = tx.send(Get(NettyRequest), ())
                nettyRequest.uri
              })
              rm2
            }

            val frolicReq: Request = frolicNettyModels.emptyRequest.mapEntity(_.sendWrite(Set(NettyRequest), req))
            val frolicResp: Response = logic.requestHandler.f.apply(frolicReq)
            val textContent: String = frolicResp.getContent
            val bytes: Array[Byte] = textContent.getBytes("utf8")

            val response: FullHttpResponse = new DefaultFullHttpResponse(HTTP_1_1, OK, Unpooled.wrappedBuffer(bytes))
            response.headers().set(CONTENT_TYPE, "text/plain")
            response.headers().setInt(CONTENT_LENGTH, response.content().readableBytes())

            if (!keepAlive) {
              ctx.write(response).addListener(ChannelFutureListener.CLOSE)
            } else {
              response.headers().set(CONNECTION, HttpHeaderValues.KEEP_ALIVE)
              ctx.write(response)
            }
          case _ =>
            () // Ignore
        }
      }

      override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit =  {
        cause.printStackTrace()
        ctx.close()
      }
    }

    val serverInitializer = new ChannelInitializer[SocketChannel] {

      val sslCtx: SslContext = {
        val ssc = new SelfSignedCertificate()
        SslContext.newServerContext(ssc.certificate(), ssc.privateKey())
      }

      override def initChannel(ch: SocketChannel) {
        val p: ChannelPipeline  = ch.pipeline()
        p.addLast(sslCtx.newHandler(ch.alloc()))
        p.addLast(new HttpServerCodec())
        p.addLast(serverHandler)
      }
    }

    val bossGroup: EventLoopGroup = new NioEventLoopGroup(1)
    val workerGroup: EventLoopGroup = new NioEventLoopGroup()
    try {
      val b = new ServerBootstrap()
      b.option[Integer](ChannelOption.SO_BACKLOG, 1024)
      b.group(bossGroup, workerGroup)
        .channel(classOf[NioServerSocketChannel])
        .handler(new LoggingHandler(LogLevel.INFO))
        .childHandler(serverInitializer)
      val ch: Channel = b.bind(8443).sync().channel()
      println("Server started on port 8443.")
      ch.closeFuture().sync()
    } finally {
      bossGroup.shutdownGracefully()
      workerGroup.shutdownGracefully()
    }
  }

}


