package nz.rd.frolic

import io.netty.bootstrap.ServerBootstrap
import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.handler.codec.http._
import io.netty.handler.ssl.util.SelfSignedCertificate
import io.netty.handler.ssl.{SslContext, SslContextBuilder}
import nz.rd.frolic.async.Continuation
import nz.rd.frolic.async.Task
import nz.rd.frolic.async.Task.{Do, Throw}

import Continuation.-->

object Frolic {

  def start(f: (ChannelHandlerContext, HttpRequest) --> HttpResponse): Unit = {

    def serverHandler = new ChannelInboundHandlerAdapter {

      override def channelReadComplete(ctx: ChannelHandlerContext): Unit = {
        ctx.flush()
      }

      override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = {

        import io.netty.handler.codec.http.HttpHeaderNames._
        import io.netty.handler.codec.http.HttpResponseStatus._
        import io.netty.handler.codec.http.HttpVersion._

        msg match {
          case req: HttpRequest =>
            if (HttpUtil.is100ContinueExpected(req)) {
              ctx.write(new DefaultFullHttpResponse(HTTP_1_1, CONTINUE))
            }
            val keepAlive = HttpUtil.isKeepAlive(req)
            val t: Task[HttpResponse] = Do(() => f(req))
            Task.run(t.sequence {
              case Task.Return(response) =>
                if (!keepAlive) {
                  ctx.write(response).addListener(ChannelFutureListener.CLOSE)
                } else {
                  response.headers().set(CONNECTION, HttpHeaderValues.KEEP_ALIVE)
                  ctx.write(response)
                  ctx.flush()
                }
                Task.Return.Unit
              case Throw(cause) =>
                System.err.println("Failure handling request")
                cause.printStackTrace()
                ctx.close()
                Task.Return.Unit
            })
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
        SslContextBuilder.forServer(ssc.certificate(), ssc.privateKey()).build()
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


