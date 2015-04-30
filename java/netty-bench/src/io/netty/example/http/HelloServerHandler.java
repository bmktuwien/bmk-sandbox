package io.netty.example.http;

import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.ChannelFutureListener;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.SimpleChannelInboundHandler;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.handler.codec.http.HttpHeaders;
import io.netty.handler.codec.http.HttpRequest;
import io.netty.handler.codec.http.HttpResponseStatus;
import io.netty.handler.codec.http.HttpVersion;
import io.netty.util.CharsetUtil;
import io.netty.util.concurrent.FastThreadLocal;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;


public class HelloServerHandler extends SimpleChannelInboundHandler<Object> {
    private static final FastThreadLocal<DateFormat> FORMAT = new FastThreadLocal<DateFormat>() {
        @Override
        protected DateFormat initialValue() {
            return new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss z");
        }
    };

    private static final ByteBuf CONTENT_BUFFER = Unpooled.unreleasableBuffer(Unpooled.directBuffer().writeBytes("Hello, World!".getBytes(CharsetUtil.UTF_8)));
    private static final CharSequence contentLength = HttpHeaders.newEntity(String.valueOf(CONTENT_BUFFER.readableBytes()));

    private static final CharSequence TYPE_PLAIN = HttpHeaders.newEntity("text/plain; charset=UTF-8");
    private static final CharSequence TYPE_JSON = HttpHeaders.newEntity("application/json; charset=UTF-8");
    private static final CharSequence SERVER_NAME = HttpHeaders.newEntity("Netty");
    private static final CharSequence CONTENT_TYPE_ENTITY = HttpHeaders.newEntity(HttpHeaders.Names.CONTENT_TYPE);
    private static final CharSequence DATE_ENTITY = HttpHeaders.newEntity(HttpHeaders.Names.DATE);
    private static final CharSequence CONTENT_LENGTH_ENTITY = HttpHeaders.newEntity(HttpHeaders.Names.CONTENT_LENGTH);
    private static final CharSequence SERVER_ENTITY = HttpHeaders.newEntity(HttpHeaders.Names.SERVER);
    private static final CharSequence CONNECTION = HttpHeaders.newEntity(HttpHeaders.Names.CONNECTION);

    private volatile CharSequence date = HttpHeaders.newEntity(FORMAT.get().format(new Date()));

    HelloServerHandler(ScheduledExecutorService service) {
        service.scheduleWithFixedDelay(new Runnable() {
            private final DateFormat format = FORMAT.get();

            @Override
            public void run() {
                date = HttpHeaders.newEntity(format.format(new Date()));
            }
        }, 1000, 1000, TimeUnit.MILLISECONDS);

    }

    @Override
    public void channelRead0(ChannelHandlerContext ctx, Object msg) throws Exception {
        if (msg instanceof HttpRequest) {
            HttpRequest request = (HttpRequest) msg;
            String uri = request.getUri();
            if (uri.equals("/plaintext")) {
                    writeResponse(ctx, request, CONTENT_BUFFER.duplicate(), TYPE_PLAIN, contentLength);
                    return;
            }

            FullHttpResponse response = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.NOT_FOUND, Unpooled.EMPTY_BUFFER, false);
            ctx.write(response).addListener(ChannelFutureListener.CLOSE);
        }
    }

    private void writeResponse(ChannelHandlerContext ctx, HttpRequest request, ByteBuf buf, CharSequence contentType, CharSequence contentLength) {
        // Decide whether to close the connection or not.
        boolean keepAlive = HttpHeaders.isKeepAlive(request);
        // Build the response object.
        FullHttpResponse response = new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.OK, buf, false);
        HttpHeaders headers = response.headers();
        headers.set(CONTENT_TYPE_ENTITY, contentType);
        headers.set(SERVER_ENTITY, SERVER_NAME);
        headers.set(DATE_ENTITY, date);
        headers.set(CONTENT_LENGTH_ENTITY, contentLength);

        // Close the non-keep-alive connection after the write operation is
        // done.
        if (!keepAlive) {
            ctx.write(response).addListener(ChannelFutureListener.CLOSE);
        } else {
            ctx.write(response, ctx.voidPromise());
            ctx.flush();
        }
    }

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {
        ctx.close();
    }

    @Override
    public void channelReadComplete(ChannelHandlerContext ctx) throws Exception {
        ctx.flush();
    }
}