package com.ouhaochen.bot.xbot.config.handler;


import com.ouhaochen.bot.xbot.common.BusinessException;
import com.ouhaochen.bot.xbot.response.Response;
import com.ouhaochen.bot.xbot.response.ResponseEnum;
import jakarta.servlet.http.HttpServletRequest;
import lombok.extern.slf4j.Slf4j;
import org.apache.catalina.connector.ClientAbortException;
import org.dromara.hutool.core.util.ObjUtil;
import org.springframework.jdbc.BadSqlGrammarException;
import org.springframework.validation.BindException;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.Objects;

/**
 * 全局异常处理器
 */
@Slf4j
@RestControllerAdvice
public class GlobalControllerHandler {
    /**
     * 自定义异常
     */
    @ExceptionHandler(BusinessException.class)
    public Response<Object> handleErrorException(BusinessException e, HttpServletRequest request) {
        String requestURI = request.getRequestURI();
        String msg = ObjUtil.isNotNull(e.getError()) ? e.getError().getMessage() : e.getMessage();
        Integer code = ObjUtil.isNotNull(e.getError()) ? e.getError().getCode() : e.getCode();
        log.error("请求地址'{}',请求异常'{}'", requestURI, msg);
        return Response.failed(code, msg);
    }
    /**
     * 自定义验证参数异常1
     */
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public Response<Object> handleMethodArgumentNotValidException(MethodArgumentNotValidException e, HttpServletRequest request) {
        String requestURI = request.getRequestURI();
        String msg = Objects.requireNonNull(e.getBindingResult().getFieldError()).getDefaultMessage();
        log.error("请求地址'{}',请求异常'{}'", requestURI, msg);
        return Response.failed(ResponseEnum.BAD_REQUEST.getCode(), msg);
    }
    /**
     * 自定义验证参数异常2
     */
    @ExceptionHandler(BindException.class)
    public Response<Object> handleBindException(BindException e, HttpServletRequest request) {
        String requestURI = request.getRequestURI();
        String msg = Objects.requireNonNull(e.getBindingResult().getFieldError()).getDefaultMessage();
        log.error("请求地址'{}',请求异常'{}'", requestURI, msg);
        return Response.failed(ResponseEnum.BAD_REQUEST.getCode(), msg);
    }
    /**
     * 解决管道关闭的问题
     */
    @ExceptionHandler(ClientAbortException.class)
    public void pipeException(ClientAbortException e) {
    }
    /**
     * 防止sql异常导致的外泄
     */
    @ExceptionHandler(BadSqlGrammarException.class)
    public Response<Object> sqlException(BadSqlGrammarException e) {
        log.error("发生sql异常.",  e);
        return Response.failed("服务开小差了，请稍后重试");
    }
    /**
     * illegal异常
     */
    @ExceptionHandler(IllegalStateException.class)
    public void illegalException(IllegalStateException e) {
        log.error("发生illegal异常.",  e);
    }
    /**
     * 未知的运行时异常
     */
    @ExceptionHandler(RuntimeException.class)
    public Response<Object> handleRuntimeException(RuntimeException e, HttpServletRequest request) {
        String requestURI = request.getRequestURI();
        log.error("请求地址'{}',发生未知异常.", requestURI, e);
        return Response.failed("服务开小差了，请稍后重试");
    }
    /**
     * 系统异常
     */
    @ExceptionHandler(Exception.class)
    public Response<Object> handleException(Exception e, HttpServletRequest request) {
        String requestURI = request.getRequestURI();
        log.error("请求地址'{}',发生系统异常.", requestURI, e);
        return Response.failed(e.getMessage());
    }
}
