package com.ouhaochen.bot.xbot.commons.response;

import com.ouhaochen.bot.xbot.commons.enums.ResponseEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Serial;
import java.io.Serializable;

public class Response<T> implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;
    private static final Logger logger = LoggerFactory.getLogger(Response.class);

    /**
     * 响应码
     */
    private Integer code;
    /**
     * 响应消息
     */
    private String message;
    /**
     * 响应数据
     */
    private T data;

    protected Response() {
    }

    private Response(Integer code, String message, T data) {
        this.code = code;
        this.message = message;
        this.data = data;
    }

    /**
     * 成功响应
     */
    public static <T> Response<T> success() {
        return new Response<>(
                ResponseEnum.SUCCESS.getCode(),
                ResponseEnum.SUCCESS.getMessage(),
                null);
    }

    public static <T> Response<T> success(String message) {
        return new Response<>(
                ResponseEnum.SUCCESS.getCode(),
                message,
                null);
    }

    public static <T> Response<T> success(T data) {
        return new Response<>(
                ResponseEnum.SUCCESS.getCode(),
                ResponseEnum.SUCCESS.getMessage(),
                data);
    }

    public static <T> Response<T> success(String message, T data) {
        return new Response<>(
                ResponseEnum.SUCCESS.getCode(),
                message,
                data);
    }

    /**
     * 失败响应
     */
    public static <T> Response<T> failed(ResponseEnum resultEnum) {
        return new Response<>(
                resultEnum.getCode(),
                resultEnum.getMessage(),
                null);
    }

    public static <T> Response<T> failed(Integer code, String message) {
        return new Response<>(
                code,
                message,
                null);
    }

    public static <T> Response<T> failed(String message) {
        return new Response<>(
                ResponseEnum.FAILED.getCode(),
                message,
                null);
    }

    public static <T> Response<T> failed() {
        return failed(ResponseEnum.FAILED);
    }

    public Integer getCode() {
        return code;
    }

    public void setCode(Integer code) {
        this.code = code;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public T getData() {
        return data;
    }

    public void setData(T data) {
        this.data = data;
    }

    @Override
    public String toString() {
        return "Response{" +
                "code=" + code +
                ", message='" + message + '\'' +
                ", data=" + data +
                '}';
    }

    private static void logResponse(Response<?> response) {
        logger.info("Response: {}", response);
    }

    public static <T> Response<T> successWithLog() {
        Response<T> response = success();
        logResponse(response);
        return response;
    }

    public static <T> Response<T> successWithLog(String message) {
        Response<T> response = success(message);
        logResponse(response);
        return response;
    }

    public static <T> Response<T> successWithLog(T data) {
        Response<T> response = success(data);
        logResponse(response);
        return response;
    }

    public static <T> Response<T> successWithLog(String message, T data) {
        Response<T> response = success(message, data);
        logResponse(response);
        return response;
    }

    public static <T> Response<T> failedWithLog(ResponseEnum resultEnum) {
        Response<T> response = failed(resultEnum);
        logResponse(response);
        return response;
    }

    public static <T> Response<T> failedWithLog(Integer code, String message) {
        Response<T> response = failed(code, message);
        logResponse(response);
        return response;
    }

    public static <T> Response<T> failedWithLog(String message) {
        Response<T> response = failed(message);
        logResponse(response);
        return response;
    }

    public static <T> Response<T> failedWithLog() {
        Response<T> response = failed();
        logResponse(response);
        return response;
    }
}
