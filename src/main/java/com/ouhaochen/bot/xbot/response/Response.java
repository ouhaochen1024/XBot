package com.ouhaochen.bot.xbot.response;

import java.io.Serial;
import java.io.Serializable;


public class Response<T> implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

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

    public Response(Integer code, String message, T data) {
        this.code = code;
        this.message = message;
        this.data = data;
    }

    public Response(Integer code, String message) {
        this.code = code;
        this.message = message;
    }

    @SuppressWarnings("unchecked")
    public static <T> Response<T> success() {
        return new Response<>(
                ResponseEnum.SUCCESS.getCode(),
                ResponseEnum.SUCCESS.getMessage(),
                (T) "DONE");
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
}