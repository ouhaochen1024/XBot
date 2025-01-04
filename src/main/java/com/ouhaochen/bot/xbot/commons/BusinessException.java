package com.ouhaochen.bot.xbot.commons;


import com.ouhaochen.bot.xbot.response.ResponseEnum;

public class BusinessException extends RuntimeException {

    private Integer code;

    private String message;

    private ResponseEnum error;

    public BusinessException() {
        super();
        this.message = ResponseEnum.FAILED.getMessage();
        this.code = ResponseEnum.FAILED.getCode();
    }

    public BusinessException(String message) {
        super(message);
        this.message = message;
        this.code = ResponseEnum.FAILED.getCode();
    }

    public BusinessException(Integer code, String message) {
        super(message);
        this.message = message;
        this.code = code;
    }

    public BusinessException(ResponseEnum error) {
        super(error.getMessage());
        this.message = error.getMessage();
        this.code = error.getCode();
    }

    public Integer getCode() {
        return code;
    }

    public void setCode(Integer code) {
        this.code = code;
    }

    @Override
    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public ResponseEnum getError() {
        return error;
    }

    public void setError(ResponseEnum error) {
        this.error = error;
    }
}
