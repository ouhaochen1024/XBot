package com.ouhaochen.bot.xbot.response;

import lombok.Getter;

@Getter
public enum ResponseEnum {

    SUCCESS(200, "OK", "操作成功"),
    FAILED(500, "Internal Server Error", "操作失败"),
    BAD_REQUEST(400, "Bad Request", "请求参数有误"),
    NOT_LOGIN(401, "Unauthorized", "登录信息过期，请重新登录"),
    NOT_ACCESS(403, "Forbidden", "权限不足"),
    ;

    private final Integer code;

    private final String status;

    private final String message;

    ResponseEnum(Integer code, String status, String message) {
        this.code = code;
        this.status = status;
        this.message = message;
    }

}
