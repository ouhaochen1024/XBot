package com.ouhaochen.bot.xbot.core.context;


import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
public class BotContext<T> {
    private String msg;
    private Boolean approve = Boolean.FALSE;
    private String approveReason = "";
    private Boolean autoEscape = Boolean.FALSE;
    private T data;

    // 带参构造方法
    public BotContext(T data) {
        this.data = data;
    }

    public BotContext(String msg, T data) {
        this.msg = msg;
        this.data = data;
    }

    public BotContext(String msg, Boolean approve, String approveReason) {
        this.msg = msg;
        this.approve = approve;
        this.approveReason = approveReason;
    }

    // 静态工厂方法
    public static <T> BotContext<T> ofData(T data) {
        return new BotContext<>(data);
    }

    public static <T> BotContext<T> ofData(String msg, T data) {
        return new BotContext<>(msg, data);
    }

    public static <T> BotContext<T> ofMsg(String msg) {
        return new BotContext<>(msg, null);
    }

    public static <T> BotContext<T> ofApprove(String msg, Boolean approve, String approveReason) {
        return new BotContext<>(msg, approve, approveReason);
    }
}
