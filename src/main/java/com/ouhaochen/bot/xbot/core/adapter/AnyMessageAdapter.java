package com.ouhaochen.bot.xbot.core.adapter;

import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.message.MessageEvent;

public class AnyMessageAdapter implements MessageAdapter {
    private final MessageEvent event;

    public AnyMessageAdapter(MessageEvent event) {
        this.event = event;
    }

    @Override
    public Long getBotId() {
        return event.getSelfId();
    }

    @Override
    public Long getUserId() {
        return event.getUserId();
    }

    @Override
    public String getUserNickName() {
        return "";
    }

    @Override
    public Long getGroupId() {
        if (event instanceof GroupMessageEvent) {
            return ((GroupMessageEvent) event).getGroupId();
        }
        return 0L;
    }
}
