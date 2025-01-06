package com.ouhaochen.bot.xbot.core.adapter;

import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.message.MessageEvent;

public class AnyMessageEventAdapter implements EventAdapter {
    private final AnyMessageEvent event;

    public AnyMessageEventAdapter(AnyMessageEvent event) {
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
        if (null != event.getGroupId()) {
            return event.getGroupId();
        }
        return 0L;
    }
}
