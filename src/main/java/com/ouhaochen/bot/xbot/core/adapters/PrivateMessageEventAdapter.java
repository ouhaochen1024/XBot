package com.ouhaochen.bot.xbot.core.adapters;

import com.mikuac.shiro.dto.event.message.PrivateMessageEvent;

public class PrivateMessageEventAdapter implements EventAdapter {
    private final PrivateMessageEvent event;

    public PrivateMessageEventAdapter(PrivateMessageEvent event) {
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
        return event.getPrivateSender().getNickname();
    }

    @Override
    public Long getGroupId() {
        return 0L; // 私聊没有群组ID
    }
}
