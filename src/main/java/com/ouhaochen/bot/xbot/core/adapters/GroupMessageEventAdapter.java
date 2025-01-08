package com.ouhaochen.bot.xbot.core.adapters;

import com.mikuac.shiro.dto.event.message.GroupMessageEvent;

public class GroupMessageEventAdapter implements EventAdapter {
    private final GroupMessageEvent event;

    public GroupMessageEventAdapter(GroupMessageEvent event) {
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
        return event.getSender().getNickname();
    }

    @Override
    public Long getGroupId() {
        return event.getGroupId();
    }
}
