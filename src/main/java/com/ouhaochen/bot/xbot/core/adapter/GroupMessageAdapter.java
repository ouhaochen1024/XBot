package com.ouhaochen.bot.xbot.core.adapter;

import com.mikuac.shiro.dto.event.message.GroupMessageEvent;

public class GroupMessageAdapter implements MessageAdapter {
    private final GroupMessageEvent event;

    public GroupMessageAdapter(GroupMessageEvent event) {
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
