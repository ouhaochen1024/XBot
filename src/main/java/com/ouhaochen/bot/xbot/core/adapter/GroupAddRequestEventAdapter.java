package com.ouhaochen.bot.xbot.core.adapter;

import com.mikuac.shiro.dto.event.request.GroupAddRequestEvent;

public class GroupAddRequestEventAdapter implements EventAdapter {

    private final GroupAddRequestEvent event;

    public GroupAddRequestEventAdapter(GroupAddRequestEvent event) {
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
        return event.getGroupId();
    }

    public String getComment() {
        return event.getComment();
    }

    public Long getInvitorId() {
        return event.getInvitorId();
    }
}
