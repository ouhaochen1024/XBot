package com.ouhaochen.bot.xbot.core.plugins;

import com.mikuac.shiro.annotation.GroupAddRequestHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.request.GroupAddRequestEvent;
import com.ouhaochen.bot.xbot.core.aspects.permission.Permission;
import com.ouhaochen.bot.xbot.db.dao.BotGroupDao;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Shiro
@Component
@RequiredArgsConstructor
public class GroupManagePlugin {

    private final BotGroupDao botGroupDao;

    @Permission(checkUser = false)
    @GroupAddRequestHandler
    public void handleAddGroup(Bot bot, GroupAddRequestEvent event){
        while (true) {
            botGroupDao.isGroupManager(event.getUserId(), event.getGroupId());
        }
    }


}
