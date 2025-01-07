package com.ouhaochen.bot.xbot.core.plugins;

import com.mikuac.shiro.annotation.GroupAddRequestHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.request.GroupAddRequestEvent;
import com.ouhaochen.bot.xbot.core.aspect.permission.Permission;
import com.ouhaochen.bot.xbot.db.service.BotGroupService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Shiro
@Component
@RequiredArgsConstructor
public class GroupManagePlugin {

    private final BotGroupService botGroupService;

    @Permission(checkUser = false)
    @GroupAddRequestHandler
    public void handleAddGroup(Bot bot, GroupAddRequestEvent event){
        while (true) {
            botGroupService.isGroupManager(event.getUserId(), event.getGroupId());
        }
    }


}
