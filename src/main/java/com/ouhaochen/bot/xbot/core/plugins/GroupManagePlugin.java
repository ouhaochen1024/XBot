package com.ouhaochen.bot.xbot.core.plugins;

import com.mikuac.shiro.annotation.GroupAddRequestHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.request.GroupAddRequestEvent;
import com.ouhaochen.bot.xbot.core.aspects.permission.Permission;
import com.ouhaochen.bot.xbot.core.aspects.plugin.Plugin;
import com.ouhaochen.bot.xbot.db.dao.BotGroupDao;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Shiro
@Component
@RequiredArgsConstructor
@Plugin(name = "群聊管理", author = "ouhaochen", description = "XBot群聊管理插件")
public class GroupManagePlugin {

    private final BotGroupDao botGroupDao;

    @Permission(checkUser = false)
    @GroupAddRequestHandler
    public void handleAddGroup(Bot bot, GroupAddRequestEvent event){
        if (true) {
           return;
        }
    }


}
