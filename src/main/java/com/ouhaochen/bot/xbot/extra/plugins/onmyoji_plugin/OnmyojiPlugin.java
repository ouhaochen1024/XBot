package com.ouhaochen.bot.xbot.extra.plugins.onmyoji_plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.MessageHandlerFilter;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.ouhaochen.bot.xbot.core.aspects.permission.Permission;
import com.ouhaochen.bot.xbot.core.aspects.plugin.Plugin;
import com.ouhaochen.bot.xbot.core.context.PluginServiceContext;
import com.ouhaochen.bot.xbot.core.plugins.group_manage_plugin.GroupManagePluginService;
import com.ouhaochen.bot.xbot.core.utils.ActionUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
@Shiro
@Component
@RequiredArgsConstructor
@Plugin(name = "阴阳师订阅插件", author = "ouhaochen", description = "XBot阴阳师订阅插件")
public class OnmyojiPlugin {

    private final ActionUtil actionUtil;
    private final OnmyojiPluginService onmyojiPluginService;

    //task任务爬取
    @Async("ThreadPoolConfig")
    @Scheduled(cron = "0/5 * * * * ?")
    public void getOfficialFeeds() {
        PluginServiceContext context = onmyojiPluginService.getOfficialFeeds();
        //actionUtil.sendGroupResponse(context.getBotId(), context.getGroupId(), context);
    }




}
