package com.ouhaochen.bot.xbot.core.plugins;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.MessageHandlerFilter;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.ouhaochen.bot.xbot.core.aspects.permission.Permission;
import com.ouhaochen.bot.xbot.core.context.PluginServiceContext;
import com.ouhaochen.bot.xbot.core.service.BasePluginService;
import com.ouhaochen.bot.xbot.core.utils.MatcherUtil;
import com.ouhaochen.bot.xbot.core.utils.SendMsgUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.regex.Matcher;

@Shiro
@Component
@RequiredArgsConstructor
public class BasePlugin {

    private final BasePluginService basePluginService;

    @Permission(checkGroup = false)
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^打开插件\\s(.*)?$")
    public void openPlugin(Bot bot, AnyMessageEvent event, Matcher matcher) {
        String pluginName = MatcherUtil.getNormal(bot, event, matcher);
        PluginServiceContext context = basePluginService.openPlugin(bot.getSelfId(), pluginName);
        SendMsgUtil.sendResponse(bot, event, context);
    }

    @Permission(checkGroup = false)
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^关闭插件\\s(.*)?$")
    public void closePlugin(Bot bot, AnyMessageEvent event, Matcher matcher) {
        String pluginName = MatcherUtil.getNormal(bot, event, matcher);
        PluginServiceContext context = basePluginService.closePlugin(bot.getSelfId(), pluginName);
        SendMsgUtil.sendResponse(bot, event, context);
    }

    @Permission(checkGroup = false)
    @GroupMessageHandler
    @MessageHandlerFilter(cmd = "添加本群使用权限")
    public void addGroup(Bot bot, GroupMessageEvent event) {
        PluginServiceContext context = basePluginService.addGroup(event.getSelfId(), event.getGroupId());
        SendMsgUtil.sendResponse(bot, event, context);
    }

    @Permission(checkGroup = false)
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^添加群\\s(.*)?\\s使用权限$")
    public void addGroup(Bot bot, AnyMessageEvent event, Matcher matcher) {
        Number number = MatcherUtil.getNumber(bot, event, matcher);
        if (null == number) return;
        PluginServiceContext context = basePluginService.addGroup(event.getSelfId(), number.longValue());
        SendMsgUtil.sendResponse(bot, event, context);
    }

    @Permission(checkGroup = false)
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^删除群\\s(.*)?\\s使用权限$")
    public void delGroup(Bot bot, AnyMessageEvent event, Matcher matcher) {
        Number number = MatcherUtil.getNumber(bot, event, matcher);
        if (null == number) return;
        PluginServiceContext context = basePluginService.delGroup(event.getSelfId(), number.longValue());
        SendMsgUtil.sendResponse(bot, event, context);
    }
}
