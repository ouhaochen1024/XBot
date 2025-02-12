package com.ouhaochen.bot.xbot.core.plugins.system_plugin;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.MessageHandlerFilter;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.ouhaochen.bot.xbot.core.aspects.permission.Permission;
import com.ouhaochen.bot.xbot.core.aspects.plugin.Plugin;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import com.ouhaochen.bot.xbot.core.enums.PluginTypeEnum;
import com.ouhaochen.bot.xbot.core.info.PluginInfo;
import com.ouhaochen.bot.xbot.core.utils.ActionUtil;
import com.ouhaochen.bot.xbot.core.utils.MatcherUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.regex.Matcher;

@Shiro
@Component
@RequiredArgsConstructor
@Plugin(name = "系统插件", author = "ouhaochen", description = "XBot系统插件", type = PluginTypeEnum.SYSTEM)
public class SystemPlugin {

    private final SystemPluginService systemPluginService;

    @Permission(checkGroup = false)
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^(?:启用插件|启用)\\s+(.*)$")
    public void enablePlugin(Bot bot, AnyMessageEvent event, Matcher matcher) {
        String pluginName = MatcherUtil.getNormal(bot, event, matcher);
        BotContext<Object> context = systemPluginService.enablePlugin(bot.getSelfId(), event.getGroupId(), pluginName);
        ActionUtil.sendResponse(bot, event, context);
    }

    @Permission(checkGroup = false)
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^(?:禁用插件|禁用)\\s+(.*)$")
    public void disablePlugin(Bot bot, AnyMessageEvent event, Matcher matcher) {
        String pluginName = MatcherUtil.getNormal(bot, event, matcher);
        BotContext<Object> context = systemPluginService.disablePlugin(bot.getSelfId(), event.getGroupId(), pluginName);
        ActionUtil.sendResponse(bot, event, context);
    }

    @Permission(checkGroup = false)
    @GroupMessageHandler
    @MessageHandlerFilter(cmd = "插件列表")
    public void viewPlugins(Bot bot, AnyMessageEvent event) {
        BotContext<List<PluginInfo>> context = systemPluginService.viewPlugins(bot.getSelfId(), event.getGroupId());
        ActionUtil.sendResponse(bot, event, context);
    }


    @Permission(checkGroup = false)
    @GroupMessageHandler
    @MessageHandlerFilter(cmd = "添加本群|添加本群权限")
    public void addGroup(Bot bot, GroupMessageEvent event) {
        BotContext<Object> context = systemPluginService.addGroup(event.getSelfId(), event.getGroupId());
        ActionUtil.sendResponse(bot, event, context);
    }

    @Permission(checkGroup = false)
    @GroupMessageHandler
    @MessageHandlerFilter(cmd = "删除本群|删除本群权限")
    public void delGroup(Bot bot, GroupMessageEvent event) {
        BotContext<Object> context = systemPluginService.delGroup(event.getSelfId(), event.getGroupId());
        ActionUtil.sendResponse(bot, event, context);
    }

    @Permission(checkGroup = false)
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^添加群\\s(.*)?$")
    public void addGroup(Bot bot, AnyMessageEvent event, Matcher matcher) {
        Number number = MatcherUtil.getNumber(bot, event, matcher);
        if (null == number) return;
        BotContext<Object> context = systemPluginService.addGroup(event.getSelfId(), number.longValue());
        ActionUtil.sendResponse(bot, event, context);
    }

    @Permission(checkGroup = false)
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^删除群\\s(.*)?$")
    public void delGroup(Bot bot, AnyMessageEvent event, Matcher matcher) {
        Number number = MatcherUtil.getNumber(bot, event, matcher);
        if (null == number) return;
        BotContext<Object> context = systemPluginService.delGroup(event.getSelfId(), number.longValue());
        ActionUtil.sendResponse(bot, event, context);
    }
}
