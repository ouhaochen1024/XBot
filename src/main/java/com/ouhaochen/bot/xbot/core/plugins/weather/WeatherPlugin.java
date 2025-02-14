package com.ouhaochen.bot.xbot.core.plugins.weather;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.MessageHandlerFilter;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.ouhaochen.bot.xbot.core.aspects.permission.Permission;
import com.ouhaochen.bot.xbot.core.aspects.plugin.Plugin;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import com.ouhaochen.bot.xbot.core.enums.PluginTypeEnum;
import com.ouhaochen.bot.xbot.core.utils.ActionUtil;
import com.ouhaochen.bot.xbot.core.utils.MatcherUtil;
import lombok.RequiredArgsConstructor;

import java.util.regex.Matcher;

@RequiredArgsConstructor
@Plugin(name = "天气查询", author = "ouhaochen", description = "XBot天气查询插件", type = PluginTypeEnum.COMMON)
public class WeatherPlugin {

    private final WeatherPluginService weatherPluginService;

    @Permission(checkUser = false)
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^天气\\s(.*)?$")
    public void weather(Bot bot, AnyMessageEvent event, Matcher matcher) {
        String cityName = MatcherUtil.getNormal(bot, event, matcher);
        if (null == cityName) return;
        BotContext<Object> context = weatherPluginService.weather(cityName);
        ActionUtil.sendResponse(bot, event, context);
    }

    @Permission(checkUser = false)
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^(.*)?\\s天气$")
    public void weather2(Bot bot, AnyMessageEvent event, Matcher matcher) {
        String cityName = MatcherUtil.getNormal(bot, event, matcher);
        if (null == cityName) return;
        BotContext<Object> context = weatherPluginService.weather(cityName);
        ActionUtil.sendResponse(bot, event, context);
    }

}
