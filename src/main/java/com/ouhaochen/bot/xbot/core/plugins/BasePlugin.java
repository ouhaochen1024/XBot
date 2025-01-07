package com.ouhaochen.bot.xbot.core.plugins;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.MessageHandlerFilter;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.ouhaochen.bot.xbot.commons.enums.TrueOrFalseEnum;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.core.aspect.permission.Permission;
import com.ouhaochen.bot.xbot.core.constants.XBotRedisConstantKey;
import com.ouhaochen.bot.xbot.core.utils.MatcherUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.regex.Matcher;

@Shiro
@Component
@RequiredArgsConstructor
public class BasePlugin {

    private final RedisTemplateClient redisTemplateClient;

    @Permission(checkGroup = false)
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^打开插件\\s(.*)?$")
    public void openPlugin(Bot bot, AnyMessageEvent event, Matcher matcher) {
        String pluginName = MatcherUtil.getNormalStr(bot, event, matcher);
        if (checkPluginNotExist(bot, event, pluginName)) return;
        redisTemplateClient.putHash(XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY, pluginName, TrueOrFalseEnum.TRUE.getCode());
        bot.sendMsg(event, String.format("插件%s已开启", pluginName), false);
    }

    @Permission(checkGroup = false)
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^关闭插件\\s(.*)?$")
    public void closePlugin(Bot bot, AnyMessageEvent event, Matcher matcher) {
        String pluginName = MatcherUtil.getNormalStr(bot, event, matcher);
        if ("BasePlugin".equals(pluginName)) return;
        if (checkPluginNotExist(bot, event, pluginName)) return;
        redisTemplateClient.putHash(XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY, pluginName, TrueOrFalseEnum.FALSE.getCode());
        bot.sendMsg(event, String.format("插件%s已关闭", pluginName), false);
    }


    private boolean checkPluginNotExist(Bot bot, AnyMessageEvent event, String pluginName) {
        List<String> pluginList = redisTemplateClient.getSet(XBotRedisConstantKey.X_BOT_PLUGINS_LIST_SET_KEY).stream().map(Object::toString).toList();
        if (!pluginList.contains(pluginName)) {
            bot.sendMsg(event, String.format("插件%s不存在", pluginName), false);
            return true;
        }
        return false;
    }

}
