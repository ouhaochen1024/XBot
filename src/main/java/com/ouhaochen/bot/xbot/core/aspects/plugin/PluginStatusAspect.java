package com.ouhaochen.bot.xbot.core.aspects.plugin;

import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.Event;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.message.PrivateMessageEvent;
import com.mikuac.shiro.dto.event.request.GroupAddRequestEvent;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.core.adapters.EventAdapter;
import com.ouhaochen.bot.xbot.core.adapters.GroupAddRequestEventAdapter;
import com.ouhaochen.bot.xbot.core.adapters.GroupMessageEventAdapter;
import com.ouhaochen.bot.xbot.core.adapters.PrivateMessageEventAdapter;
import com.ouhaochen.bot.xbot.core.constant.XBotRedisConstantKey;
import com.ouhaochen.bot.xbot.core.enums.PluginStatusEnum;
import com.ouhaochen.bot.xbot.core.utils.CommonUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.stereotype.Component;


@Slf4j
@Aspect
@Component
@RequiredArgsConstructor
public class PluginStatusAspect {

    private final RedisTemplateClient redisTemplateClient;

    @Pointcut("@within(com.ouhaochen.bot.xbot.core.aspects.plugin.Plugin) && execution(* com.ouhaochen.bot.xbot.core.plugins..*.*(..)) || " +
            "execution(* com.ouhaochen.bot.xbot.extra.plugins..*.*(..))")
    public void plugins() {
    }

    @Around(value = "plugins()")
    public Object around(ProceedingJoinPoint point) throws Throwable {
        Object[] args = point.getArgs();
        if (args.length == 0) {
            return point.proceed();
        }
        Bot bot = (Bot) args[0];
        Event event = (Event) args[1];
        EventAdapter adapter;
        Integer status;
        String pluginName = CommonUtil.getPluginInfo(point.getTarget().getClass()).getName();
        if (event instanceof GroupMessageEvent) {
            adapter = new GroupMessageEventAdapter((GroupMessageEvent) event);
            if (adapter.getGroupId() != null) {
                status = (Integer) redisTemplateClient.getHashValue(XBotRedisConstantKey.X_BOT_GROUP_PLUGIN_STATUS_HASH_KEY(bot.getSelfId(), adapter.getGroupId()), pluginName);
            } else {
                status = (Integer) redisTemplateClient.getHashValue(XBotRedisConstantKey.X_BOT_PRIVATE_PLUGIN_STATUS_HASH_KEY + bot.getSelfId(), pluginName);
            }
        } else if (event instanceof PrivateMessageEvent) {
            status = (Integer) redisTemplateClient.getHashValue(XBotRedisConstantKey.X_BOT_PRIVATE_PLUGIN_STATUS_HASH_KEY + bot.getSelfId(), pluginName);
        } else if (event instanceof GroupAddRequestEvent) {
            adapter = new GroupAddRequestEventAdapter((GroupAddRequestEvent) event);
            status = (Integer) redisTemplateClient.getHashValue(XBotRedisConstantKey.X_BOT_GROUP_PLUGIN_STATUS_HASH_KEY(bot.getSelfId(), adapter.getGroupId()), pluginName);
        } else {
            return point.proceed();
        }
        //判断状态
        if (status == null || PluginStatusEnum.DISABLED.getCode().equals(status)) {
            return null;
        }
        return point.proceed();
    }
}
