package com.ouhaochen.bot.xbot.core.aspects.permission;


import com.mikuac.shiro.dto.event.Event;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.message.PrivateMessageEvent;
import com.mikuac.shiro.dto.event.request.GroupAddRequestEvent;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.commons.security.CurrentUserInfo;
import com.ouhaochen.bot.xbot.commons.security.ThreadLocalUserInfo;
import com.ouhaochen.bot.xbot.core.adapters.EventAdapter;
import com.ouhaochen.bot.xbot.core.adapters.GroupAddRequestEventAdapter;
import com.ouhaochen.bot.xbot.core.adapters.GroupMessageEventAdapter;
import com.ouhaochen.bot.xbot.core.adapters.PrivateMessageEventAdapter;
import com.ouhaochen.bot.xbot.core.config.PluginConfig;
import com.ouhaochen.bot.xbot.core.constant.XBotRedisConstantKey;
import com.ouhaochen.bot.xbot.db.dao.BotGroupDao;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

@Slf4j
@Aspect
@Component
@RequiredArgsConstructor
public class PermissionAspect {

    private final BotGroupDao botGroupDao;
    private final RedisTemplateClient redisTemplateClient;

    @Around(value = "@annotation(permission)", argNames = "point,permission")
    public Object doBefore(ProceedingJoinPoint point, Permission permission) throws Throwable {
        Object[] args = point.getArgs();
        Event event = (Event) args[1];
        EventAdapter adapter;
        if (event instanceof GroupMessageEvent) {
            adapter = new GroupMessageEventAdapter((GroupMessageEvent) event);
        } else if (event instanceof PrivateMessageEvent) {
            adapter = new PrivateMessageEventAdapter((PrivateMessageEvent) event);
        } else if (event instanceof GroupAddRequestEvent) {
            adapter = new GroupAddRequestEventAdapter((GroupAddRequestEvent) event);
        } else {
            return point.proceed();
        }
        if (!PluginConfig.SUPERVISORS.contains(adapter.getUserId()) && redisTemplateClient.hasHashKey(XBotRedisConstantKey.X_BOT_BLACKLIST_HASH_KEY + adapter.getBotId(), adapter.getUserId().toString())){
            return null;
        }
        if (permission.checkUser() && !PluginConfig.SUPERVISORS.contains(adapter.getUserId())) {
            return null;
        }
        if (permission.checkGroup() && adapter.getGroupId() != null && !botGroupDao.isGroupManager(adapter.getBotId(), adapter.getGroupId())) {
            return null;
        }
        // 插件名称
        String pluginName = point.getTarget().getClass().getSimpleName();
        // 将用户信息存入当前线程
        CurrentUserInfo currentUserInfo = new CurrentUserInfo()
                .setBotId(adapter.getBotId())
                .setUserId(adapter.getUserId())
                .setGroupId(adapter.getGroupId())
                .setUserNickName(adapter.getUserNickName())
                .setCurrentPluginName(pluginName);
        ThreadLocalUserInfo.setCurrentUserInfo(currentUserInfo);
        return point.proceed();
    }

}
