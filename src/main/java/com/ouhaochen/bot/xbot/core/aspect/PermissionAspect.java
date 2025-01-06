package com.ouhaochen.bot.xbot.core.aspect;


import com.mikuac.shiro.dto.event.Event;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.message.PrivateMessageEvent;
import com.mikuac.shiro.dto.event.request.GroupAddRequestEvent;
import com.ouhaochen.bot.xbot.commons.security.CurrentUserInfo;
import com.ouhaochen.bot.xbot.commons.security.ThreadLocalUserInfo;
import com.ouhaochen.bot.xbot.core.adapter.*;
import com.ouhaochen.bot.xbot.core.configs.PluginConfig;
import com.ouhaochen.bot.xbot.db.service.BotGroupService;
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

    private final BotGroupService botGroupService;

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
        if (permission.checkUser() && !PluginConfig.SUPERVISORS.contains(adapter.getUserId())) {
            return null;
        }
        if (permission.checkGroup() && !botGroupService.isGroupManager(adapter.getBotId(), adapter.getGroupId())) {
            return null;
        }
        //将用户信息存入当前线程
        CurrentUserInfo currentUserInfo = new CurrentUserInfo()
                .setBotId(adapter.getBotId())
                .setUserId(adapter.getUserId())
                .setGroupId(adapter.getGroupId())
                .setUserNickName(adapter.getUserNickName());
        ThreadLocalUserInfo.setCurrentUserInfo(currentUserInfo);
        return point.proceed();
    }

}
