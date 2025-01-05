package com.ouhaochen.bot.xbot.core.aspect;


import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.message.MessageEvent;
import com.mikuac.shiro.dto.event.message.PrivateMessageEvent;
import com.ouhaochen.bot.xbot.commons.security.CurrentUserInfo;
import com.ouhaochen.bot.xbot.commons.security.ThreadLocalUserInfo;
import com.ouhaochen.bot.xbot.core.adapter.AnyMessageAdapter;
import com.ouhaochen.bot.xbot.core.adapter.GroupMessageAdapter;
import com.ouhaochen.bot.xbot.core.adapter.MessageAdapter;
import com.ouhaochen.bot.xbot.core.adapter.PrivateMessageAdapter;
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
        MessageEvent event = (MessageEvent) args[1];
        MessageAdapter messageAdapter;
        if (event instanceof GroupMessageEvent) {
            messageAdapter = new GroupMessageAdapter((GroupMessageEvent) event);
        } else if (event instanceof PrivateMessageEvent) {
            messageAdapter = new PrivateMessageAdapter((PrivateMessageEvent) event);
        } else {
            messageAdapter = new AnyMessageAdapter(event);
        }
        if (permission.checkUser() && !PluginConfig.SUPERVISORS.contains(messageAdapter.getUserId())) {
            return null;
        }
        if (permission.checkGroup() && !botGroupService.isGroupManager(messageAdapter.getBotId(), messageAdapter.getGroupId())) {
            return null;
        }
        //将用户信息存入当前线程
        CurrentUserInfo currentUserInfo = new CurrentUserInfo()
                .setBotId(messageAdapter.getBotId())
                .setUserId(messageAdapter.getUserId())
                .setGroupId(messageAdapter.getGroupId())
                .setUserNickName(messageAdapter.getUserNickName());
        ThreadLocalUserInfo.setCurrentUserInfo(currentUserInfo);
        return point.proceed();
    }

}
