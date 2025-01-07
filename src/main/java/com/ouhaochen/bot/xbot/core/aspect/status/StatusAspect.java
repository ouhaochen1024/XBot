package com.ouhaochen.bot.xbot.core.aspect.status;

import com.ouhaochen.bot.xbot.commons.enums.TrueOrFalseEnum;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.core.constants.XBotRedisConstantKey;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.List;


@Slf4j
@Aspect
@Component
@RequiredArgsConstructor
public class StatusAspect {

    @Value("${spring.profiles.active}")
    private List<String> active;
    private final RedisTemplateClient redisTemplateClient;

    @Pointcut("execution(* com.ouhaochen.bot.xbot.core.plugins..*.*(..))")
    public void controller() {
    }

    @Around("controller()")
    public Object around(ProceedingJoinPoint point) throws Throwable {
//        if (!active.contains("prod")) return point.proceed();
        //取最后一个包名
        String className = point.getTarget().getClass().getName();
        String[] split = className.split("\\.");
        String pluginName = split[split.length - 1];
        Integer status = (Integer) redisTemplateClient.getHashValue(XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY, pluginName);
        if (status == null) {
            redisTemplateClient.putHash(XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY, pluginName, TrueOrFalseEnum.TRUE.getCode());
        } else if (TrueOrFalseEnum.FALSE.getCode().equals(status)) {
            return null;
        }
        return point.proceed();
    }
}
