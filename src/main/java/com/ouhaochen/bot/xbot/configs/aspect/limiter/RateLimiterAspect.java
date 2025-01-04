package com.ouhaochen.bot.xbot.configs.aspect.limiter;


import com.ouhaochen.bot.xbot.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.commons.BusinessException;
import com.ouhaochen.bot.xbot.utils.HttpUtil;
import jakarta.annotation.Resource;
import jakarta.servlet.http.HttpServletRequest;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.lang.reflect.Method;
import java.util.concurrent.TimeUnit;

@Slf4j
@Aspect
@Component
public class RateLimiterAspect {

    @Resource
    private RedisTemplateClient redisTemplateClient;

    /**
     * 滑动窗口实现限流
     */
    @Before("@annotation(rateLimiter)")
    public void doBefore(JoinPoint point, RateLimiter rateLimiter) {
        // 在 {time} 秒内仅允许访问 {count} 次。
        int time = rateLimiter.time();
        int count = rateLimiter.count();
        // 根据用户IP和接口方法，构造key
        String combineKey = getCombineKey(point);
        // 限流逻辑实现
        ZSetOperations<String, Object> zSetOperations = redisTemplateClient.zSetOperations();
        // 记录本次访问的时间结点
        long currentMs = System.currentTimeMillis();
        // 移除{time}秒之前的访问记录（滑动窗口思想）
        zSetOperations.removeRangeByScore(combineKey, 0, currentMs - time * 1000L);
        // 获得当前窗口内的访问记录数（不含本次访问）
        Long currCount = zSetOperations.zCard(combineKey);
        // 把本次访问也算进去，如果总次数限流，则抛出异常
        if (currCount + 1 > count) {
            log.info("[limit] 限制请求数'{}'，当前请求数'{}'，缓存key'{}'", count, currCount, combineKey);
            throw new BusinessException("访问过于频繁，请稍后再试！");
        }
        // 没有限流，则记录本次访问
        zSetOperations.add(combineKey, currentMs, currentMs);
        // 这一步是为了防止member一直存在于内存中
        redisTemplateClient.expire(combineKey, time, TimeUnit.SECONDS);
    }

    /**
     * 把用户IP和接口方法名拼接成 redis 的 key
     *
     * @param point 切入点
     * @return 组合key
     */
    private String getCombineKey(JoinPoint point) {
        StringBuilder sb = new StringBuilder("API_LIMIT:");
        ServletRequestAttributes attributes = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
        assert attributes != null;
        HttpServletRequest request = attributes.getRequest();
        sb.append(HttpUtil.getRealIpAddress(request));
        MethodSignature signature = (MethodSignature) point.getSignature();
        Method method = signature.getMethod();
        Class<?> targetClass = method.getDeclaringClass();
        return sb.append("-").append(targetClass.getName())
                .append("-").append(method.getName()).toString();
    }
}
