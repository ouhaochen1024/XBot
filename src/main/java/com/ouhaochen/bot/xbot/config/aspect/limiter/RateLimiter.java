package com.ouhaochen.bot.xbot.config.aspect.limiter;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.TYPE})
public @interface RateLimiter {

    /**
     * 限流时间，单位秒
     */
    int time() default 60;

    /**
     * 限流次数
     */
    int count() default 1;
}
