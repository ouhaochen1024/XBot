package com.ouhaochen.bot.xbot.configs.aspect.log;

import java.lang.annotation.*;

/**
 * AOP日志切入
 */
//注解放置的目标位置,METHOD是可注解在方法级别上
@Target(ElementType.METHOD)
//注解在哪个阶段执行
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface Log {
    /**
     * 操作模块
     */
    String module() default "";

    /**
     * 操作类型
     */
    String type() default "";

    /**
     * 操作说明
     */
    String desc() default "";
}
