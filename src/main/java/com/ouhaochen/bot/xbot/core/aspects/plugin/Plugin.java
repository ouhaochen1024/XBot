package com.ouhaochen.bot.xbot.core.aspects.plugin;

import java.lang.annotation.*;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE})
public @interface Plugin {
    /**
     * 插件名称
     */
    String name() default "";
    /**
     * 插件作者
     */
    String author() default "未知";
    /**
     * 插件版本
     */
    String version() default "v1.0.0";
    /**
     * 插件描述
     */
    String description() default "一个XBot插件";
    /**
     * 是否默认启用
     */
    boolean enable() default true;
    /**
     * 该插件是否排除插件管理查询
     */
    boolean system() default false;
}
