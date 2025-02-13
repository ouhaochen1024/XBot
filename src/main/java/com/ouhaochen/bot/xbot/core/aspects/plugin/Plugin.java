package com.ouhaochen.bot.xbot.core.aspects.plugin;

import com.mikuac.shiro.annotation.common.Shiro;
import com.ouhaochen.bot.xbot.core.enums.PluginTypeEnum;
import org.springframework.stereotype.Component;

import java.lang.annotation.*;

@Shiro
@Component
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
     * 插件类型
     */
    PluginTypeEnum type() default PluginTypeEnum.SYSTEM;
}
