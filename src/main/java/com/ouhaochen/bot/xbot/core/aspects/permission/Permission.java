package com.ouhaochen.bot.xbot.core.aspects.permission;

import java.lang.annotation.*;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface Permission {
    /**
     * 是否检查当前用户是否为机器人的管理者
     */
    boolean checkUser() default true;

    /**
     * 是否检查当前QQ群是否有权限使用机器人
     */
    boolean checkGroup() default true;
}
