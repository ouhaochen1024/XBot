package com.ouhaochen.bot.xbot.core.aspect.permission;

import java.lang.annotation.*;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD})
public @interface Permission {
    /**
     * 是否检查用户机器人所有权
     */
    boolean checkUser() default true;

    /**
     * 是否检查该群在管理名单里面
     */
    boolean checkGroup() default true;
}
