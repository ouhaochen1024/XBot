package com.ouhaochen.bot.xbot.configs.handler;

import com.baomidou.mybatisplus.core.handlers.MetaObjectHandler;
import com.ouhaochen.bot.xbot.enums.DelFlagEnum;
import com.ouhaochen.bot.xbot.plugins.permission.UserEnvInfo;
import org.apache.ibatis.reflection.MetaObject;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Component
public class MyMetaObjectHandler implements MetaObjectHandler {

    // 定义 ThreadLocal 变量，保存当前线程的用户ID
    private static final ThreadLocal<UserEnvInfo> CURRENT_USER_ENV_INFO = ThreadLocal.withInitial(UserEnvInfo::new);

    // 设置当前线程的用户环境信息
    public static void setCurrentUserEnvInfo(UserEnvInfo userEnvInfo) {
        CURRENT_USER_ENV_INFO.set(userEnvInfo);
    }

    // 获取当前线程的用户环境信息
    public static UserEnvInfo getCurrentUserEnvInfo() {
        return CURRENT_USER_ENV_INFO.get();
    }

    private Long getCurrentUserId() {
        return getCurrentUserEnvInfo().getUserId();
    }

    private Long getCurrentGroupId() {
        return getCurrentUserEnvInfo().getGroupId();
    }


    @Override
    public void insertFill(MetaObject metaObject) {
        metaObject.setValue("createTime", LocalDateTime.now());
        metaObject.setValue("updateTime", LocalDateTime.now());
        metaObject.setValue("createBy", getCurrentUserId());
        metaObject.setValue("updateBy", getCurrentUserId());
        metaObject.setValue("delFlag", DelFlagEnum.NOT_DELETED.getCode());
    }

    @Override
    public void updateFill(MetaObject metaObject) {
        metaObject.setValue("updateTime", LocalDateTime.now());
        metaObject.setValue("updateBy", getCurrentUserId());
    }
}

