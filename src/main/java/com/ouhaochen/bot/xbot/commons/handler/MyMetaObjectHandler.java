package com.ouhaochen.bot.xbot.commons.handler;

import com.baomidou.mybatisplus.core.handlers.MetaObjectHandler;
import com.ouhaochen.bot.xbot.commons.enums.DelFlagEnum;
import com.ouhaochen.bot.xbot.commons.utils.UserEvnUtil;
import org.apache.ibatis.reflection.MetaObject;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;

@Component
public class MyMetaObjectHandler implements MetaObjectHandler {
    @Override
    public void insertFill(MetaObject metaObject) {
        metaObject.setValue("createTime", LocalDateTime.now());
        metaObject.setValue("updateTime", LocalDateTime.now());
        metaObject.setValue("createBy", UserEvnUtil.getUserId());
        metaObject.setValue("updateBy", UserEvnUtil.getUserId());
        metaObject.setValue("delFlag", DelFlagEnum.NOT_DELETED.getCode());
    }

    @Override
    public void updateFill(MetaObject metaObject) {
        metaObject.setValue("updateTime", LocalDateTime.now());
        metaObject.setValue("updateBy", UserEvnUtil.getUserId());
    }
}

