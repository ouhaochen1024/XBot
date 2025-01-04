package com.ouhaochen.bot.xbot.plugins.permission;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.message.MessageEvent;
import com.ouhaochen.bot.xbot.configs.XBotConfig;
import com.ouhaochen.bot.xbot.configs.handler.MyMetaObjectHandler;
import com.ouhaochen.bot.xbot.db.domain.entity.BotGroupEntity;
import com.ouhaochen.bot.xbot.db.service.BotGroupService;
import com.ouhaochen.bot.xbot.enums.DelFlagEnum;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class Permission {

    private final BotGroupService botGroupService;

    public boolean isSupervisor(MessageEvent event) {
        UserEnvInfo userEnvInfo = new UserEnvInfo();
        userEnvInfo.setUserId(event.getUserId());
        userEnvInfo.setGroupId(null);
        MyMetaObjectHandler.setCurrentUserEnvInfo(userEnvInfo);
        return XBotConfig.SUPERVISORS.contains(event.getUserId());
    }

    public boolean isGroupManager(GroupMessageEvent event) {
        UserEnvInfo userEnvInfo = new UserEnvInfo();
        userEnvInfo.setUserId(event.getUserId());
        userEnvInfo.setGroupId(event.getGroupId());
        MyMetaObjectHandler.setCurrentUserEnvInfo(userEnvInfo);
        return botGroupService.exists(new LambdaQueryWrapper<BotGroupEntity>()
                .eq(BotGroupEntity::getGroupId, event.getGroupId())
                .eq(BotGroupEntity::getBotId, event.getSelfId())
                .eq(BotGroupEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode())
        );
    }

}
