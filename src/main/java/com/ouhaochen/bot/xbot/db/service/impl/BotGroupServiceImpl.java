package com.ouhaochen.bot.xbot.db.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.ouhaochen.bot.xbot.commons.enums.DelFlagEnum;
import com.ouhaochen.bot.xbot.db.domain.entity.BotGroupEntity;
import com.ouhaochen.bot.xbot.db.mapper.BotGroupMapper;
import com.ouhaochen.bot.xbot.db.service.BotGroupService;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author ouhaochen
 * @since 2025-01-04
 */
@Service
public class BotGroupServiceImpl extends ServiceImpl<BotGroupMapper, BotGroupEntity> implements BotGroupService {

    @Override
    public boolean isGroupManager(Long userId, Long groupId) {
        return exists(new LambdaQueryWrapper<BotGroupEntity>()
                .eq(BotGroupEntity::getGroupId, groupId)
                .eq(BotGroupEntity::getBotId, userId)
                .eq(BotGroupEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode())
        );
    }
}
