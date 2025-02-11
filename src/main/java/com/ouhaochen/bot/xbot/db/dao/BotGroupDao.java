package com.ouhaochen.bot.xbot.db.dao;

import com.baomidou.mybatisplus.extension.service.IService;
import com.ouhaochen.bot.xbot.db.entity.BotGroupEntity;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author ouhaochen
 * @since 2025-01-04
 */
public interface BotGroupDao extends IService<BotGroupEntity> {
    boolean isGroupManager(Long userId, Long groupId);
}
