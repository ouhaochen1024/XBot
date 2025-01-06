package com.ouhaochen.bot.xbot.db.service;

import com.ouhaochen.bot.xbot.db.entity.BotGroupEntity;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author ouhaochen
 * @since 2025-01-04
 */
public interface BotGroupService extends IService<BotGroupEntity> {

    boolean isGroupManager(Long userId, Long groupId);
}
