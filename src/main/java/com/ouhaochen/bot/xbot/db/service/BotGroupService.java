package com.ouhaochen.bot.xbot.db.service;

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
public interface BotGroupService extends IService<BotGroupEntity> {

    boolean isGroupManager(Long userId, Long groupId);
}
