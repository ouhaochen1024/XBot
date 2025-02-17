package com.ouhaochen.bot.xbot.core.constant;

import com.ouhaochen.bot.xbot.commons.redis.constants.RedisConstantKey;

public final class XBotRedisConstantKey extends RedisConstantKey {
    //群插件状态
    public static final String X_BOT_GROUP_PLUGIN_STATUS_HASH_KEY = "x_bot_group_plugin_status:";
    //私聊插件状态
    public static final String X_BOT_PRIVATE_PLUGIN_STATUS_HASH_KEY = "x_bot_private_plugin_status:";
    public static String X_BOT_GROUP_PLUGIN_STATUS_HASH_KEY(Long botId, Long groupId) {
        return X_BOT_GROUP_PLUGIN_STATUS_HASH_KEY + botId + ":" + groupId;
    }
    //全局黑名单
    public static final String X_BOT_BLACKLIST_HASH_KEY = "x_bot_blacklist:";
}
