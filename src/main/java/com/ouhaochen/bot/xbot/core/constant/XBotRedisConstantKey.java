package com.ouhaochen.bot.xbot.core.constant;

import com.ouhaochen.bot.xbot.commons.redis.constants.RedisConstantKey;

public final class XBotRedisConstantKey extends RedisConstantKey {
    public static final String X_BOT_PLUGINS_LIST_SET_KEY = "x_bot_plugins_list";
    public static final String X_BOT_PLUGIN_STATUS_HASH_KEY = "x_bot_plugin_status:";
    public static String X_BOT_PLUGIN_STATUS_HASH_KEY(Long botId, Long groupId) {
        return X_BOT_PLUGIN_STATUS_HASH_KEY + botId + ":" + groupId;
    }
}
