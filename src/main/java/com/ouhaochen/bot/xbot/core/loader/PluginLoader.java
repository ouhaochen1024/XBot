package com.ouhaochen.bot.xbot.core.loader;

import com.ouhaochen.bot.xbot.commons.enums.DelFlagEnum;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.commons.utils.StreamUtil;
import com.ouhaochen.bot.xbot.core.constant.XBotRedisConstantKey;
import com.ouhaochen.bot.xbot.core.enums.PluginStatusEnum;
import com.ouhaochen.bot.xbot.core.info.PluginInfo;
import com.ouhaochen.bot.xbot.core.utils.CommonUtil;
import com.ouhaochen.bot.xbot.db.dao.BotGroupDao;
import com.ouhaochen.bot.xbot.db.entity.BotGroupEntity;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class PluginLoader {

    @Value("${plugin-loader.enabled}")
    private boolean enabled;
    @Value("${xbot.plugins.basePackage}")
    private String basePackage;
    private final BotGroupDao botGroupDao;
    private final RedisTemplateClient redisTemplateClient;

    @PostConstruct
    public void loadPlugins() {
        if (!enabled) {
            return;
        }
        List<PluginInfo> pluginInfos = CommonUtil.getAllPluginInfos(basePackage);
        List<String> pluginNames = StreamUtil.mapping(pluginInfos, PluginInfo::getName);
        //更新全部插件信息缓存
        redisTemplateClient.delete(XBotRedisConstantKey.X_BOT_PLUGINS_LIST_SET_KEY);
        pluginInfos.forEach(pluginInfo -> {
            redisTemplateClient.putSet(XBotRedisConstantKey.X_BOT_PLUGINS_LIST_SET_KEY, pluginInfo);
            //查询全部机器人群组关系
        });
        List<BotGroupEntity> botGroupEntities = botGroupDao.lambdaQuery().eq(BotGroupEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()).list();
        //更新插件状态缓存
        botGroupEntities.forEach(botGroupEntity -> {
            String pluginStatusKey = XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY(botGroupEntity.getBotId(), botGroupEntity.getGroupId());
            //如果没有则初始化
            if (!redisTemplateClient.hasKey(pluginStatusKey)) {
                pluginInfos.forEach(pluginInfo -> {
                    redisTemplateClient.putHash(pluginStatusKey, pluginInfo.getName(), pluginInfo.getEnable() ? PluginStatusEnum.ENABLED.getCode() : PluginStatusEnum.DISABLED.getCode());
                });
                //更新缓存
            } else {
                Map<String, String> statusMap = redisTemplateClient.hGetAll(pluginStatusKey);
                //如果没有在列表里面则移除缓存
                for (Map.Entry<String, String> entry : statusMap.entrySet()) {
                    if (!pluginNames.contains(entry.getKey())) {
                        redisTemplateClient.deleteHash(pluginStatusKey, entry.getKey());
                    }
                }
                //添加新插件
                for (PluginInfo pluginInfo : pluginInfos) {
                    if (!statusMap.containsKey(pluginInfo.getName())) {
                        redisTemplateClient.putHash(pluginStatusKey, pluginInfo.getName(), pluginInfo.getEnable() ? PluginStatusEnum.ENABLED.getCode() : PluginStatusEnum.DISABLED.getCode());
                    }
                }
            }
        });
    }
}
