package com.ouhaochen.bot.xbot.core.loader;

import com.ouhaochen.bot.xbot.commons.enums.DelFlagEnum;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.commons.utils.StreamUtil;
import com.ouhaochen.bot.xbot.core.constant.XBotRedisConstantKey;
import com.ouhaochen.bot.xbot.core.enums.PluginStatusEnum;
import com.ouhaochen.bot.xbot.core.enums.PluginTypeEnum;
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
    private final BotGroupDao botGroupDao;
    private final RedisTemplateClient redisTemplateClient;

    @PostConstruct
    public void loadPlugins() {
        if (!enabled) {
            return;
        }
        List<PluginInfo> pluginInfos = CommonUtil.getAllPluginInfos();
        List<PluginInfo> groupPluginInfos = pluginInfos.stream()
                .filter(pluginInfo -> !pluginInfo.getType().equals(PluginTypeEnum.PRIVATE.getCode()))
                .toList();
        List<PluginInfo> privatePluginInfos = pluginInfos.stream()
                .filter(pluginInfo -> !pluginInfo.getType().equals(PluginTypeEnum.GROUP.getCode()))
                .toList();
        List<String> pluginNames = StreamUtil.mapping(pluginInfos, PluginInfo::getName);
        List<String> groupPluginNames = StreamUtil.mapping(groupPluginInfos, PluginInfo::getName);
        List<String> privatePluginNames = StreamUtil.mapping(privatePluginInfos, PluginInfo::getName);
        //查询全部机器人群组关系
        List<BotGroupEntity> botGroupEntities = botGroupDao.lambdaQuery().eq(BotGroupEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()).list();
        //更新群插件状态缓存
        botGroupEntities.forEach(botGroupEntity -> {
            String groupPluginStatusKey = XBotRedisConstantKey.X_BOT_GROUP_PLUGIN_STATUS_HASH_KEY(botGroupEntity.getBotId(), botGroupEntity.getGroupId());
            //如果没有则初始化
            if (!redisTemplateClient.hasKey(groupPluginStatusKey)) {
                groupPluginInfos.forEach(pluginInfo -> {
                    redisTemplateClient.putHash(groupPluginStatusKey, pluginInfo.getName(), pluginInfo.getEnable() ? PluginStatusEnum.ENABLED.getCode() : PluginStatusEnum.DISABLED.getCode());
                });
                //更新缓存
            } else {
                Map<Object, Object> statusMap = redisTemplateClient.getEntries(groupPluginStatusKey);
                //如果没有在列表里面则移除缓存
                for (Map.Entry<Object, Object> entry : statusMap.entrySet()) {
                    if (!groupPluginNames.contains((String) entry.getKey())) {
                        redisTemplateClient.deleteHash(groupPluginStatusKey, entry.getKey());
                    }
                }
                //添加新插件
                for (PluginInfo pluginInfo : groupPluginInfos) {
                    if (!statusMap.containsKey(pluginInfo.getName())) {
                        redisTemplateClient.putHash(groupPluginStatusKey, pluginInfo.getName(), pluginInfo.getEnable() ? PluginStatusEnum.ENABLED.getCode() : PluginStatusEnum.DISABLED.getCode());
                    }
                }
            }
        });
        //更新私聊插件状态缓存
        botGroupEntities.forEach(botGroupEntity -> {
            String privatePluginStatusKey = XBotRedisConstantKey.X_BOT_PRIVATE_PLUGIN_STATUS_HASH_KEY + botGroupEntity.getBotId();
            if (!redisTemplateClient.hasKey(privatePluginStatusKey)) {
                privatePluginInfos.forEach(pluginInfo -> {
                    redisTemplateClient.putHash(privatePluginStatusKey, pluginInfo.getName(), pluginInfo.getEnable() ? PluginStatusEnum.ENABLED.getCode() : PluginStatusEnum.DISABLED.getCode());
                });
            } else {
                Map<Object, Object> statusMap = redisTemplateClient.getEntries(privatePluginStatusKey);
                for (Map.Entry<Object, Object> entry : statusMap.entrySet()) {
                    if (!privatePluginNames.contains((String) entry.getKey())) {
                        redisTemplateClient.deleteHash(privatePluginStatusKey, entry.getKey());
                    }
                }
                for (PluginInfo pluginInfo : privatePluginInfos) {
                    if (!statusMap.containsKey(pluginInfo.getName())) {
                        redisTemplateClient.putHash(privatePluginStatusKey, pluginInfo.getName(), pluginInfo.getEnable() ? PluginStatusEnum.ENABLED.getCode() : PluginStatusEnum.DISABLED.getCode());
                    }
                }
            }
        });
    }
}
