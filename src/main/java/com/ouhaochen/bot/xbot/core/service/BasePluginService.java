package com.ouhaochen.bot.xbot.core.service;

import com.ouhaochen.bot.xbot.commons.enums.DelFlagEnum;
import com.ouhaochen.bot.xbot.commons.enums.TrueOrFalseEnum;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.core.constant.XBotRedisConstantKey;
import com.ouhaochen.bot.xbot.core.context.PluginServiceContext;
import com.ouhaochen.bot.xbot.db.dao.BotGroupDao;
import com.ouhaochen.bot.xbot.db.entity.BotGroupEntity;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class BasePluginService {

    private final BotGroupDao botGroupService;
    private final RedisTemplateClient redisTemplateClient;

    private final static String BASE_PLUGIN = "BasePlugin";

    public PluginServiceContext openPlugin(Long botId, String pluginName) {
        if (BASE_PLUGIN.equals(pluginName)) return null;
        PluginServiceContext context = new PluginServiceContext();
        if (checkPluginNotExist(pluginName)) {
            return context.setMsg(String.format("插件%s不存在", pluginName));
        }
        // 存入缓存
        redisTemplateClient.putHash(XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY + botId, pluginName, TrueOrFalseEnum.TRUE.getCode());
        return context.setMsg(String.format("插件%s已开启", pluginName));
    }

    public PluginServiceContext closePlugin(Long botId, String pluginName) {
        if (BASE_PLUGIN.equals(pluginName)) return null;
        PluginServiceContext context = new PluginServiceContext();
        if (checkPluginNotExist(pluginName)) {
            return context.setMsg(String.format("插件%s不存在", pluginName));
        }
        // 存入缓存
        redisTemplateClient.putHash(XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY + botId, pluginName, TrueOrFalseEnum.FALSE.getCode());
        return context.setMsg(String.format("插件%s已关闭", pluginName));
    }

    public PluginServiceContext addGroup(Long botId, Long groupId) {
        PluginServiceContext context = new PluginServiceContext();
        // 判断是否已经添加
        BotGroupEntity botGroupEntity = botGroupService.lambdaQuery().eq(BotGroupEntity::getGroupId, groupId).eq(BotGroupEntity::getBotId, botId).eq(BotGroupEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()).one();
        if (null != botGroupEntity) {
            return context.setMsg(String.format("群%d已经添加过了", groupId));
        }
        // 入库
        botGroupService.save(new BotGroupEntity().setGroupId(groupId).setBotId(botId));
        return context.setMsg(String.format("群%d已经添加成功", groupId));
    }

    public PluginServiceContext delGroup(Long botId, Long groupId) {
        PluginServiceContext context = new PluginServiceContext();
        BotGroupEntity botGroupEntity = botGroupService.lambdaQuery().eq(BotGroupEntity::getGroupId, groupId).eq(BotGroupEntity::getBotId, botId).eq(BotGroupEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()).one();
        if (null != botGroupEntity) {
            botGroupEntity.setDelFlag(DelFlagEnum.DELETED.getCode());
            botGroupService.updateById(botGroupEntity);
            return context.setMsg(String.format("群%d已经删除成功", groupId));
        } else {
            return context.setMsg("该群还未添加");
        }
    }


    private boolean checkPluginNotExist(String pluginName) {
        List<String> pluginList = redisTemplateClient.getSet(XBotRedisConstantKey.X_BOT_PLUGINS_LIST_SET_KEY).stream().map(Object::toString).toList();
        return !pluginList.contains(pluginName);
    }


}
