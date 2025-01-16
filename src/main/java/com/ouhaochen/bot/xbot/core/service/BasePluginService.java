package com.ouhaochen.bot.xbot.core.service;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.ouhaochen.bot.xbot.commons.enums.DelFlagEnum;
import com.ouhaochen.bot.xbot.commons.enums.TrueOrFalseEnum;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.core.constant.XBotRedisConstantKey;
import com.ouhaochen.bot.xbot.core.context.PluginServiceContext;
import com.ouhaochen.bot.xbot.core.enums.PluginStatusEnum;
import com.ouhaochen.bot.xbot.core.utils.CommonUtil;
import com.ouhaochen.bot.xbot.db.dao.BotGroupDao;
import com.ouhaochen.bot.xbot.db.entity.BotGroupEntity;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class BasePluginService {

    @Value("${xbot.plugins.basePackage}")
    private String basePackage;
    private final BotGroupDao botGroupDao;
    private final RedisTemplateClient redisTemplateClient;

    public PluginServiceContext enablePlugin(Long botId, String pluginName) {
        if (pluginName == null || CommonUtil.getPluginExclude(basePackage).contains(pluginName)) return null;
        PluginServiceContext context = new PluginServiceContext();
        if (checkPluginNotExist(pluginName)) {
            return context.setMsg(String.format("插件【%s】不存在", pluginName));
        }
        // 存入缓存
        redisTemplateClient.putHash(XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY + botId, pluginName, TrueOrFalseEnum.TRUE.getCode());
        return context.setMsg(String.format("插件【%s】已启用", pluginName));
    }

    public PluginServiceContext disablePlugin(Long botId, String pluginName) {
        if (pluginName == null || CommonUtil.getPluginExclude(basePackage).contains(pluginName)) return null;
        PluginServiceContext context = new PluginServiceContext();
        if (checkPluginNotExist(pluginName)) {
            return context.setMsg(String.format("插件【%s】不存在", pluginName));
        }
        // 存入缓存
        redisTemplateClient.putHash(XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY + botId, pluginName, TrueOrFalseEnum.FALSE.getCode());
        return context.setMsg(String.format("插件【%s】已禁用", pluginName));
    }

    public PluginServiceContext viewPlugins(long botId) {
        //检查目前插件状态 没有在插件列表的插件默认给开启的缓存
        List<String> pluginList = redisTemplateClient.getSet(XBotRedisConstantKey.X_BOT_PLUGINS_LIST_SET_KEY).stream().map(Object::toString).toList();
        for (String pluginName : pluginList) {
            if (!redisTemplateClient.hasHashKey(XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY + botId, pluginName)) {
                redisTemplateClient.putHash(XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY + botId, pluginName, TrueOrFalseEnum.TRUE.getCode());
            }
        }
        //正式查询状态
        PluginServiceContext context = new PluginServiceContext();
        Map<String, String> pluginStatusMap = redisTemplateClient.hGetAll(XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY + botId);
        for(String exclude : CommonUtil.getPluginExclude(basePackage)){
            pluginStatusMap.remove(exclude);
        }
        StringBuilder sb = new StringBuilder();
        for (Map.Entry<String, String> pluginStatus : pluginStatusMap.entrySet()) {
            sb.append(pluginStatus.getKey()).append("：").append(PluginStatusEnum.getMessage(Integer.valueOf((pluginStatus.getValue()))));
           //判断有没有下一个元素再拼接换行
            if (pluginStatusMap.entrySet().stream().toList().indexOf(pluginStatus) != pluginStatusMap.size() - 1) {
                sb.append("\n");
            }
        }
        return context.setMsg(sb.toString());
    }

    public PluginServiceContext addGroup(Long botId, Long groupId) {
        PluginServiceContext context = new PluginServiceContext();
        // 判断是否已经添加
        boolean isExist = botGroupDao.exists(new LambdaQueryWrapper<BotGroupEntity>().eq(BotGroupEntity::getGroupId, groupId).eq(BotGroupEntity::getBotId, botId).eq(BotGroupEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()));
        if (isExist) {
            return context.setMsg(String.format("群【%d】已经添加过了", groupId));
        }
        // 入库
        botGroupDao.save(new BotGroupEntity().setGroupId(groupId).setBotId(botId));
        return context.setMsg(String.format("群【%d】已经添加成功", groupId));
    }

    public PluginServiceContext delGroup(Long botId, Long groupId) {
        PluginServiceContext context = new PluginServiceContext();
        BotGroupEntity botGroupEntity = botGroupDao.lambdaQuery().eq(BotGroupEntity::getGroupId, groupId).eq(BotGroupEntity::getBotId, botId).eq(BotGroupEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()).one();
        if (null != botGroupEntity) {
            botGroupEntity.setDelFlag(DelFlagEnum.DELETED.getCode());
            botGroupDao.updateById(botGroupEntity);
            return context.setMsg(String.format("群【%d】已经删除成功", groupId));
        } else {
            return context.setMsg(String.format("群【%d】还未添加", groupId));
        }
    }

    private boolean checkPluginNotExist(String pluginName) {
        List<String> pluginList = redisTemplateClient.getSet(XBotRedisConstantKey.X_BOT_PLUGINS_LIST_SET_KEY).stream().map(Object::toString).toList();
        return !pluginList.contains(pluginName);
    }
}
