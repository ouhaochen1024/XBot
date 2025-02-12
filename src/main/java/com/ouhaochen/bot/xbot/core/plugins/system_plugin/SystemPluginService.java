package com.ouhaochen.bot.xbot.core.plugins.system_plugin;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.ouhaochen.bot.xbot.commons.enums.DelFlagEnum;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.commons.utils.StreamUtil;
import com.ouhaochen.bot.xbot.core.constant.XBotRedisConstantKey;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import com.ouhaochen.bot.xbot.core.enums.PluginStatusEnum;
import com.ouhaochen.bot.xbot.core.info.PluginInfo;
import com.ouhaochen.bot.xbot.core.utils.CommonUtil;
import com.ouhaochen.bot.xbot.db.dao.BotGroupDao;
import com.ouhaochen.bot.xbot.db.entity.BotGroupEntity;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class SystemPluginService {

    @Value("${xbot.plugins.basePackage}")
    private String basePackage;
    private final BotGroupDao botGroupDao;
    private final RedisTemplateClient redisTemplateClient;

    public BotContext<Object> enablePlugin(Long botId, Long groupId, String pluginName) {
        if (checkPluginNotExist(botId, groupId, pluginName)) {
            return BotContext.ofMsg(String.format("插件【%s】不存在", pluginName));
        }
        // 存入缓存
        redisTemplateClient.putHash(XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY(botId, groupId), pluginName, PluginStatusEnum.ENABLED.getCode());
        return BotContext.ofMsg(String.format("插件【%s】已启用", pluginName));
    }

    public BotContext<Object> disablePlugin(Long botId, Long groupId, String pluginName) {
        if (checkPluginNotExist(botId, groupId, pluginName)) {
            return BotContext.ofMsg(String.format("插件【%s】不存在", pluginName));
        }
        // 存入缓存
        redisTemplateClient.putHash(XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY(botId, groupId), pluginName, PluginStatusEnum.DISABLED.getCode());
        return BotContext.ofMsg(String.format("插件【%s】已禁用", pluginName));
    }

    public BotContext<List<PluginInfo>> viewPlugins(Long botId, Long groupId) {
        Map<String, String> pluginStatusMap = redisTemplateClient.hGetAll(XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY(botId, groupId));
        //去掉系统插件
        for (String systemPluginName : StreamUtil.mapping(CommonUtil.getAllSystemPluginInfos(basePackage), PluginInfo::getName)) {
            pluginStatusMap.remove(systemPluginName);
        }
        //转换成<PluginInfo>对象 list key为name value为status
        List<PluginInfo> pluginInfoList = pluginStatusMap.entrySet().stream()
                .map(entry -> PluginInfo.builder().name(entry.getKey()).status(entry.getValue()).build())
                .sorted(Comparator.comparing((PluginInfo info) -> Integer.parseInt(info.getStatus()))
                        .reversed()
                        .thenComparing(PluginInfo::getName))
                .toList();
        //构建消息
        MsgUtils msgUtils = MsgUtils.builder().text("〓 插件列表 〓" + "\n");
        for (PluginInfo pluginInfo : pluginInfoList) {
            msgUtils.text(PluginStatusEnum.getIcon(pluginInfo.getStatus()) + "【" + pluginInfo.getName() + "】" + "\n");
        }
        //总量
        msgUtils.text("共" + pluginStatusMap.size() + "个，启用" + pluginStatusMap.values().stream().filter(s -> s.equals(PluginStatusEnum.ENABLED.getCodeStr())).count() + "个");
        return BotContext.ofData(msgUtils.build(), pluginInfoList);
    }

    public BotContext<Object> addGroup(Long botId, Long groupId) {
        // 判断是否已经添加
        boolean isExist = botGroupDao.exists(new LambdaQueryWrapper<BotGroupEntity>().eq(BotGroupEntity::getGroupId, groupId).eq(BotGroupEntity::getBotId, botId).eq(BotGroupEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()));
        if (isExist) {
            return BotContext.ofMsg(String.format("群【%d】已经添加过了", groupId));
        }
        // 入库
        botGroupDao.save(new BotGroupEntity().setGroupId(groupId).setBotId(botId));
        return BotContext.ofMsg(String.format("群【%d】已经添加成功", groupId));
    }

    public BotContext<Object> delGroup(Long botId, Long groupId) {
        BotGroupEntity botGroupEntity = botGroupDao.lambdaQuery().eq(BotGroupEntity::getGroupId, groupId).eq(BotGroupEntity::getBotId, botId).eq(BotGroupEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()).one();
        if (null != botGroupEntity) {
            botGroupEntity.setDelFlag(DelFlagEnum.DELETED.getCode());
            botGroupDao.updateById(botGroupEntity);
            return BotContext.ofMsg(String.format("群【%d】已经删除成功", groupId));
        } else {
            return BotContext.ofMsg(String.format("群【%d】还未添加", groupId));
        }
    }

    private boolean checkPluginNotExist(Long botId, Long groupId, String pluginName) {
        return !redisTemplateClient.hasHashKey(XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY(botId, groupId), pluginName);
    }
}
