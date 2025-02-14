package com.ouhaochen.bot.xbot.core.plugins.system;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.ouhaochen.bot.xbot.commons.enums.DelFlagEnum;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.commons.utils.StreamUtil;
import com.ouhaochen.bot.xbot.core.constant.XBotRedisConstantKey;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import com.ouhaochen.bot.xbot.core.enums.PluginStatusEnum;
import com.ouhaochen.bot.xbot.core.enums.PluginTypeEnum;
import com.ouhaochen.bot.xbot.core.info.PluginInfo;
import com.ouhaochen.bot.xbot.core.loader.PluginLoader;
import com.ouhaochen.bot.xbot.core.utils.CommonUtil;
import com.ouhaochen.bot.xbot.db.dao.BotGroupDao;
import com.ouhaochen.bot.xbot.db.entity.BotGroupEntity;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class SystemPluginService {

    private final BotGroupDao botGroupDao;
    private final PluginLoader pluginLoader;
    private final RedisTemplateClient redisTemplateClient;

    public BotContext<Object> enablePlugin(Long botId, Long groupId, String pluginName) {
        // 获得插件信息
        PluginInfo pluginInfo = CommonUtil.getPluginInfo(pluginName);
        if (pluginInfo == null) {
            return BotContext.ofMsg(String.format("插件【%s】不存在", pluginName));
        }
        // 判断类型
        if (PluginTypeEnum.SYSTEM.getCode().equals(pluginInfo.getType()) || (groupId == null && PluginTypeEnum.GROUP.getCode().equals(pluginInfo.getType())) || (groupId != null && PluginTypeEnum.PRIVATE.getCode().equals(pluginInfo.getType()))) {
            return new BotContext<>(null);
        } else if (PluginTypeEnum.GROUP.getCode().equals(pluginInfo.getType()) || (PluginTypeEnum.COMMON.getCode().equals(pluginInfo.getType()) && groupId != null)) {
            redisTemplateClient.putHash(XBotRedisConstantKey.X_BOT_GROUP_PLUGIN_STATUS_HASH_KEY(botId, groupId), pluginName, PluginStatusEnum.ENABLED.getCode());
        } else {
            redisTemplateClient.putHash(XBotRedisConstantKey.X_BOT_PRIVATE_PLUGIN_STATUS_HASH_KEY + botId, pluginName, PluginStatusEnum.ENABLED.getCode());
        }
        return BotContext.ofMsg(String.format("插件【%s】已启用", pluginName));
    }

    public BotContext<Object> disablePlugin(Long botId, Long groupId, String pluginName) {
        // 获得插件信息
        PluginInfo pluginInfo = CommonUtil.getPluginInfo(pluginName);
        if (pluginInfo == null) {
            return BotContext.ofMsg(String.format("插件【%s】不存在", pluginName));
        }
        // 判断类型
        if (PluginTypeEnum.SYSTEM.getCode().equals(pluginInfo.getType()) || (PluginTypeEnum.GROUP.getCode().equals(pluginInfo.getType()) && groupId == null) || (PluginTypeEnum.PRIVATE.getCode().equals(pluginInfo.getType()) && groupId != null)) {
            return new BotContext<>(null);
        } else if (PluginTypeEnum.GROUP.getCode().equals(pluginInfo.getType()) || (PluginTypeEnum.COMMON.getCode().equals(pluginInfo.getType()) && groupId != null)) {
            redisTemplateClient.putHash(XBotRedisConstantKey.X_BOT_GROUP_PLUGIN_STATUS_HASH_KEY(botId, groupId), pluginName, PluginStatusEnum.DISABLED.getCode());
        } else {
            redisTemplateClient.putHash(XBotRedisConstantKey.X_BOT_PRIVATE_PLUGIN_STATUS_HASH_KEY + botId, pluginName, PluginStatusEnum.DISABLED.getCode());
        }
        return BotContext.ofMsg(String.format("插件【%s】已禁用", pluginName));
    }

    public BotContext<List<PluginInfo>> viewPlugins(Long botId, Long groupId) {
        Map<Object, Object> pluginStatusMap;
        if (groupId == null) {
            pluginStatusMap = redisTemplateClient.getEntries(XBotRedisConstantKey.X_BOT_PRIVATE_PLUGIN_STATUS_HASH_KEY + botId);
        } else {
            pluginStatusMap = redisTemplateClient.getEntries(XBotRedisConstantKey.X_BOT_GROUP_PLUGIN_STATUS_HASH_KEY(botId, groupId));
        }
        //去掉系统插件
        for (String systemPluginName : StreamUtil.mapping(CommonUtil.getAllSystemPluginInfos(), PluginInfo::getName)) {
            pluginStatusMap.remove(systemPluginName);
        }
        //转换成<PluginInfo>对象 list key为name value为status
        List<PluginInfo> pluginInfoList = pluginStatusMap.entrySet().stream().map(entry -> PluginInfo.builder().name((String) entry.getKey()).status((Integer) entry.getValue()).build()).sorted(Comparator.comparing((PluginInfo info) -> Integer.parseInt(String.valueOf(info.getStatus()))).reversed().thenComparing(PluginInfo::getName)).toList();
        //构建消息
        MsgUtils msgUtils = MsgUtils.builder().text("〓 插件列表 〓" + "\n");
        for (PluginInfo pluginInfo : pluginInfoList) {
            msgUtils.text(PluginStatusEnum.getIcon(pluginInfo.getStatus()) + "【" + pluginInfo.getName() + "】" + "\n");
        }
        //总量
        msgUtils.text("共" + pluginStatusMap.size() + "个，启用" + pluginStatusMap.values().stream().filter(s -> PluginStatusEnum.ENABLED.getCode().equals(Integer.valueOf(s.toString()))).count() + "个");
        return BotContext.ofData(msgUtils.build(), pluginInfoList);
    }

    public BotContext<Object> addGroup(Long botId, Long groupId) {
        // 判断是否已经添加
        boolean isExist = botGroupDao.exists(new LambdaQueryWrapper<BotGroupEntity>().eq(BotGroupEntity::getGroupId, groupId).eq(BotGroupEntity::getBotId, botId).eq(BotGroupEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()));
        if (isExist) {
            return BotContext.ofMsg(String.format("群【%d】已经添加过了", groupId));
        }
        // 关系入库
        botGroupDao.save(new BotGroupEntity().setGroupId(groupId).setBotId(botId));
        // 重新加载插件
        pluginLoader.loadPlugins();
        return BotContext.ofMsg(String.format("群【%d】已经添加成功", groupId));
    }

    public BotContext<Object> delGroup(Long botId, Long groupId) {
        BotGroupEntity botGroupEntity = botGroupDao.lambdaQuery().eq(BotGroupEntity::getGroupId, groupId).eq(BotGroupEntity::getBotId, botId).eq(BotGroupEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()).one();
        if (null != botGroupEntity) {
            botGroupEntity.setDelFlag(DelFlagEnum.DELETED.getCode());
            botGroupDao.updateById(botGroupEntity);
            // 重新加载插件
            pluginLoader.loadPlugins();
            return BotContext.ofMsg(String.format("群【%d】已经删除成功", groupId));
        } else {
            return BotContext.ofMsg(String.format("群【%d】还未添加", groupId));
        }
    }
}
