package com.ouhaochen.bot.xbot.core.plugins;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.MessageHandlerFilter;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.ouhaochen.bot.xbot.commons.enums.DelFlagEnum;
import com.ouhaochen.bot.xbot.commons.enums.TrueOrFalseEnum;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.core.aspect.permission.Permission;
import com.ouhaochen.bot.xbot.core.constants.XBotRedisConstantKey;
import com.ouhaochen.bot.xbot.core.utils.MatcherUtil;
import com.ouhaochen.bot.xbot.db.entity.BotGroupEntity;
import com.ouhaochen.bot.xbot.db.service.BotGroupService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.regex.Matcher;

@Shiro
@Component
@RequiredArgsConstructor
public class BasePlugin {

    private final BotGroupService botGroupService;
    private final RedisTemplateClient redisTemplateClient;

    @Permission(checkGroup = false)
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^打开插件\\s(.*)?$")
    public void openPlugin(Bot bot, AnyMessageEvent event, Matcher matcher) {
        String pluginName = MatcherUtil.getNormalStr(bot, event, matcher);
        if ("BasePlugin".equals(pluginName)) return;
        if (checkPluginNotExist(bot, event, pluginName)) return;
        redisTemplateClient.putHash(XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY, pluginName, TrueOrFalseEnum.TRUE.getCode());
        bot.sendMsg(event, String.format("插件%s已开启", pluginName), false);
    }

    @Permission(checkGroup = false)
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^关闭插件\\s(.*)?$")
    public void closePlugin(Bot bot, AnyMessageEvent event, Matcher matcher) {
        String pluginName = MatcherUtil.getNormalStr(bot, event, matcher);
        if ("BasePlugin".equals(pluginName)) return;
        if (checkPluginNotExist(bot, event, pluginName)) return;
        redisTemplateClient.putHash(XBotRedisConstantKey.X_BOT_PLUGIN_STATUS_HASH_KEY, pluginName, TrueOrFalseEnum.FALSE.getCode());
        bot.sendMsg(event, String.format("插件%s已关闭", pluginName), false);
    }

    @Permission(checkGroup = false)
    @GroupMessageHandler
    @MessageHandlerFilter(cmd = "添加本群使用权限")
    public void addGroup(Bot bot, GroupMessageEvent event) {
        Long groupId = event.getGroupId();
        // 判断是否已经添加
        BotGroupEntity botGroupEntity = botGroupService.lambdaQuery().eq(BotGroupEntity::getGroupId, groupId).eq(BotGroupEntity::getBotId, event.getSelfId()).eq(BotGroupEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()).one();
        if (null != botGroupEntity) {
            bot.sendGroupMsg(event.getGroupId(), "该群已经添加过了", false);
            return;
        }
        // 添加群
        botGroupService.save(new BotGroupEntity().setGroupId(groupId).setBotId(bot.getSelfId()));
        // 构建消息
        String sendMsg = MsgUtils.builder().text(String.format("群%d已经添加成功", groupId)).build();
        // 发送消息
        bot.sendGroupMsg(event.getGroupId(), sendMsg, false);
    }

    @Permission(checkGroup = false)
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^添加群\\s(.*)?\\s使用权限$")
    public void addGroup(Bot bot, AnyMessageEvent event, Matcher matcher) {
        Long groupId = MatcherUtil.getLongStr(bot, event, matcher);
        // 判断是否已经添加
        BotGroupEntity botGroupEntity = botGroupService.lambdaQuery().eq(BotGroupEntity::getGroupId, groupId).eq(BotGroupEntity::getBotId, event.getSelfId()).eq(BotGroupEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()).one();
        if (null != botGroupEntity) {
            bot.sendMsg(event, "该群已经添加过了", false);
            return;
        }
        // 添加群
        botGroupService.save(new BotGroupEntity().setGroupId(groupId).setBotId(bot.getSelfId()));
        // 构建消息
        String sendMsg = MsgUtils.builder().text(String.format("群%d已经添加成功", groupId)).build();
        // 发送消息
        bot.sendMsg(event, sendMsg, false);
    }

    @Permission(checkGroup = false)
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^删除群\\s(.*)?\\s使用权限$")
    public void delGroup(Bot bot, AnyMessageEvent event, Matcher matcher) {
        Long groupId = MatcherUtil.getLongStr(bot, event, matcher);
        BotGroupEntity botGroupEntity = botGroupService.lambdaQuery().eq(BotGroupEntity::getGroupId, groupId).eq(BotGroupEntity::getBotId, event.getSelfId()).eq(BotGroupEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()).one();
        if (null != botGroupEntity) {
            botGroupEntity.setDelFlag(DelFlagEnum.DELETED.getCode());
            botGroupService.updateById(botGroupEntity);
            // 构建消息
            String sendMsg = MsgUtils.builder().text(String.format("群%d已经删除成功", groupId)).build();
            // 发送消息
            bot.sendMsg(event, sendMsg, false);
        } else {
            bot.sendMsg(event, "该群还未添加", false);
        }
    }


    private boolean checkPluginNotExist(Bot bot, AnyMessageEvent event, String pluginName) {
        List<String> pluginList = redisTemplateClient.getSet(XBotRedisConstantKey.X_BOT_PLUGINS_LIST_SET_KEY).stream().map(Object::toString).toList();
        if (!pluginList.contains(pluginName)) {
            bot.sendMsg(event, String.format("插件%s不存在", pluginName), false);
            return true;
        }
        return false;
    }

}
