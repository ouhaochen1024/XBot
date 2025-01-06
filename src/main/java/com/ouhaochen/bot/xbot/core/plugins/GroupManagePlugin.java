package com.ouhaochen.bot.xbot.core.plugins;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.GroupAddRequestHandler;
import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.MessageHandlerFilter;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.request.GroupAddRequestEvent;
import com.ouhaochen.bot.xbot.commons.enums.DelFlagEnum;
import com.ouhaochen.bot.xbot.core.aspect.Permission;
import com.ouhaochen.bot.xbot.core.utils.MatcherUtil;
import com.ouhaochen.bot.xbot.db.entity.BotGroupEntity;
import com.ouhaochen.bot.xbot.db.service.BotGroupService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.regex.Matcher;

@Shiro
@Component
@RequiredArgsConstructor
public class GroupManagePlugin {

    private final BotGroupService botGroupService;

    @Permission(checkGroup = false)
    @GroupMessageHandler
    @MessageHandlerFilter(cmd = "添加本群")
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
    @MessageHandlerFilter(cmd = "^添加群\\s(.*)?$")
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
    @MessageHandlerFilter(cmd = "^删除群\\s(.*)?$")
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

    @Permission(checkUser = false)
    @GroupAddRequestHandler
    public void handleAddGroup(Bot bot, GroupAddRequestEvent event){
        while (true) {
            botGroupService.isGroupManager(event.getUserId(), event.getGroupId());
        }
    }


}
