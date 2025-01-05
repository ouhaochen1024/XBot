package com.ouhaochen.bot.xbot.core.plugins;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.MessageHandlerFilter;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.ouhaochen.bot.xbot.core.aspect.Permission;
import com.ouhaochen.bot.xbot.db.domain.entity.BotGroupEntity;
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
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^添加群\\s(.*)?$")
    public void addGroup(Bot bot, AnyMessageEvent event, Matcher matcher) {
        long groupId;
        try {
            groupId = Long.parseLong(matcher.group(1));
        } catch (Exception e) {
            bot.sendMsg(event, "请输入正确的群号", false);
            return;
        }
        // 判断是否已经添加
        if (botGroupService.lambdaQuery().eq(BotGroupEntity::getGroupId, groupId).eq(BotGroupEntity::getBotId, event.getSelfId()).exists()) {
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
}
