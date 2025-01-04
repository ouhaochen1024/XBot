package com.ouhaochen.bot.xbot.plugins;

import com.mikuac.shiro.annotation.MessageHandlerFilter;
import com.mikuac.shiro.annotation.PrivateMessageHandler;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.PrivateMessageEvent;
import com.ouhaochen.bot.xbot.db.domain.entity.BotGroupEntity;
import com.ouhaochen.bot.xbot.db.service.BotGroupService;
import com.ouhaochen.bot.xbot.plugins.permission.Permission;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.regex.Matcher;

@Shiro
@Component
@RequiredArgsConstructor
public class GroupManagePlugin {

    private final BotGroupService botGroupService;
    public final Permission permission;

    @PrivateMessageHandler
    @MessageHandlerFilter(cmd = "^添加群\\s(.*)?$")
    public void addGroup(Bot bot, PrivateMessageEvent event, Matcher matcher) {
        if (!permission.isSupervisor(event)) return;
        long groupId;
        try {
            groupId = Long.parseLong(matcher.group(1));
        } catch (Exception e) {
            bot.sendPrivateMsg(event.getUserId(), "请输入正确的群号", false);
            return;
        }
        // 判断是否已经添加
        if (botGroupService.lambdaQuery().eq(BotGroupEntity::getGroupId, groupId).eq(BotGroupEntity::getBotId, event.getSelfId()).exists()) {
            bot.sendPrivateMsg(event.getUserId(), "该群已经添加过了", false);
            return;
        }
        // 添加群
        botGroupService.save(new BotGroupEntity().setGroupId(groupId).setBotId(bot.getSelfId()));
        // 构建消息
        String sendMsg = MsgUtils.builder().text(String.format("群%d已经添加成功", groupId)).build();
        // 发送私聊消息
        bot.sendPrivateMsg(event.getUserId(), sendMsg, false);
    }

}
