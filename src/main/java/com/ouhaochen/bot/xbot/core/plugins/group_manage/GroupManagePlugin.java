package com.ouhaochen.bot.xbot.core.plugins.group_manage;

import com.mikuac.shiro.annotation.GroupAddRequestHandler;
import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.MessageHandlerFilter;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.request.GroupAddRequestEvent;
import com.ouhaochen.bot.xbot.core.aspects.permission.Permission;
import com.ouhaochen.bot.xbot.core.aspects.plugin.Plugin;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import com.ouhaochen.bot.xbot.core.enums.PluginTypeEnum;
import com.ouhaochen.bot.xbot.core.utils.ActionUtil;
import com.ouhaochen.bot.xbot.core.utils.MatcherUtil;
import lombok.RequiredArgsConstructor;
import org.dromara.hutool.core.thread.ThreadUtil;

import java.util.regex.Matcher;

@RequiredArgsConstructor
@Plugin(name = "群聊管理", author = "ouhaochen", description = "XBot群聊管理插件", type = PluginTypeEnum.GROUP)
public class GroupManagePlugin {

    private final GroupManagePluginService groupManagePluginService;

    @Permission
    @GroupMessageHandler
    @MessageHandlerFilter(cmd = "^添加入群关键词\\s(.*)?$")
    public void addGroupKeyword(Bot bot, GroupMessageEvent event, Matcher matcher) {
        String keyword = MatcherUtil.getNormal(bot, event, matcher);
        if(keyword == null) return;
        BotContext<Object> context = groupManagePluginService.addGroupKeyword(bot.getSelfId(), event.getGroupId(), keyword);
        ActionUtil.sendResponse(bot, event, context);
    }

    @Permission
    @GroupMessageHandler
    @MessageHandlerFilter(cmd = "^删除入群关键词\\s(.*)?$")
    public void delGroupKeyword(Bot bot, GroupMessageEvent event, Matcher matcher) {
        String keyword = MatcherUtil.getNormal(bot, event, matcher);
        if(keyword == null) return;
        BotContext<Object> context = groupManagePluginService.delGroupKeyword(bot.getSelfId(), event.getGroupId(), keyword);
        ActionUtil.sendResponse(bot, event, context);
    }

    @Permission(checkUser = false)
    @GroupAddRequestHandler
    public void handleAddGroup(Bot bot, GroupAddRequestEvent event){
        BotContext<Object> context = groupManagePluginService.handleAddGroup(bot.getSelfId(), event.getUserId(), event.getGroupId(), event.getComment());
        ActionUtil.handleGroupAdd(bot, event.getFlag(), event.getSubType(), context.getApprove(), context.getApproveReason());
        if (context.getApprove()) {
            ThreadUtil.sleep(1000);
            GroupMessageEvent groupMessageEvent = new GroupMessageEvent();
            groupMessageEvent.setGroupId(event.getGroupId());
            ActionUtil.sendResponse(bot, groupMessageEvent, context);
        }
    }

}
