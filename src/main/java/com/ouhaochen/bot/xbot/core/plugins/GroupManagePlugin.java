package com.ouhaochen.bot.xbot.core.plugins;

import com.mikuac.shiro.annotation.GroupAddRequestHandler;
import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.MessageHandlerFilter;
import com.mikuac.shiro.annotation.common.Shiro;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.request.GroupAddRequestEvent;
import com.ouhaochen.bot.xbot.core.aspects.permission.Permission;
import com.ouhaochen.bot.xbot.core.aspects.plugin.Plugin;
import com.ouhaochen.bot.xbot.core.context.PluginServiceContext;
import com.ouhaochen.bot.xbot.core.service.GroupManageService;
import com.ouhaochen.bot.xbot.core.utils.ActionUtil;
import com.ouhaochen.bot.xbot.core.utils.MatcherUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.regex.Matcher;

@Shiro
@Component
@RequiredArgsConstructor
@Plugin(name = "群聊管理", author = "ouhaochen", description = "XBot群聊管理插件")
public class GroupManagePlugin {

    private final GroupManageService groupManageService;

    @Permission
    @GroupMessageHandler
    @MessageHandlerFilter(cmd = "^添加入群关键词\\s(.*)?$")
    public void addGroupKeyword(Bot bot, GroupMessageEvent event, Matcher matcher) {
        String keyword = MatcherUtil.getNormal(bot, event, matcher);
        PluginServiceContext context = groupManageService.addGroupKeyword(bot.getSelfId(), event.getGroupId(), keyword);
        ActionUtil.sendResponse(bot, event, context);
    }

    @Permission
    @GroupMessageHandler
    @MessageHandlerFilter(cmd = "^删除入群关键词\\s(.*)?$")
    public void delGroupKeyword(Bot bot, GroupMessageEvent event, Matcher matcher) {
        String keyword = MatcherUtil.getNormal(bot, event, matcher);
        PluginServiceContext context = groupManageService.delGroupKeyword(bot.getSelfId(), event.getGroupId(), keyword);
        ActionUtil.sendResponse(bot, event, context);
    }

    @Permission(checkUser = false)
    @GroupAddRequestHandler
    public void handleAddGroup(Bot bot, GroupAddRequestEvent event){
        PluginServiceContext context = groupManageService.handleAddGroup(bot.getSelfId(), event.getGroupId(), event.getComment());
        ActionUtil.handleGroupAdd(bot, event.getFlag(), event.getSubType(), context.getApprove(), context.getApproveReason());
    }

}
