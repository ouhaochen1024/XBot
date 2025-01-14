package com.ouhaochen.bot.xbot.core.utils;


import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.ouhaochen.bot.xbot.core.context.PluginServiceContext;
import org.dromara.hutool.core.text.StrUtil;

public final class SendMsgUtil {

    public static void sendResponse(Bot bot, AnyMessageEvent event, PluginServiceContext context) {
        if (StrUtil.isBlank(context.getMsg())) return;
        bot.sendMsg(event, context.getMsg(), context.getAutoEscape());
    }

    public static void sendResponse(Bot bot, GroupMessageEvent event, PluginServiceContext context) {
        if (StrUtil.isBlank(context.getMsg())) return;
        bot.sendGroupMsg(event.getGroupId(), context.getMsg(), context.getAutoEscape());
    }

}
