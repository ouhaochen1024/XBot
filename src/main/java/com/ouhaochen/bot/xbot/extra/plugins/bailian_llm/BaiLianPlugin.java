package com.ouhaochen.bot.xbot.extra.plugins.bailian_llm;

import com.mikuac.shiro.annotation.AnyMessageHandler;
import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.MessageHandlerFilter;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.ouhaochen.bot.xbot.core.aspects.permission.Permission;
import com.ouhaochen.bot.xbot.core.aspects.plugin.Plugin;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import com.ouhaochen.bot.xbot.core.enums.PluginTypeEnum;
import com.ouhaochen.bot.xbot.core.utils.ActionUtil;
import com.ouhaochen.bot.xbot.core.utils.MatcherUtil;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.OnmyojiPluginService;
import lombok.RequiredArgsConstructor;

import java.util.regex.Matcher;

@RequiredArgsConstructor
@Plugin(name = "阿里百炼大模型", author = "ouhaochen", description = "XBot阿里百炼大模型插件", enable = false, type = PluginTypeEnum.COMMON)
public class BaiLianPlugin {

    private final ActionUtil actionUtil;
    private final BaiLianPluginService baiLianPluginService;

    @Permission
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "当前模型")
    public void currentModel(Bot bot, AnyMessageEvent event) {
        BotContext<Object> context = baiLianPluginService.currentModel(bot.getSelfId());
        ActionUtil.sendResponse(bot, event, context);
    }

    @Permission
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "^设置模型\\s(.*)?$")
    public void setModel(Bot bot, AnyMessageEvent event, Matcher matcher) {
        String keyword = MatcherUtil.getNormal(bot, event, matcher);
        if(keyword == null) return;
        BotContext<Object> context = baiLianPluginService.setModel(bot.getSelfId(), keyword);
        ActionUtil.sendResponse(bot, event, context);
    }


}
