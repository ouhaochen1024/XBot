package com.ouhaochen.bot.xbot.core.utils;

import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.core.BotContainer;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import lombok.RequiredArgsConstructor;
import org.dromara.hutool.core.text.StrUtil;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public final class ActionUtil {

    private final BotContainer botContainer;

    public static void sendResponse(Bot bot, AnyMessageEvent event, BotContext<?> context) {
        if (context == null || StrUtil.isBlank(context.getMsg())) return;
        bot.sendMsg(event, context.getMsg(), context.getAutoEscape());
        if (context.getVideo() != null) {
            String video = MsgUtils.builder().video(context.getVideo(), context.getCover()).build();
            bot.sendMsg(event, video, context.getAutoEscape());
        }
    }

    public static void sendResponse(Bot bot, GroupMessageEvent event, BotContext<?> context) {
        if (context == null || StrUtil.isBlank(context.getMsg())) return;
        bot.sendGroupMsg(event.getGroupId(), context.getMsg(), context.getAutoEscape());
        if (context.getVideo() != null) {
            String video = MsgUtils.builder().video(context.getVideo(), context.getCover()).build();
            bot.sendGroupMsg(event.getGroupId(), video, context.getAutoEscape());
        }
    }

    public void sendPrivateResponse(Long botId, Long userId, BotContext<?> context) {
        if (context == null || StrUtil.isBlank(context.getMsg())) return;
        Bot bot = botContainer.robots.get(botId);
        bot.sendPrivateMsg(userId, context.getMsg(), context.getAutoEscape());
        if (context.getVideo() != null) {
            String video = MsgUtils.builder().video(context.getVideo(), context.getCover()).build();
            bot.sendGroupMsg(userId, video, context.getAutoEscape());
        }
    }

    public void sendGroupResponse(Long botId, Long groupId, BotContext<?> context) {
        if (context == null || StrUtil.isBlank(context.getMsg())) return;
        Bot bot = botContainer.robots.get(botId);
        bot.sendGroupMsg(groupId, context.getMsg(), context.getAutoEscape());
        if (context.getVideo() != null) {
            String video = MsgUtils.builder().video(context.getVideo(), context.getCover()).build();
            bot.sendGroupMsg(groupId, video, context.getAutoEscape());
        }
    }

    public static void handleGroupAdd(Bot bot, String flag, String subType, boolean approve, String reason) {
        bot.setGroupAddRequest(flag, subType, approve, reason);
    }
}
