package com.ouhaochen.bot.xbot.core.utils;

import com.mikuac.shiro.common.utils.MsgUtils;
import com.mikuac.shiro.common.utils.ShiroUtils;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.core.BotContainer;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.message.PrivateMessageEvent;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import lombok.RequiredArgsConstructor;
import org.dromara.hutool.core.text.StrUtil;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Component
@RequiredArgsConstructor
public final class ActionUtil {

    private final BotContainer botContainer;

    public static void sendResponse(Bot bot, AnyMessageEvent event, BotContext<?> context) {
        if (context == null || StrUtil.isBlank(context.getMsg())) return;
        if (context.getVideo() != null) {
            String video = MsgUtils.builder().video(context.getVideo(), context.getCover()).build();
            if(event.getGroupId() != null) {
                sendGroupForwardMsg(bot, context.getMsg(), video);
            } else {
                sendPrivateForwardMsg(bot, context.getMsg(), video);
            }
        } else {
            bot.sendMsg(event, context.getMsg(), context.getAutoEscape());
        }
    }

    public static void sendResponse(Bot bot, GroupMessageEvent event, BotContext<?> context) {
        if (context == null || StrUtil.isBlank(context.getMsg())) return;
        if (context.getVideo() != null) {
            String video = MsgUtils.builder().video(context.getVideo(), context.getCover()).build();
            sendGroupForwardMsg(bot, context.getMsg(), video);
        } else {
            bot.sendGroupMsg(event.getGroupId(), context.getMsg(), context.getAutoEscape());
        }
    }

    public static void sendResponse(Bot bot, PrivateMessageEvent event, BotContext<?> context) {
        if (context == null || StrUtil.isBlank(context.getMsg())) return;
        if (context.getVideo() != null) {
            String video = MsgUtils.builder().video(context.getVideo(), context.getCover()).build();
            sendPrivateForwardMsg(bot, context.getMsg(), video);
        } else {
            bot.sendPrivateMsg(event.getUserId(), context.getMsg(), context.getAutoEscape());
        }
    }

    public void sendPrivateResponse(Long botId, Long userId, BotContext<?> context) {
        if (context == null || StrUtil.isBlank(context.getMsg())) return;
        Bot bot = botContainer.robots.get(botId);
        if (context.getVideo() != null) {
            String video = MsgUtils.builder().video(context.getVideo(), context.getCover()).build();
            sendPrivateForwardMsg(bot, context.getMsg(), video);
        } else {
            bot.sendPrivateMsg(userId, context.getMsg(), context.getAutoEscape());
        }
    }

    public void sendGroupResponse(Long botId, Long groupId, BotContext<?> context) {
        if (context == null || StrUtil.isBlank(context.getMsg())) return;
        Bot bot = botContainer.robots.get(botId);
        if (context.getVideo() != null) {
            String video = MsgUtils.builder().video(context.getVideo(), context.getCover()).build();
            sendGroupForwardMsg(bot, context.getMsg(), video);
        } else {
            bot.sendGroupMsg(groupId, context.getMsg(), context.getAutoEscape());
        }
    }

    public static void handleGroupAdd(Bot bot, String flag, String subType, boolean approve, String reason) {
        bot.setGroupAddRequest(flag, subType, approve, reason);
    }

    public static void sendGroupForwardMsg(Bot bot,  String... msg) {
        List<Map<String, Object>> forwardMsg = ShiroUtils.generateForwardMsg(bot, List.of(msg));
        bot.sendGroupForwardMsg(bot.getSelfId(), forwardMsg);
    }

    public static void sendPrivateForwardMsg(Bot bot,  String... msg) {
        List<Map<String, Object>> forwardMsg = ShiroUtils.generateForwardMsg(bot, List.of(msg));
        bot.sendPrivateForwardMsg(bot.getSelfId(), forwardMsg);
    }
}
