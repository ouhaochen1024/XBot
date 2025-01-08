package com.ouhaochen.bot.xbot.core.utils;


import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.message.PrivateMessageEvent;
import lombok.extern.slf4j.Slf4j;
import org.dromara.hutool.core.math.NumberUtil;
import org.dromara.hutool.core.text.StrUtil;

import java.util.regex.Matcher;

@Slf4j
public final class MatcherUtil {

    public static final String WRONG_MSG = "命令格式有误";

    public static Number getNumber(Bot bot, AnyMessageEvent event, Matcher matcher) {
        try {
            String str = matcher.group(1);
            return NumberUtil.parseNumber(str);
        } catch (Exception e) {
            bot.sendMsg(event, WRONG_MSG, false);
            return null;
        }
    }

    public static Number getNumber(Bot bot, PrivateMessageEvent event, Matcher matcher) {
        try {
            String str = matcher.group(1);
            return NumberUtil.parseNumber(str);
        } catch (Exception e) {
            bot.sendPrivateMsg(event.getUserId(), WRONG_MSG, false);
            return null;
        }
    }

    public static Number getNumber(Bot bot, GroupMessageEvent event, Matcher matcher) {
        try {
            String str = matcher.group(1);
            return NumberUtil.parseNumber(str);
        } catch (Exception e) {
            bot.sendGroupMsg(event.getGroupId(), WRONG_MSG, false);
            return null;
        }
    }

    public static String getNormal(Bot bot, AnyMessageEvent event, Matcher matcher) {
        try {
            String str = matcher.group(1);
            if (StrUtil.isBlank(str)) {
                bot.sendMsg(event, WRONG_MSG, false);
                return null;
            }
            return str;
        } catch (Exception e) {
            bot.sendMsg(event, WRONG_MSG, false);
            return null;
        }
    }

    public static String getNormal(Bot bot, PrivateMessageEvent event, Matcher matcher) {
        try {
            String str = matcher.group(1);
            if (StrUtil.isBlank(str)) {
                bot.sendPrivateMsg(event.getUserId(), WRONG_MSG, false);
                return null;
            }
            return str;
        } catch (Exception e) {
            bot.sendPrivateMsg(event.getUserId(), WRONG_MSG, false);
            return null;
        }
    }

    public static String getNormal(Bot bot, GroupMessageEvent event, Matcher matcher) {
        try {
            String str = matcher.group(1);
            if (StrUtil.isBlank(str)) {
                bot.sendGroupMsg(event.getGroupId(), WRONG_MSG, false);
                return null;
            }
            return str;
        } catch (Exception e) {
            bot.sendGroupMsg(event.getGroupId(), "请输入正确的字符串", false);
            return null;
        }
    }
}
