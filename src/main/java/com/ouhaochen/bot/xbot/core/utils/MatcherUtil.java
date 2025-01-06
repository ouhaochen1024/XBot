package com.ouhaochen.bot.xbot.core.utils;


import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.AnyMessageEvent;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.mikuac.shiro.dto.event.message.PrivateMessageEvent;
import lombok.extern.slf4j.Slf4j;
import org.dromara.hutool.core.text.StrUtil;

import java.util.regex.Matcher;

@Slf4j
public final class MatcherUtil {

    public static Long getLongStr(Bot bot, AnyMessageEvent event, Matcher matcher) {
        try {
            String str = matcher.group(1);
            return StrUtil.isNumeric(str) ? Long.parseLong(str) : null;
        } catch (Exception e) {
            log.error("获取数字失败", e);
            bot.sendMsg(event, "请输入正确的数字", false);
            return null;
        }
    }

    public static Long getLongStr(Bot bot, PrivateMessageEvent event, Matcher matcher) {
        try {
            String str = matcher.group(1);
            return StrUtil.isNumeric(str) ? Long.parseLong(str) : null;
        } catch (Exception e) {
            log.error("获取数字失败", e);
            bot.sendPrivateMsg(event.getUserId(), "请输入正确的数字", false);
            return null;
        }
    }

    public static Long getLongStr(Bot bot, GroupMessageEvent event, Matcher matcher) {
        try {
            String str = matcher.group(1);
            return StrUtil.isNumeric(str) ? Long.parseLong(str) : null;
        } catch (Exception e) {
            log.error("获取数字失败", e);
            bot.sendGroupMsg(event.getGroupId(), "请输入正确的数字", false);
            return null;
        }
    }

    public static String getNormalStr(Bot bot, AnyMessageEvent event, Matcher matcher) {
        try {
            String str = matcher.group(1);
            return StrUtil.isNotBlank(str) ? str : null;
        } catch (Exception e) {
            log.error("获取字符串失败", e);
            bot.sendMsg(event, "请输入正确的字符串", false);
            return null;
        }
    }

    public static String getNormalStr(Bot bot, PrivateMessageEvent event, Matcher matcher) {
        try {
            String str = matcher.group(1);
            return StrUtil.isNotBlank(str) ? str : null;
        } catch (Exception e) {
            log.error("获取字符串失败", e);
            bot.sendPrivateMsg(event.getUserId(), "请输入正确的字符串", false);
            return null;
        }
    }

    public static String getNormalStr(Bot bot, GroupMessageEvent event, Matcher matcher) {
        try {
            String str = matcher.group(1);
            return StrUtil.isNotBlank(str) ? str : null;
        } catch (Exception e) {
            log.error("获取字符串失败", e);
            bot.sendGroupMsg(event.getGroupId(), "请输入正确的字符串", false);
            return null;
        }
    }
}
