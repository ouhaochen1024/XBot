package com.ouhaochen.bot.xbot.commons.utils;


import com.ouhaochen.bot.xbot.commons.security.ThreadLocalUserInfo;

public final class UserEvnUtil {
    private UserEvnUtil() {
    }
    public static Long getBotId() {
        return ThreadLocalUserInfo.getCurrentUserInfo().getBotId();
    }
    public static Long getUserId() {
       return ThreadLocalUserInfo.getCurrentUserInfo().getUserId();
    }
    public static String getUserNickName(String userId) {
        return ThreadLocalUserInfo.getCurrentUserInfo().getUserNickName();
    }
    public static Long getGroupId() {
        return ThreadLocalUserInfo.getCurrentUserInfo().getGroupId();
    }

}
