package com.ouhaochen.bot.xbot.commons.utils;


import com.ouhaochen.bot.xbot.commons.security.ThreadLocalUserInfo;

public final class UserEvnUtil {
    private UserEvnUtil() {
    }
    public static Long getBotId() {
        return null == ThreadLocalUserInfo.getCurrentUserInfo().getBotId() ? 0L : ThreadLocalUserInfo.getCurrentUserInfo().getBotId();
    }
    public static Long getUserId() {
       return null == ThreadLocalUserInfo.getCurrentUserInfo().getUserId() ? 0L : ThreadLocalUserInfo.getCurrentUserInfo().getUserId();
    }
    public static String getUserNickName(String userId) {
        return null == ThreadLocalUserInfo.getCurrentUserInfo().getUserNickName() ? "" : ThreadLocalUserInfo.getCurrentUserInfo().getUserNickName();
    }
    public static Long getGroupId() {
        return null == ThreadLocalUserInfo.getCurrentUserInfo().getGroupId() ? 0L : ThreadLocalUserInfo.getCurrentUserInfo().getGroupId();
    }

}
