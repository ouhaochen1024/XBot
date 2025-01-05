package com.ouhaochen.bot.xbot.commons.security;


public final class ThreadLocalUserInfo {

    // 定义 ThreadLocal 变量，保存当前线程的用户信息
    private static final ThreadLocal<CurrentUserInfo> CURRENT_USER_INFO = ThreadLocal.withInitial(CurrentUserInfo::new);

    public static void setCurrentUserInfo(CurrentUserInfo currentUserInfo) {
        CURRENT_USER_INFO.set(currentUserInfo);
    }

    public static CurrentUserInfo getCurrentUserInfo() {
        return CURRENT_USER_INFO.get();
    }

}
