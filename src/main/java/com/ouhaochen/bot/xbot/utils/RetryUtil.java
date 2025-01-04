package com.ouhaochen.bot.xbot.utils;

/**
 * 请求重试工具
 */
public class RetryUtil {
    public static <T> T retry(Function<T> fun, Integer times) {
        try {
            return fun.execute();
        } catch (Exception e) {
            if (times > 1) {
                return retry(fun, --times);
            } else {
                throw new RuntimeException(e);
            }
        }
    }

    public static <T> T retry(Function<T> fun) {
        int times = 3;
        return retry(fun, times);
    }

    @FunctionalInterface
    public interface Function<T> {
        T execute();
    }
}
