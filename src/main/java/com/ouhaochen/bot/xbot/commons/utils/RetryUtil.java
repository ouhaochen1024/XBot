package com.ouhaochen.bot.xbot.commons.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 请求重试工具
 */
public final class RetryUtil {
    private static final Logger logger = LoggerFactory.getLogger(org.dromara.hutool.core.thread.RetryUtil.class);
    private static final int DEFAULT_RETRY_TIMES = 3;
    private static final long RETRY_INTERVAL_MS = 1000L; // 1 second

    public static <T> T retry(Function<T> fun, Integer times) {
        return retryWithInterval(fun, times, RETRY_INTERVAL_MS);
    }

    public static <T> T retry(Function<T> fun) {
        return retry(fun, DEFAULT_RETRY_TIMES);
    }

    public static void retry(VoidFunction fun, Integer times) {
        retryWithInterval(fun, times, RETRY_INTERVAL_MS);
    }

    public static void retry(VoidFunction fun) {
        retry(fun, DEFAULT_RETRY_TIMES);
    }

    private static <T> T retryWithInterval(Function<T> fun, Integer times, long interval) {
        try {
            return fun.execute();
        } catch (Exception e) {
            if (times > 1) {
                logger.warn("重试失败，正在重试...剩余的尝试次数： {}", times - 1, e);
                try {
                    Thread.sleep(interval);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    throw new RuntimeException("重试期间线程中断", ie);
                }
                return retryWithInterval(fun, --times, interval);
            } else {
                logger.error("所有重试尝试均失败", e);
                throw new RuntimeException("所有重试尝试均失败", e);
            }
        }
    }

    private static void retryWithInterval(VoidFunction fun, Integer times, long interval) {
        try {
            fun.execute();
        } catch (Exception e) {
            if (times > 1) {
                logger.warn("重试失败，正在重试...剩余的尝试次数：  {}", times - 1, e);
                try {
                    Thread.sleep(interval);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    throw new RuntimeException("重试期间线程中断", ie);
                }
                retryWithInterval(fun, --times, interval);
            } else {
                logger.error("所有重试尝试均失败", e);
                throw new RuntimeException("所有重试尝试均失败", e);
            }
        }
    }

    @FunctionalInterface
    public interface Function<T> {
        T execute() throws Exception;
    }

    @FunctionalInterface
    public interface VoidFunction {
        void execute() throws Exception;
    }
}
