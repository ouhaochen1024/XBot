package com.ouhaochen.bot.xbot.commons.utils;

import org.dromara.hutool.core.data.id.IdUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Random;

public final class ShortUUIDUtil {
    private static final Logger logger = LoggerFactory.getLogger(ShortUUIDUtil.class);
    private static final String[] CHARS = new String[]{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
            "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "0", "1", "2", "3", "4", "5", "6", "7", "8",
            "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T",
            "U", "V", "W", "X", "Y", "Z"};
    private static final String[] NUMBERS = new String[]{"0", "1", "2", "3", "4", "5", "6", "7", "8", "9"};
    private static final int MIN_DIGITS = 4;
    private static final Random RANDOM = new Random();

    // 获得9位UUID短号
    public static String create() {
        return create(9);
    }

    // 获得n位UUID短号 最小4位
    public static String create(int digits) {
        return create(digits, false);
    }

    // 获得n位UUID短号 最小4位 字母是否全大写
    public static String create(int digits, boolean isUpperCase) {
        if (digits < MIN_DIGITS) {
            logger.warn("请求的位数 {} 小于最小允许值 {}，使用最小允许值。", digits, MIN_DIGITS);
            digits = MIN_DIGITS;
        }
        StringBuilder shortBuffer = new StringBuilder();
        String uuid = IdUtil.randomUUID().replace("-", "");
        for (int i = 0; i < digits; i++) {
            if (i < 8) {
                String str = uuid.substring(i * 4, i * 4 + 4);
                int x = Integer.parseInt(str, 16);
                shortBuffer.append(CHARS[x % 0x3E]);
            } else {
                shortBuffer.append(CHARS[RANDOM.nextInt(61)]);
            }
        }
        return isUpperCase ? shortBuffer.toString().toUpperCase() : shortBuffer.toString();
    }

    // 获得n位数字ID 最小4位
    public static String createNumberId(int digits) {
        if (digits < MIN_DIGITS) {
            logger.warn("请求的位数 {} 小于最小允许值 {}，使用最小允许值。", digits, MIN_DIGITS);
            digits = MIN_DIGITS;
        }
        StringBuilder shortBuffer = new StringBuilder();
        for (int i = 0; i < digits; i++) {
            int x = RANDOM.nextInt(10);
            shortBuffer.append(NUMBERS[x]);
        }
        return shortBuffer.toString();
    }
}
