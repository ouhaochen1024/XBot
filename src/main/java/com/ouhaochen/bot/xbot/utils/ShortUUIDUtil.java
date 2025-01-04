package com.ouhaochen.bot.xbot.utils;


import org.dromara.hutool.core.data.id.UUID;

import java.util.Random;

public final class ShortUUIDUtil {
    public static String[] chars = new String[]{"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
            "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "0", "1", "2", "3", "4", "5", "6", "7", "8",
            "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T",
            "U", "V", "W", "X", "Y", "Z"};

    public static String[] numbers = new String[]{ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"};


    //获得9位UUID短号
    public static String create() {
        StringBuilder shortBuffer = new StringBuilder();
        String uuid = UUID.randomUUID().toString().replace("-", "");
        for (int i = 0; i < 8; i++) {
            String str = uuid.substring(i * 4, i * 4 + 4);
            int x = Integer.parseInt(str, 16);
            shortBuffer.append(chars[x % 0x3E]);
        }
        shortBuffer.append(chars[new Random().nextInt(61)]);
        return shortBuffer.toString();
    }

    //获得n位UUID短号 最小4位
    public static String create(int digits) {
        if (digits < 4) digits = 4;
        StringBuilder shortBuffer = new StringBuilder();
        String uuid = UUID.randomUUID().toString().replace("-", "");
        for (int i = 0; i < digits; i++) {
            String str = uuid.substring(i * 4, i * 4 + 4);
            int x = Integer.parseInt(str, 16);
            shortBuffer.append(chars[x % 0x3E]);
        }
        return shortBuffer.toString();
    }

    //获得n位UUID短号 最小4位 字母是否全大写
    public static String create(int digits, boolean isUpperCase) {
        if (digits < 4) digits = 4;
        StringBuilder shortBuffer = new StringBuilder();
        String uuid = UUID.randomUUID().toString().replace("-", "");
        for (int i = 0; i < digits; i++) {
            String str = uuid.substring(i * 4, i * 4 + 4);
            int x = Integer.parseInt(str, 16);
            shortBuffer.append(chars[x % 0x3E]);
        }
        if (isUpperCase) {
            return shortBuffer.toString().toUpperCase();
        } else {
            return shortBuffer.toString();
        }
    }

    public static String createNumberId(int digits) {
        if (digits < 4) digits = 4;
        StringBuilder shortBuffer = new StringBuilder();
        for (int i = 0; i < digits; i++) {
            Random random = new Random();
            int x = random.nextInt(10);
            shortBuffer.append(numbers[x]);
        }
        return shortBuffer.toString();
    }

}
