package com.ouhaochen.bot.xbot.commons.enums;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum DateFormatEnum {
    NORM_YEAR("yyyy"),
    NORM_ONLY_MONTH("MM"),
    NORM_ONLY_DAY("dd"),
    NORM_MONTH("yyyy-MM"),
    SIMPLE_MONTH("yyyyMM"),
    NORM_DATE("yyyy-MM-dd"),
    NORM_TIME("HH:mm:ss"),
    NORM_DATETIME_MINUTE("yyyy-MM-dd HH:mm"),
    NORM_DATETIME("yyyy-MM-dd HH:mm:ss"),
    NORM_DATETIME_MS("yyyy-MM-dd HH:mm:ss.SSS"),
    NORM_DATETIME_COMMA_MS("yyyy-MM-dd HH:mm:ss,SSS"),
    CHINESE_DATE("yyyy年MM月dd日"),
    CHINESE_DATE_TIME("yyyy年MM月dd日HH时mm分ss秒"),
    PURE_DATE("yyyyMMdd");

    private final String pattern;
}
