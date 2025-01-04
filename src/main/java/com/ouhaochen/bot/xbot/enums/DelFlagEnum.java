package com.ouhaochen.bot.xbot.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum DelFlagEnum {
    /**
     * 删除标记
     */
    NOT_DELETED(0, "未删除"),
    DELETED(1, "已删除");

    private final Integer code;
    private final String description;
}
