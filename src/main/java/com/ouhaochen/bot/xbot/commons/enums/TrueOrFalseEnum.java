package com.ouhaochen.bot.xbot.commons.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum TrueOrFalseEnum {
    /**
     *  真假枚举
     */
    FALSE(0, "false"),
    TRUE(1, "true");

    private final Integer code;
    private final String description;
}
