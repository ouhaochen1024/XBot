package com.ouhaochen.bot.xbot.core.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum PluginTypeEnum {
    //系统 群聊 私聊 通用
    SYSTEM(0, "系统"),
    GROUP(1, "群聊"),
    PRIVATE(2, "私聊"),
    COMMON(3, "通用");

    private final Integer code;
    private final String type;

    public static PluginTypeEnum getPluginTypeEnum(Integer code) {
        for (PluginTypeEnum pluginTypeEnum : PluginTypeEnum.values()) {
            if (pluginTypeEnum.getCode().equals(code)) {
                return pluginTypeEnum;
            }
        }
        return null;
    }
}
