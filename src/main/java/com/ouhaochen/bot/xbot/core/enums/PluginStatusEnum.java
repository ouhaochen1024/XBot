package com.ouhaochen.bot.xbot.core.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum PluginStatusEnum {
    //0 禁用 1 启用
    DISABLED(0, "禁用"),
    ENABLED(1, "启用");

    private final Integer code;
    private final String message;

    public static PluginStatusEnum getPluginStatusEnum(Integer code) {
        for (PluginStatusEnum pluginStatusEnum : PluginStatusEnum.values()) {
            if (pluginStatusEnum.getCode().equals(code)) {
                return pluginStatusEnum;
            }
        }
        return null;
    }

    public static String getMessage(Integer code) {
        for (PluginStatusEnum pluginStatusEnum : PluginStatusEnum.values()) {
            if (pluginStatusEnum.getCode().equals(code)) {
                return pluginStatusEnum.getMessage();
            }
        }
        return null;
    }
}
