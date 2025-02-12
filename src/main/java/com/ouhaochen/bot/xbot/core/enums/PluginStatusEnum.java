package com.ouhaochen.bot.xbot.core.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum PluginStatusEnum {
    //0 禁用 1 启用
    DISABLED(0, "0", "禁用", "\uD83D\uDD34"),
    ENABLED(1, "1", "启用", "\uD83D\uDFE2");

    private final Integer code;
    private final String codeStr;
    private final String status;
    private final String icon;

    public static PluginStatusEnum getPluginStatusEnum(Integer code) {
        for (PluginStatusEnum pluginStatusEnum : PluginStatusEnum.values()) {
            if (pluginStatusEnum.getCode().equals(code)) {
                return pluginStatusEnum;
            }
        }
        return null;
    }

    public static PluginStatusEnum getPluginStatusEnum(String codeStr) {
        for (PluginStatusEnum pluginStatusEnum : PluginStatusEnum.values()) {
            if (pluginStatusEnum.getCodeStr().equals(codeStr)) {
                return pluginStatusEnum;
            }
        }
        return null;
    }

    public static String getStatus(Integer code) {
        for (PluginStatusEnum pluginStatusEnum : PluginStatusEnum.values()) {
            if (pluginStatusEnum.getCode().equals(code)) {
                return pluginStatusEnum.getStatus();
            }
        }
        return null;
    }

    public static String getStatus(String codeStr) {
        for (PluginStatusEnum pluginStatusEnum : PluginStatusEnum.values()) {
            if (pluginStatusEnum.getCodeStr().equals(codeStr)) {
                return pluginStatusEnum.getStatus();
            }
        }
        return null;
    }

    public static String getIcon(Integer code) {
        for (PluginStatusEnum pluginStatusEnum : PluginStatusEnum.values()) {
            if (pluginStatusEnum.getCode().equals(code)) {
                return pluginStatusEnum.getIcon();
            }
        }
        return null;
    }

    public static String getIcon(String codeStr) {
        for (PluginStatusEnum pluginStatusEnum : PluginStatusEnum.values()) {
            if (pluginStatusEnum.getCodeStr().equals(codeStr)) {
                return pluginStatusEnum.getIcon();
            }
        }
        return null;
    }
}
