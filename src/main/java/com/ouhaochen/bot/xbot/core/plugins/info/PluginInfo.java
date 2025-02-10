package com.ouhaochen.bot.xbot.core.plugins.info;

import com.ouhaochen.bot.xbot.core.enums.PluginStatusEnum;
import lombok.Data;

@Data
public class PluginInfo {

    private String pluginName;
    private String pluginDesc;
    private String pluginAuthor;
    private String pluginVersion;
    private String pluginStatusCode;
    private PluginStatusEnum pluginStatus;

    public PluginInfo(String pluginName, String pluginDesc, String pluginAuthor, String pluginVersion) {
        this.pluginName = pluginName;
        this.pluginDesc = pluginDesc;
        this.pluginAuthor = pluginAuthor;
        this.pluginVersion = pluginVersion;
    }

    public PluginInfo(String pluginName, String pluginStatusCode) {
        this.pluginName = pluginName;
        this.pluginStatusCode = pluginStatusCode;
        this.pluginStatus = PluginStatusEnum.getPluginStatusEnum(pluginStatusCode);
    }
}
