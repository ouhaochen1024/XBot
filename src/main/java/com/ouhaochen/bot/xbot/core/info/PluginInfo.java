package com.ouhaochen.bot.xbot.core.info;

import com.ouhaochen.bot.xbot.core.enums.PluginStatusEnum;
import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class PluginInfo {
    private String pluginName;
    private String pluginDesc;
    private String pluginAuthor;
    private String pluginVersion;
    private String pluginStatusCode;
    private PluginStatusEnum pluginStatus;

    public PluginInfo(String pluginName, String pluginStatusCode) {
        this.pluginName = pluginName;
        this.pluginStatusCode = pluginStatusCode;
        this.pluginStatus = PluginStatusEnum.getPluginStatusEnum(pluginStatusCode);
    }
}
