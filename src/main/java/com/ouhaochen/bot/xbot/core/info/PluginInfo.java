package com.ouhaochen.bot.xbot.core.info;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class PluginInfo {
    private String name;
    private String author;
    private String version;
    private String description;
    private Boolean enable;
    private Integer status;
    private Integer type;
}
