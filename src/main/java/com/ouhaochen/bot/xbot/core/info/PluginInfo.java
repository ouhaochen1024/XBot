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
    private Boolean system;
    private String status;

    public PluginInfo(String name, String author, String version, String description, Boolean enable, Boolean system, String status) {
        this.name = name;
        this.author = author;
        this.version = version;
        this.description = description;
        this.enable = enable;
        this.system = system;
        this.status = status;
    }

    public PluginInfo() {
    }
}
