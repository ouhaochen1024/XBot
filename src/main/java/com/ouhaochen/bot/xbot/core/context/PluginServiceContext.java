package com.ouhaochen.bot.xbot.core.context;


import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
public class PluginServiceContext {
    private String msg;
    private Boolean autoEscape = Boolean.FALSE;
}
