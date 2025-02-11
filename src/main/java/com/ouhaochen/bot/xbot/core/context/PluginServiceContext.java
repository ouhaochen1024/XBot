package com.ouhaochen.bot.xbot.core.context;


import lombok.Builder;
import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
public class PluginServiceContext {
    private String msg;
    private Boolean approve = Boolean.FALSE;
    private String approveReason = "";
    private Boolean autoEscape = Boolean.FALSE;
}
