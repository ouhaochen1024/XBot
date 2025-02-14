package com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po;

import lombok.Data;

@Data
public class DsResponse<T> {
    private Integer code;
    private String errmsg;
    private T result;
}
