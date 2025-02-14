package com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.response;

import lombok.Data;

@Data
public class DsResponse<T> {
    private Integer code;
    private String errmsg;
    private T result;
}
