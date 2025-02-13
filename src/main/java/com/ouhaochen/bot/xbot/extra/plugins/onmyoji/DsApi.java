package com.ouhaochen.bot.xbot.extra.plugins.onmyoji;

import org.dromara.hutool.http.HttpUtil;
import org.dromara.hutool.json.JSONUtil;

import java.util.HashMap;
import java.util.Map;

public final class DsApi {

    public static final String FEED_TYPES = "1,2,3,4,6,7,10,11";
    public static final String OFFICIAL_USER_UID = "5b00c224c7de46d98d33a6e0722ce28f";

    public static Response getOfficialFeeds() {
        return getSomeOneFeeds(OFFICIAL_USER_UID);
    }

    public static Response getSomeOneFeeds(String someOneUid) {
        Map<String, Object> params = new HashMap<>();
        params.put("feedTypes", FEED_TYPES);
        params.put("someOneUid", someOneUid);
        String body = HttpUtil.createGet("https://inf.ds.163.com/v1/web/feed/basic/getSomeOneFeeds")
                .form(params)
                .send()
                .bodyStr();
        return JSONUtil.toBean(body, Response.class);
    }
}
