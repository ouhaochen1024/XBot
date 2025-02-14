package com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.api;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.TypeReference;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.Response;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.DsResponse;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.some_one_feeds.Feed;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.some_one_feeds.SomeOneFeeds;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.userinfo.UserInfo;
import org.dromara.hutool.http.HttpUtil;
import org.dromara.hutool.json.JSONUtil;

import java.util.HashMap;
import java.util.Map;

public final class DsApi {

    //动态类型
    public static final String ONMYOJI_FEED_TYPES = "1,2,3,4,6,7,10,11";
    //根据uid获取账号信息
    public static final String ONMYOJI_GET_USER_INFO = "https://inf.ds.163.com/v1/web/user/getUserInfo";
    //根据uid获取个人动态列表
    public static final String ONMYOJI_GET_SOME_ONE_FEEDS = "https://inf.ds.163.com/v1/web/feed/basic/getSomeOneFeeds";
    //阴阳师官方uid
    public static final String ONMYOJI_OFFICIAL_UID = "5b00c224c7de46d98d33a6e0722ce28f";
    //蜃彩流uid
    public static final String ONMYOJI_SHEN_CAI_LIU_UID = "c5d7f9ab6d584afb8ddf55e20bba9e63";
    public static DsResponse<SomeOneFeeds> getSomeOneFeeds(String someOneUid) {
        Map<String, Object> params = new HashMap<>();
        params.put("feedTypes", ONMYOJI_FEED_TYPES);
        params.put("someOneUid", someOneUid);
        String body = HttpUtil.createGet(ONMYOJI_GET_SOME_ONE_FEEDS)
                .form(params)
                .send()
                .bodyStr();
        return JSON.parseObject(body, new TypeReference<>() {});
    }

    public static DsResponse<UserInfo> getUserInfo(String uid) {
        Map<String, Object> params = new HashMap<>();
        params.put("uid", uid);
        String body = HttpUtil.createGet(ONMYOJI_GET_USER_INFO)
                .form(params)
                .send()
                .bodyStr();
        return JSON.parseObject(body, new TypeReference<>() {});
    }
}
