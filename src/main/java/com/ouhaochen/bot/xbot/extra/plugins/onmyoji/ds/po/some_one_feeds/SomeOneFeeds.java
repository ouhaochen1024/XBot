package com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.some_one_feeds;

import lombok.Data;

import java.util.List;

@Data
public class SomeOneFeeds {
    private List<Feed> feeds;
    private List<UserInfo> userInfos;
}
