package com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.userinfo;

import lombok.Data;

@Data
public class Record {
    private String uid;
    private Integer feedCount;
    private Long followerCount;
    private Integer followingCount;
    private Integer roleCount;
    private Integer columnCount;
    private Integer squareCount;
    private Long likeCount;
    private String lastPostIpLocation;
}