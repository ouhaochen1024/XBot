package com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.some_one_feeds;

import lombok.Data;

@Data
public class UserInfo {
    private User user;
    private UserRecord record;

    @Data
    public static class User {
        private String uid;
        private String nick;
        private String icon;
        private String intro;
        private Integer gender;
        private String identityAuthenticInfo;
        private String identityType;
        private Integer followerLevel;
        private String exclusiveTopic;
        private Long deleteTime;
        private Long createTime;
        private Long updateTime;
    }

    @Data
    public static class UserRecord {
        private String uid;
        private Integer feedCount;
        private Integer followerCount;
        private Integer followingCount;
        private Integer squareCount;
        private Integer likeCount;
        private String lastPostIpLocation;
    }
}
