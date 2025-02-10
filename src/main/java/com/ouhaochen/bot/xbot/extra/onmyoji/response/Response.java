package com.ouhaochen.bot.xbot.extra.onmyoji.response;

import lombok.Data;
import java.util.List;

@Data
public class Response {

    private Result result;
    private Integer code;
    private String errmsg;

    @Data
    public static class Result {
        private List<Feed> feeds;
        private List<UserInfo> userInfos;
//        private Map<String, SquareBaseInfo> squareBaseInfoMap;
    }

    @Data
    public static class Feed {
        private String id;
        private String uid;
        private int type;
        private String content;
    }

    @Data
    public static class UserInfo {
        private User user;
        private UserRecord record;
    }

    @Data
    public static class User {
        private String uid;
        private String nick;
        private String icon;
        private String intro;
        private int gender;
        private String identityAuthenticInfo;
        private String identityType;
        private int followerLevel;
        private String exclusiveTopic;
        private long deleteTime;
        private long createTime;
        private long updateTime;
    }

    @Data
    public static class UserRecord {
        private String uid;
        private int feedCount;
        private int followerCount;
        private int followingCount;
        private int squareCount;
        private int likeCount;
        private String lastPostIpLocation;
    }

//    @Data
//    public static class SquareBaseInfo {
//        private String squareId;
//        private String icon;
//        private String name;
//        private String tag;
//        private HomeBackground homeBackground;
//    }

//    @Data
//    public static class HomeBackground {
//        private String background;
//        private String bgColor;
//        private String color;
//    }
}
