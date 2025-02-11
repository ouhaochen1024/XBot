package com.ouhaochen.bot.xbot.extra.plugins.onmyoji_plugin;

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
        private Integer type;
        private String content;
        private Content contentPO;
        private Long deleteTime;
        private Long createTime;
        private Long updateTime;
    }

    @Data
    public static class Content {
        private Integer type;
        private Body body;
    }

    @Data
    public static class Body {
        private String text;
        private List<Media> media;
    }

    @Data
    public static class Media {
        private String url;
        private String mimeType;
        private Integer size;
        private Integer width;
        private Integer height;
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
