package com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.userinfo;

import lombok.Data;

import java.util.List;

@Data
public class User {
    private String uid;
    private String nick;
    private String icon;
    private String intro;
    private Integer gender;
    private String birth;
    private String location;
    private String identityAuthenticInfo;
    private String identityType;
    private Integer followerLevel;
    private String state;
    private String frame;
    private String medal;
    private String homePageBgImg;
    private List<Object> squareMedals;
    private String chatBubble;
    private Long deleteTime;
    private Long createTime;
    private Long updateTime;
}
