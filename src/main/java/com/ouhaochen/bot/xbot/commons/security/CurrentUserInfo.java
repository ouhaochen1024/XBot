package com.ouhaochen.bot.xbot.commons.security;


import lombok.Data;
import lombok.experimental.Accessors;

@Data
@Accessors(chain = true)
public class CurrentUserInfo {
    private Long botId;
    private Long userId;
    private String userNickName;
    private Long groupId;
}
