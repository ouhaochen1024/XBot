package com.ouhaochen.bot.xbot.extra.plugins.onmyoji;

import com.mikuac.shiro.common.utils.MsgUtils;
import com.ouhaochen.bot.xbot.commons.enums.DateFormatEnum;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.api.DsApi;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.response.DsResponse;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.some_one_feeds.Feed;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.some_one_feeds.FeedContent;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.some_one_feeds.SomeOneFeeds;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.userinfo.UserInfo;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.dromara.hutool.core.date.TimeUtil;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Map;
import java.util.concurrent.TimeUnit;

@Slf4j
@Service
@RequiredArgsConstructor
public class OnmyojiPluginService {

    // 官方动态获取延迟
    private static final String ONMYOJI_OFFICIAL_FEED_DELAY_KEY = "onmyoji_official_feed_delay:";
    // 群组订阅账号
    private static final String ONMYOJI_GROUP_SUBSCRIBE_UID_HASH_KEY = "onmyoji_group_subscribe_uid:";
    // 群组订阅账号已发送最后一条动态
    private static final String ONMYOJI_GROUP_LAST_FEEDS_SENT_ID_KEY = "onmyoji_group_last_feeds_sent_id:";

    public static String ONMYOJI_OFFICIAL_FEED_DELAY_KEY(Long botId, Long groupId) {
        return ONMYOJI_OFFICIAL_FEED_DELAY_KEY + botId + ":" + groupId;
    }

    public static String ONMYOJI_GROUP_SUBSCRIBE_UID_HASH_KEY(Long botId, Long groupId) {
        return ONMYOJI_GROUP_SUBSCRIBE_UID_HASH_KEY + botId + ":" + groupId;
    }

    public static String ONMYOJI_GROUP_LAST_FEEDS_SENT_ID_KEY(Long botId, Long groupId, String uid) {
        return ONMYOJI_GROUP_LAST_FEEDS_SENT_ID_KEY + botId + ":" + groupId + ":" + uid;
    }

    private final RedisTemplateClient redisTemplateClient;

    public BotContext<Object> subscribe(Long botId, Long groupId, String uid) {
        //如果是网址则取最后/一串代码
        if (uid.contains("/")) {
            uid = uid.substring(uid.lastIndexOf("/") + 1);
        }
        if (redisTemplateClient.hasHashKey(ONMYOJI_GROUP_SUBSCRIBE_UID_HASH_KEY(botId, groupId), uid)) {
            return BotContext.ofMsg("本群已订阅该大神账号");
        }
        //查询该账号信息
        try {
            DsResponse<UserInfo> userInfoDsResponse = DsApi.getUserInfo(uid);
            UserInfo userInfo = userInfoDsResponse.getResult();
            if (userInfoDsResponse.getCode().equals(HttpStatus.OK.value())) {
                redisTemplateClient.putHash(ONMYOJI_GROUP_SUBSCRIBE_UID_HASH_KEY(botId, groupId), uid, userInfo);
                String msg = MsgUtils.builder()
                        .text(String.format("大神账号【%s】订阅成功", userInfo.getUser().getNick()))
                        .img(userInfo.getUser().getIcon())
                        .build();
                return BotContext.ofMsg(msg);
            } else {
                return BotContext.ofMsg(userInfoDsResponse.getErrmsg());
            }
        } catch (Exception e) {
            return BotContext.ofMsg("抓取该大神用户信息时失败，请稍后重试");
        }
    }

    public BotContext<Object> unsubscribe(long selfId, Long groupId, String uid) {
        //如果是网址则取最后/一串代码
        if (uid.contains("/")) {
            uid = uid.substring(uid.lastIndexOf("/") + 1);
        }
        UserInfo userInfo = (UserInfo) redisTemplateClient.getHashValue(ONMYOJI_GROUP_SUBSCRIBE_UID_HASH_KEY(selfId, groupId), uid);
        if (userInfo != null) {
            redisTemplateClient.deleteHash(ONMYOJI_GROUP_SUBSCRIBE_UID_HASH_KEY(selfId, groupId), uid);
            return BotContext.ofMsg(String.format("本群取消订阅大神账号【%s】成功", userInfo.getUser().getNick()));
        } else {
            return BotContext.ofMsg("本群未订阅该大神账号");
        }
    }

    public BotContext<Object> subscribeList(long selfId, Long groupId) {
        Map<Object, Object> subscribeMap = redisTemplateClient.getEntries(ONMYOJI_GROUP_SUBSCRIBE_UID_HASH_KEY(selfId, groupId));
        if (subscribeMap.isEmpty()) {
            return BotContext.ofMsg("本群暂未订阅任何大神账号");
        } else {
            StringBuilder msg = new StringBuilder();
            for (Map.Entry<Object, Object> entry : subscribeMap.entrySet()) {
                UserInfo userInfo = (UserInfo) entry.getValue();
                msg.append(String.format("【%s】%s\n", userInfo.getUser().getNick(), userInfo.getUser().getUid()));
            }
            return BotContext.ofMsg(msg.toString());
        }
    }

    public BotContext<Feed> getFeedsTask(Long botId, Long groupId, String uid) {
        try {
            DsResponse<SomeOneFeeds> someOneFeeds = DsApi.getSomeOneFeeds(uid);
            if (someOneFeeds.getCode().equals(HttpStatus.OK.value()) && !someOneFeeds.getResult().getFeeds().isEmpty()) {
                Feed feed = someOneFeeds.getResult().getFeeds().get(0);
                String feedId = feed.getId();
                if (redisTemplateClient.hasKey(ONMYOJI_GROUP_LAST_FEEDS_SENT_ID_KEY(botId, groupId, feedId))) {
                    return new BotContext<>(null);
                } else {
                    BotContext<Feed> botContext = new BotContext<>(feed);
                    feed.setFeedContent();
                    FeedContent feedContent = feed.getFeedContent();
                    DateTimeFormatter formatter = DateTimeFormatter.ofPattern(DateFormatEnum.NORM_DATETIME.getPattern());
                    MsgUtils msgUtil = MsgUtils.builder()
                            .text(String.format("@%s", someOneFeeds.getResult().getUserInfos().get(0).getUser().getNick()))
                            //时间戳转日期
                            .text(" " + TimeUtil.of(feed.getCreateTime(),  ZoneId.of("Asia/Shanghai")).format(formatter))
                            .text("\n")
                            .text(feedContent.getBody().getText());
                    for (FeedContent.Media media : feedContent.getBody().getMedia()) {
                        if (media.getMimeType().contains("image")) {
                            msgUtil.img(media.getUrl());
                        }
                        if (media.getMimeType().contains("video")) {
                            botContext.setVideo(media.getUrl());
                            botContext.setCover(media.getCover());
                        }
                    }
                    botContext.setMsg(msgUtil.build());
                    redisTemplateClient.set(ONMYOJI_GROUP_LAST_FEEDS_SENT_ID_KEY(botId, groupId, feedId), feed, 31, TimeUnit.DAYS);
                    return botContext;
                }
            } else {
                log.error("获取阴阳师大神用户：{} 动态失败，错误信息：{}", uid, someOneFeeds.getErrmsg());
            }
        } catch (Exception e) {
            log.error("获取阴阳师大神用户：{} 动态失败", uid, e);
        }
        return new BotContext<>(null);
    }
}
