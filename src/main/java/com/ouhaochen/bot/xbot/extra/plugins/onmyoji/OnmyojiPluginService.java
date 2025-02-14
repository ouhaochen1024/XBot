package com.ouhaochen.bot.xbot.extra.plugins.onmyoji;

import com.mikuac.shiro.common.utils.MsgUtils;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.api.DsApi;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.DsResponse;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.some_one_feeds.Feed;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.some_one_feeds.FeedContent;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.some_one_feeds.SomeOneFeeds;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.userinfo.UserInfo;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import java.util.concurrent.TimeUnit;

@Slf4j
@Service
@RequiredArgsConstructor
public class OnmyojiPluginService {

    private static final String ONMYOJI_OFFICIAL_FEED_DELAY_KEY = "onmyoji_official_feed_delay:";
    // 群组订阅账号
    private static final String ONMYOJI_GROUP_SUBSCRIBE_UID_HASH_KEY = "onmyoji_group_subscribe_uid:";
    // 群组订阅账号已发送的动态
    private static final String ONMYOJI_GROUP_FEEDS_SENT_ID_HASH_KEY = "onmyoji_group_feeds_sent_id:";

    public static String ONMYOJI_OFFICIAL_FEED_DELAY_KEY(Long botId, Long groupId) {
        return ONMYOJI_OFFICIAL_FEED_DELAY_KEY + botId + ":" + groupId;
    }

    public static String ONMYOJI_GROUP_SUBSCRIBE_UID_HASH_KEY(Long botId, Long groupId) {
        return ONMYOJI_GROUP_SUBSCRIBE_UID_HASH_KEY + botId + ":" + groupId;
    }

    public static String ONMYOJI_GROUP_FEEDS_SENT_ID_HASH_KEY(Long botId, Long groupId) {
        return ONMYOJI_GROUP_FEEDS_SENT_ID_HASH_KEY + botId + ":" + groupId;
    }

    private final RedisTemplateClient redisTemplateClient;

    public BotContext<Feed> getFeedsTask(Long botId, Long groupId, String uid) {
        try {
            DsResponse<SomeOneFeeds> someOneFeeds = DsApi.getSomeOneFeeds(uid);
            if (someOneFeeds.getCode().equals(HttpStatus.OK.value()) && !someOneFeeds.getResult().getFeeds().isEmpty()) {
                Feed feed = someOneFeeds.getResult().getFeeds().get(0);
                if (redisTemplateClient.hasHashKey(ONMYOJI_GROUP_FEEDS_SENT_ID_HASH_KEY(botId, groupId), feed.getId())) {
                    return new BotContext<>(null);
                } else {
                    BotContext<Feed> botContext = new BotContext<>(feed);
                    feed.setFeedContent();
                    FeedContent feedContent = feed.getFeedContent();
                    MsgUtils msgUtil = MsgUtils.builder()
                            .text(String.format("@%s", someOneFeeds.getResult().getUserInfos().get(0).getUser().getNick()))
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
                    redisTemplateClient.putHash(ONMYOJI_GROUP_FEEDS_SENT_ID_HASH_KEY(botId, groupId), feed.getId(), feed);
                    redisTemplateClient.expire(ONMYOJI_GROUP_FEEDS_SENT_ID_HASH_KEY(botId, groupId), 31, TimeUnit.DAYS);
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

    public BotContext<Object> subscribe(Long botId, Long groupId, String uid) {
        if (redisTemplateClient.hasHashKey(ONMYOJI_GROUP_SUBSCRIBE_UID_HASH_KEY(botId, groupId), uid)) {
            return BotContext.ofMsg("该大神账号已被本群订阅");
        }
        //查询该账号信息
        try {
            DsResponse<UserInfo> userInfo = DsApi.getUserInfo(uid);
            if (userInfo.getCode().equals(HttpStatus.OK.value())) {
                redisTemplateClient.putHash(ONMYOJI_GROUP_SUBSCRIBE_UID_HASH_KEY(botId, groupId), uid, userInfo);
                String msg = MsgUtils.builder()
                        .text(String.format("大神账号：【%s】订阅成功", userInfo.getResult().getUser().getNick()))
                        .img(userInfo.getResult().getUser().getIcon())
                        .build();
                return BotContext.ofMsg(msg);
            } else {
                return BotContext.ofMsg(userInfo.getErrmsg());
            }
        } catch (Exception e) {
            return BotContext.ofMsg("抓取该大神用户信息失败，请稍后重试");
        }
    }
}
