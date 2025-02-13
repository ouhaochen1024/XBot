package com.ouhaochen.bot.xbot.extra.plugins.onmyoji_plugin;

import com.mikuac.shiro.common.utils.MsgUtils;
import com.ouhaochen.bot.xbot.commons.enums.TrueOrFalseEnum;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.concurrent.TimeUnit;

@Slf4j
@Service
@RequiredArgsConstructor
public class OnmyojiPluginService {

    public static final String ONMYOJI_OFFICIAL_FEED_KEY = "onmyoji_official_feed:";
    public static final String ONMYOJI_OFFICIAL_FEED_DELAY_KEY = "onmyoji_official_feed_delay";
    private final RedisTemplateClient redisTemplateClient;

    public BotContext<Response> getOfficialFeeds() {
        if (redisTemplateClient.hasKey(ONMYOJI_OFFICIAL_FEED_DELAY_KEY)) {
            return new BotContext<>(null);
        }
        try {
            Response response = DsApi.getOfficialFeeds();
            if (response != null && response.getCode().equals(HttpStatus.OK.value())) {
                Response.Feed leastFeed = response.getResult().getFeeds().get(0);
                leastFeed.setContentPO();
                if (!redisTemplateClient.hasKey(ONMYOJI_OFFICIAL_FEED_KEY + leastFeed.getId())) {
                    List<Response.Media> mediaList = leastFeed.getContentPO().getBody().getMedia();
                    MsgUtils msgUtil = MsgUtils.builder()
                            .text(leastFeed.getContentPO().getBody().getText());
                    for (Response.Media media : mediaList) {
                        if (media.getMimeType().contains("image")) {
                            msgUtil.img(media.getUrl());
                        }
                    }
                    String msg = msgUtil.build();
                    redisTemplateClient.set(ONMYOJI_OFFICIAL_FEED_KEY + leastFeed.getId(), TrueOrFalseEnum.TRUE.getCode(), 31, TimeUnit.DAYS);
                    redisTemplateClient.set(ONMYOJI_OFFICIAL_FEED_DELAY_KEY, TrueOrFalseEnum.TRUE.getCode(), 5, TimeUnit.MINUTES);
                    return BotContext.ofData(msg, response);
                } else {
                    redisTemplateClient.set(ONMYOJI_OFFICIAL_FEED_DELAY_KEY, TrueOrFalseEnum.TRUE.getCode(), 5, TimeUnit.MINUTES);
                }
            }
        } catch (Exception e) {
            log.error("获取阴阳师官方动态失败，将延迟2小时执行", e);
            redisTemplateClient.set(ONMYOJI_OFFICIAL_FEED_DELAY_KEY, TrueOrFalseEnum.TRUE.getCode(), 2, TimeUnit.HOURS);
        }
        return new BotContext<>(null);
    }


}
