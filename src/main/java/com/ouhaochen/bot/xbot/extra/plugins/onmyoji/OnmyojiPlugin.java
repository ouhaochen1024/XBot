package com.ouhaochen.bot.xbot.extra.plugins.onmyoji;

import com.mikuac.shiro.annotation.GroupMessageHandler;
import com.mikuac.shiro.annotation.MessageHandlerFilter;
import com.mikuac.shiro.core.Bot;
import com.mikuac.shiro.dto.event.message.GroupMessageEvent;
import com.ouhaochen.bot.xbot.commons.enums.TrueOrFalseEnum;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.core.aspects.permission.Permission;
import com.ouhaochen.bot.xbot.core.aspects.plugin.Plugin;
import com.ouhaochen.bot.xbot.core.constant.XBotRedisConstantKey;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import com.ouhaochen.bot.xbot.core.enums.PluginStatusEnum;
import com.ouhaochen.bot.xbot.core.enums.PluginTypeEnum;
import com.ouhaochen.bot.xbot.core.utils.ActionUtil;
import com.ouhaochen.bot.xbot.core.utils.MatcherUtil;
import com.ouhaochen.bot.xbot.db.dao.BotGroupDao;
import com.ouhaochen.bot.xbot.db.entity.BotGroupEntity;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.some_one_feeds.Feed;
import lombok.RequiredArgsConstructor;
import org.dromara.hutool.core.thread.ThreadUtil;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;

import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;

@RequiredArgsConstructor
@Plugin(name = "阴阳师订阅", author = "ouhaochen", description = "XBot阴阳师订阅插件", enable = false, type = PluginTypeEnum.GROUP)
public class OnmyojiPlugin {

    private final ActionUtil actionUtil;
    private final BotGroupDao botGroupDao;
    private final RedisTemplateClient redisTemplateClient;
    private final OnmyojiPluginService onmyojiPluginService;

    @Permission
    @GroupMessageHandler
    @MessageHandlerFilter(cmd = "^阴阳师订阅\\s(.*)?$")
    public void subscribe(Bot bot, GroupMessageEvent event, Matcher matcher) {
        String keyword = MatcherUtil.getNormal(bot, event, matcher);
        if(keyword == null) return;
        BotContext<Object> context = onmyojiPluginService.subscribe(bot.getSelfId(), event.getGroupId(), keyword);
        ActionUtil.sendResponse(bot, event, context);
    }

    //task任务爬取
    @Async("myThreadPool")
    @Scheduled(cron = "0/25 * * * * ?")
    public void getFeedsTask() {
        ThreadUtil.sleep(2500);
        List<BotGroupEntity> botGroupEntities = botGroupDao.getAllBotGroupList();
        botGroupEntities.forEach(botGroupEntity -> {
            Long botId = botGroupEntity.getBotId();
            Long groupId = botGroupEntity.getGroupId();
            String feedsDelayKey = OnmyojiPluginService.ONMYOJI_OFFICIAL_FEED_DELAY_KEY(botId, groupId);
            if (redisTemplateClient.hasKey(feedsDelayKey)) {
                return;
            }
            // 检查群组插件状态
            String pluginStatusKey = XBotRedisConstantKey.X_BOT_GROUP_PLUGIN_STATUS_HASH_KEY(botId, groupId);
            if (redisTemplateClient.hasHashKey(pluginStatusKey, OnmyojiPlugin.class.getAnnotation(Plugin.class).name())) {
                Integer status = (Integer) redisTemplateClient.getHashValue(pluginStatusKey, OnmyojiPlugin.class.getAnnotation(Plugin.class).name());
                // 群组插件状态为启用时，继续执行播报任务
                if (PluginStatusEnum.ENABLED.getCode().equals(status)) {
                    // 查看群组订阅缓存
                    String uidHashKey = OnmyojiPluginService.ONMYOJI_GROUP_SUBSCRIBE_UID_HASH_KEY(botId, groupId);
                    if (redisTemplateClient.hasKey(uidHashKey)) {
                        Map<String, String> uidMap = redisTemplateClient.hGetAll(uidHashKey);
                        for (Map.Entry<String, String> entry : uidMap.entrySet()) {
                            BotContext<Feed> context = onmyojiPluginService.getFeedsTask(botId, groupId, entry.getKey());
                            actionUtil.sendGroupResponse(botId, groupId, context);
                            ThreadUtil.sleep(10, TimeUnit.SECONDS);
                        }
                        redisTemplateClient.set(OnmyojiPluginService.ONMYOJI_OFFICIAL_FEED_DELAY_KEY(botId, groupId), TrueOrFalseEnum.TRUE.getCode(), 5, TimeUnit.MINUTES);
                    }
                }
            }
        });
    }


}
