package com.ouhaochen.bot.xbot.extra.plugins.onmyoji_plugin;

import com.ouhaochen.bot.xbot.commons.enums.DelFlagEnum;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.core.aspects.plugin.Plugin;
import com.ouhaochen.bot.xbot.core.constant.XBotRedisConstantKey;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import com.ouhaochen.bot.xbot.core.enums.PluginStatusEnum;
import com.ouhaochen.bot.xbot.core.enums.PluginTypeEnum;
import com.ouhaochen.bot.xbot.core.utils.ActionUtil;
import com.ouhaochen.bot.xbot.db.dao.BotGroupDao;
import com.ouhaochen.bot.xbot.db.entity.BotGroupEntity;
import lombok.RequiredArgsConstructor;
import org.dromara.hutool.core.thread.ThreadUtil;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;

import java.util.List;

@RequiredArgsConstructor
@Plugin(name = "阴阳师订阅", author = "ouhaochen", description = "XBot阴阳师订阅插件", enable = false, type = PluginTypeEnum.GROUP)
public class OnmyojiPlugin {

    private final ActionUtil actionUtil;
    private final BotGroupDao botGroupDao;
    private final RedisTemplateClient redisTemplateClient;
    private final OnmyojiPluginService onmyojiPluginService;

    //task任务爬取
    @Async("myThreadPool")
    @Scheduled(cron = "0/25 * * * * ?")
    public void getOfficialFeeds() {
        ThreadUtil.sleep(5000);
        //获取官方动态
        BotContext<Response> context = onmyojiPluginService.getOfficialFeeds();
        if (context.getData() == null) {
            return;
        }
        //获取群列表
        List<BotGroupEntity> botGroupEntities = botGroupDao.lambdaQuery().eq(BotGroupEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()).list();
        botGroupEntities.forEach(botGroupEntity -> {
            String pluginStatusKey = XBotRedisConstantKey.X_BOT_GROUP_PLUGIN_STATUS_HASH_KEY(botGroupEntity.getBotId(), botGroupEntity.getGroupId());
            //pluginName从plugin注解获取
            String pluginName = OnmyojiPlugin.class.getAnnotation(Plugin.class).name();
            if (redisTemplateClient.hasHashKey(pluginStatusKey, pluginName)) {
                Integer status = (Integer) redisTemplateClient.getHashValue(pluginStatusKey, pluginName);
                if (PluginStatusEnum.ENABLED.getCode().equals(status)) {
                    actionUtil.sendGroupResponse(botGroupEntity.getBotId(), botGroupEntity.getGroupId(), context);
                }
            }
        });
    }
}
