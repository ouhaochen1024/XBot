package com.ouhaochen.bot.xbot.extra.plugins.onmyoji_plugin;

import com.mikuac.shiro.annotation.common.Shiro;
import com.ouhaochen.bot.xbot.commons.enums.DelFlagEnum;
import com.ouhaochen.bot.xbot.core.aspects.plugin.Plugin;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import com.ouhaochen.bot.xbot.core.plugins.system_plugin.SystemPluginService;
import com.ouhaochen.bot.xbot.core.utils.ActionUtil;
import com.ouhaochen.bot.xbot.db.dao.BotGroupDao;
import com.ouhaochen.bot.xbot.db.entity.BotGroupEntity;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.List;

@Shiro
@Component
@RequiredArgsConstructor
@Plugin(name = "阴阳师订阅插件", author = "ouhaochen", description = "XBot阴阳师订阅插件", enable = false)
public class OnmyojiPlugin {

    private final ActionUtil actionUtil;
    private final BotGroupDao botGroupDao;
    private final SystemPluginService systemPluginService;
    private final OnmyojiPluginService onmyojiPluginService;


    //task任务爬取
    @Async("ThreadPoolConfig")
    @Scheduled(cron = "0/5 * * * * ?")
    public void getOfficialFeeds() {
//        List<BotGroupEntity>
//        botGroupDao.lambdaQuery().eq(BotGroupEntity::getDelFlag, DelFlagEnum.NOT_DELETED.getCode()).list().forEach(botGroupEntity -> {
//
//        });
//        systemPluginService.viewPlugins();
//        BotContext context = onmyojiPluginService.getOfficialFeeds();
        //actionUtil.sendGroupResponse(context.getBotId(), context.getGroupId(), context);
    }




}
