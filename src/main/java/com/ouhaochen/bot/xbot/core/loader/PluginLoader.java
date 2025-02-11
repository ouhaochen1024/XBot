package com.ouhaochen.bot.xbot.core.loader;

import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.core.aspects.plugin.Plugin;
import com.ouhaochen.bot.xbot.core.constant.XBotRedisConstantKey;
import com.ouhaochen.bot.xbot.core.utils.CommonUtil;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.dromara.hutool.core.text.StrUtil;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.type.filter.AnnotationTypeFilter;
import org.springframework.stereotype.Service;

import java.util.Set;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class PluginLoader {

    @Value("${plugin-loader.enabled}")
    private boolean enabled;
    @Value("${xbot.plugins.basePackage}")
    private String basePackage;
    private final RedisTemplateClient redisTemplateClient;

    @PostConstruct
    public void loadPlugins() {
        if (!enabled) {
            return;
        }
        //todo
        redisTemplateClient.delete(XBotRedisConstantKey.X_BOT_PLUGINS_LIST_SET_KEY);
        // 存入缓存
        CommonUtil.getAllPluginNames(basePackage).forEach(pluginName -> {
            redisTemplateClient.putSet(XBotRedisConstantKey.X_BOT_PLUGINS_LIST_SET_KEY, pluginName);
        });
    }
}
