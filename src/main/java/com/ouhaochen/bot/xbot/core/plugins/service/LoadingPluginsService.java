package com.ouhaochen.bot.xbot.core.plugins.service;

import com.mikuac.shiro.annotation.common.Shiro;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.core.constants.XBotRedisConstantKey;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.type.filter.AnnotationTypeFilter;
import org.springframework.stereotype.Service;

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class LoadingPluginsService {

    private final RedisTemplateClient redisTemplateClient;

    @PostConstruct
    public void loadingPlugins() {
        redisTemplateClient.delete(XBotRedisConstantKey.X_BOT_PLUGINS_LIST_SET_KEY);
        Set<String> pluginClassNames = loadPluginClassNames();
        //存入缓存
        pluginClassNames.forEach(pluginClassName -> {
            String[] split = pluginClassName.split("\\.");
            String pluginName = split[split.length - 1];
            redisTemplateClient.putSet(XBotRedisConstantKey.X_BOT_PLUGINS_LIST_SET_KEY, pluginName);
        });
    }

    public Set<String> loadPluginClassNames() {
        ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(false);
        scanner.addIncludeFilter(new AnnotationTypeFilter(Shiro.class));
        return scanner.findCandidateComponents("com.ouhaochen.bot.xbot.core.plugins")
                .stream()
                .map(BeanDefinition::getBeanClassName)
                .collect(Collectors.toSet());
    }
}
