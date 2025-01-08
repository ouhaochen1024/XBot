package com.ouhaochen.bot.xbot.core.loader;

import com.mikuac.shiro.annotation.common.Shiro;
import com.ouhaochen.bot.xbot.commons.redis.clients.RedisTemplateClient;
import com.ouhaochen.bot.xbot.core.constant.XBotRedisConstantKey;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
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

    @Value("${redis.enabled}")
    private boolean isRedisEnabled;
    private final RedisTemplateClient redisTemplateClient;

    @PostConstruct
    public void loadPlugins() {
        if (!isRedisEnabled) {
            return;
        }
        redisTemplateClient.delete(XBotRedisConstantKey.X_BOT_PLUGINS_LIST_SET_KEY);
        Set<String> pluginClassNames = getAllPluginClassNames();
        //存入缓存
        pluginClassNames.forEach(pluginClassName -> {
            String[] split = pluginClassName.split("\\.");
            String pluginName = split[split.length - 1];
            redisTemplateClient.putSet(XBotRedisConstantKey.X_BOT_PLUGINS_LIST_SET_KEY, pluginName);
        });
    }

    public Set<String> getAllPluginClassNames() {
        ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(false);
        scanner.addIncludeFilter(new AnnotationTypeFilter(Shiro.class));
        return scanner.findCandidateComponents("com.ouhaochen.bot.xbot.core.plugins")
                .stream()
                .map(BeanDefinition::getBeanClassName)
                .collect(Collectors.toSet());
    }
}
