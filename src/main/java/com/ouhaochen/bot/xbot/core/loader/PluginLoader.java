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

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class PluginLoader {

    @Value("${redis.enabled}")
    private boolean isRedisEnabled;
    @Value("${xbot.plugins.basePackage}")
    private String basePackage;
    private final RedisTemplateClient redisTemplateClient;

    @PostConstruct
    public void loadPlugins() {
        if (!isRedisEnabled) {
            return;
        }
        redisTemplateClient.delete(XBotRedisConstantKey.X_BOT_PLUGINS_LIST_SET_KEY);
        // 存入缓存
        getAllPluginClassNames().forEach(pluginName -> {
            redisTemplateClient.putSet(XBotRedisConstantKey.X_BOT_PLUGINS_LIST_SET_KEY, pluginName);
        });
    }

    public Set<String> getAllPluginClassNames() {
        // 扫描所有带有 @Plugin 注解的类
        ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(false);
        scanner.addIncludeFilter(new AnnotationTypeFilter(Plugin.class));
        Set<String> pluginClassNames = scanner.findCandidateComponents(basePackage)
                .stream()
                .map(BeanDefinition::getBeanClassName)
                .collect(Collectors.toSet());
        // 使用 CommonUtil.getPluginName 方法获取插件名称
        return pluginClassNames.stream()
                .map(className -> {
                    try {
                        Class<?> clazz = Class.forName(className);
                        return CommonUtil.getPluginName(clazz);
                    } catch (ClassNotFoundException e) {
                        return null;
                    }
                })
                .filter(StrUtil::isNotBlank)
                .collect(Collectors.toSet());
    }
}
