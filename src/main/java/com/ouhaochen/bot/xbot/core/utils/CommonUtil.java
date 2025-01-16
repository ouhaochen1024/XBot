package com.ouhaochen.bot.xbot.core.utils;

import com.ouhaochen.bot.xbot.core.aspects.plugin.Plugin;
import lombok.extern.slf4j.Slf4j;
import org.dromara.hutool.core.text.StrUtil;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.type.filter.AnnotationTypeFilter;

import java.util.HashSet;
import java.util.Set;

@Slf4j
public final class CommonUtil {

    //获取当前插件名称
    public static String getPluginName(Class<?> clazz) {
        String pluginName;
        if (clazz.isAnnotationPresent(Plugin.class)) {
            Plugin plugin = clazz.getAnnotation(com.ouhaochen.bot.xbot.core.aspects.plugin.Plugin.class);
            if (!StrUtil.isAllBlank(plugin.name(), plugin.value())) {
                pluginName = StrUtil.isNotBlank(plugin.value()) ? plugin.value() : plugin.name();
            } else {
                pluginName = clazz.getSimpleName();
            }
        } else {
            pluginName = clazz.getSimpleName();
        }
        return pluginName;
    }

    public static Set<String> getPluginExclude(String basePackage) {
        Set<String> pluginExclude = new HashSet<>();
        // 扫描所有带有 @Plugin 注解的类 筛选exclude属性为true的类名
        ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(false);
        scanner.addIncludeFilter(new AnnotationTypeFilter(Plugin.class));
        scanner.findCandidateComponents(basePackage)
                .forEach(item -> {
                    try {
                        Class<?> clazz = Class.forName(item.getBeanClassName());
                        if (clazz.getAnnotation(Plugin.class).exclude()) {
                            pluginExclude.add(getPluginName(clazz));
                        }
                    } catch (Exception ignored) {
                    }
                });
        return pluginExclude;
    }

    public static String getAddGroupAnswer(String comment) {
        String[] spitArray = comment.split("\n");
        if (spitArray.length == 2) {
            return spitArray[1].substring(3);
        }
        return comment;
    }
}
