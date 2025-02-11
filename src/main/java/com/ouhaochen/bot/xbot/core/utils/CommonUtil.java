package com.ouhaochen.bot.xbot.core.utils;

import com.ouhaochen.bot.xbot.core.aspects.plugin.Plugin;
import lombok.extern.slf4j.Slf4j;
import org.dromara.hutool.core.text.StrUtil;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.type.filter.AnnotationTypeFilter;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

@Slf4j
public final class CommonUtil {

    //获取所有带有 @Plugin 注解的类名称
    public static Set<String> getAllPluginClassNames(String basePackage) {
        ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(false);
        scanner.addIncludeFilter(new AnnotationTypeFilter(Plugin.class));
        return scanner.findCandidateComponents(basePackage)
                .stream()
                .map(BeanDefinition::getBeanClassName)
                .collect(Collectors.toSet());
    }

    //获取当前插件类的插件名称
    public static String getPluginName(Class<?> clazz) {
        String pluginName;
        if (clazz.isAnnotationPresent(Plugin.class)) {
            Plugin pluginAnnotation = clazz.getAnnotation(Plugin.class);
            if (pluginAnnotation != null && !StrUtil.isAllBlank(pluginAnnotation.name(), pluginAnnotation.value())) {
                pluginName = StrUtil.isNotBlank(pluginAnnotation.value()) ? pluginAnnotation.value() : pluginAnnotation.name();
            } else {
                pluginName = clazz.getSimpleName();
            }
        } else {
            pluginName = clazz.getSimpleName();
        }
        return pluginName;
    }

    //获取所有插件类的插件名称
    public static Set<String> getAllPluginNames(String basePackage) {
        return getAllPluginClassNames(basePackage).stream()
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

    //获取系统内置插件类的插件名称
    public static Set<String> getAllSystemPluginNames(String basePackage) {
        Set<String> pluginExclude = new HashSet<>();
        getAllPluginClassNames(basePackage)
                .forEach(className -> {
                    try {
                        Class<?> clazz = Class.forName(className);
                        Plugin pluginAnnotation = clazz.getAnnotation(Plugin.class);
                        if (pluginAnnotation != null && pluginAnnotation.system()) {
                            pluginExclude.add(getPluginName(clazz));
                        }
                    } catch (ClassNotFoundException ignored) {
                    }
                });
        return pluginExclude;
    }

    //获取自启动插件类的插件名称
    public static Set<String> getAllEnablePluginNames(String basePackage) {
        Set<String> pluginExclude = new HashSet<>();
        getAllPluginClassNames(basePackage)
                .forEach(className -> {
                    try {
                        Class<?> clazz = Class.forName(className);
                        Plugin pluginAnnotation = clazz.getAnnotation(Plugin.class);
                        if (pluginAnnotation != null && pluginAnnotation.enable()) {
                            pluginExclude.add(getPluginName(clazz));
                        }
                    } catch (ClassNotFoundException ignored) {
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
