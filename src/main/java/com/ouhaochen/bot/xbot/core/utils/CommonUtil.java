package com.ouhaochen.bot.xbot.core.utils;

import com.ouhaochen.bot.xbot.core.aspects.plugin.Plugin;
import com.ouhaochen.bot.xbot.core.config.PluginConfig;
import com.ouhaochen.bot.xbot.core.enums.PluginTypeEnum;
import com.ouhaochen.bot.xbot.core.info.PluginInfo;
import lombok.extern.slf4j.Slf4j;
import org.dromara.hutool.core.text.StrUtil;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.type.filter.AnnotationTypeFilter;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Slf4j
public final class CommonUtil {

    private static final String basePackage = PluginConfig.BASE_PACKAGE;

    //获取所有带有@Plugin注解的类
    public static List<Class<?>> getAllPluginClasses() {
        ClassPathScanningCandidateComponentProvider scanner = new ClassPathScanningCandidateComponentProvider(false);
        scanner.addIncludeFilter(new AnnotationTypeFilter(Plugin.class));
        return scanner.findCandidateComponents(basePackage)
                .stream()
                .map(BeanDefinition::getBeanClassName)
                .map(className -> {
                    try {
                        return Class.forName(className);
                    } catch (ClassNotFoundException e) {
                        return null;
                    }
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    //获取当前插件的插件信息
    public static PluginInfo getPluginInfo(Class<?> clazz) {
        if (clazz.isAnnotationPresent(Plugin.class)) {
            Plugin pluginAnnotation = clazz.getAnnotation(Plugin.class);
            return PluginInfo.builder()
                    .name(StrUtil.isNotBlank(pluginAnnotation.name()) ? pluginAnnotation.name() : clazz.getSimpleName())
                    .author(pluginAnnotation.author())
                    .version(pluginAnnotation.version())
                    .description(pluginAnnotation.description())
                    .type(pluginAnnotation.type().getCode())
                    .enable(pluginAnnotation.enable())
                    .build();
        } else {
            throw new RuntimeException("类：" + clazz.getName() + "没有@Plugin注解");
        }
    }

    public static PluginInfo getPluginInfo(String pluginName) {
        List<PluginInfo> pluginInfos = getAllPluginInfos();
        return pluginInfos.stream()
                .filter(pluginInfo -> pluginInfo.getName().equals(pluginName))
                .findFirst()
                .orElse(null);
    }

    //获取所有插件类的插件名称
    public static List<PluginInfo> getAllPluginInfos() {
        return getAllPluginClasses()
                .stream()
                .map(CommonUtil::getPluginInfo)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    //获取系统内置插件类的插件信息
    public static List<PluginInfo> getAllSystemPluginInfos() {
        return getAllPluginInfos()
                .stream()
                .filter(pluginInfo -> pluginInfo.getType().equals(PluginTypeEnum.SYSTEM.getCode()))
                .collect(Collectors.toList());
    }

    //获取自启动插件类的插件名称
    public static List<PluginInfo> getAllEnablePluginInfos() {
        return getAllPluginInfos()
                .stream()
                .filter(PluginInfo::getEnable)
                .collect(Collectors.toList());
    }

    public static String getAddGroupAnswer(String comment) {
        String[] spitArray = comment.split("\n");
        if (spitArray.length == 2) {
            return spitArray[1].substring(3);
        }
        return comment;
    }
}
