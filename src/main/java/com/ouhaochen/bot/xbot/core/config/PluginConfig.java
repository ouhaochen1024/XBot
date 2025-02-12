package com.ouhaochen.bot.xbot.core.config;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

import java.util.List;

@Configuration
public class PluginConfig implements InitializingBean {

    @Value("${xbot.version}")
    private String version;
    @Value("${xbot.formatted-version}")
    private String formattedVersion;
    @Value("${xbot.supervisors}")
    private List<Long> supervisors;
    @Value("${xbot.plugins.basePackage}")
    private String basePackage;

    public static String VERSION;
    public static String FORMATTED_VERSION;
    public static List<Long> SUPERVISORS;
    public static String BASE_PACKAGE;

    @Override
    public void afterPropertiesSet() {
        VERSION = version;
        FORMATTED_VERSION = formattedVersion;
        SUPERVISORS = supervisors;
        BASE_PACKAGE = basePackage;
    }
}
