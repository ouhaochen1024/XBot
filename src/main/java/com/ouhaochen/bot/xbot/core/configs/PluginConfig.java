package com.ouhaochen.bot.xbot.core.configs;

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

    public static String VERSION;
    public static String FORMATTED_VERSION;
    public static List<Long> SUPERVISORS;

    @Override
    public void afterPropertiesSet() {
        VERSION = version;
        FORMATTED_VERSION = formattedVersion;
        SUPERVISORS = supervisors;
    }
}
