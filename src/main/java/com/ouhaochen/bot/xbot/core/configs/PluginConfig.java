package com.ouhaochen.bot.xbot.core.configs;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

import java.util.List;

@Configuration
public class PluginConfig implements InitializingBean {

    @Value("${x-bot.version}")
    private String version;
    @Value("${x-bot.supervisors}")
    private List<Long> supervisors;

    public static String VERSION;
    public static List<Long> SUPERVISORS;

    @Override
    public void afterPropertiesSet() {
        VERSION = version;
        SUPERVISORS = supervisors;
    }
}
