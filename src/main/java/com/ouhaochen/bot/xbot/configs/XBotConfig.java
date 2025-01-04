package com.ouhaochen.bot.xbot.configs;

import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;

import java.util.List;

@Configuration
public class XBotConfig implements InitializingBean {

    @Value("${x-bot.supervisors}")
    private List<Long> supervisors;

    public static List<Long> SUPERVISORS;

    @Override
    public void afterPropertiesSet() {
        SUPERVISORS = supervisors;
    }
}
