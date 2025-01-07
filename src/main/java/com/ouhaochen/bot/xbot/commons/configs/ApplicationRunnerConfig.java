package com.ouhaochen.bot.xbot.commons.configs;

import com.ouhaochen.bot.xbot.commons.utils.IPUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class ApplicationRunnerConfig implements ApplicationRunner {

    private final Logger log = LoggerFactory.getLogger(this.getClass());

    @Value("${spring.application.name}")
    private String appName;

    @Value("${server.port}")
    private int serverPort;

    @Value("${shiro.ws.server.url}")
    private String wsContextPath;

    @Value("${server.servlet.context-path}")
    private String appContextPath;

    @Value("${spring.profiles.active}")
    private List<String> profiles;

    @Override
    public void run(ApplicationArguments args) {
        String ipAddress = IPUtil.getIpAddress();
        log.info("""
                
                ----------------------------------------------------------
                Application '{}' is running!
                Profile(s): {}
                WS Server:              ws://{}:{}{}
                WS Server(Local):       ws://localhost:{}{}
                WebApi Server:          http://{}:{}{}/doc.html
                WebApi Server(Local):   http://localhost:{}{}/doc.html
                ----------------------------------------------------------
                """, appName, profiles, ipAddress, serverPort, appContextPath + wsContextPath, serverPort, appContextPath + wsContextPath, ipAddress, serverPort, appContextPath, serverPort, appContextPath);
    }
}
