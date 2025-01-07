package com.ouhaochen.bot.xbot.commons.configs;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import java.net.InetAddress;
import java.util.List;

@Component
public class ApplicationRunnerConfig implements ApplicationRunner {

    private final Logger log = LoggerFactory.getLogger(this.getClass());

    @Value("${spring.application.name}")
    private String appName;

    @Value("${server.port}")
    private int serverPort;

    @Value("${shiro.ws.server.url}")
    private String contextPath;

    @Value("${spring.profiles.active}")
    private List<String> profiles;

    @Override
    public void run(ApplicationArguments args) {
        String ipAddress = "127.0.0.1";
        try {
            ipAddress = InetAddress.getLocalHost().getHostAddress();
        } catch (Exception e) {
            log.error("获取本机IP地址失败", e);
        }
        log.info("""
                
                ----------------------------------------------------------
                Application '{}' is running!
                Profile(s): {}
                WS Server:         ws://{}:{}{}
                WS Server(Local):  ws://localhost:{}{}
                ----------------------------------------------------------
                """, appName, profiles, ipAddress, serverPort, contextPath, serverPort, contextPath);
    }
}
