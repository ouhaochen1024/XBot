package com.ouhaochen.bot.xbot;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.core.env.Environment;

@Slf4j
@SpringBootApplication
public class XBotApplication {

	public static void main(String[] args) {
		Environment env = SpringApplication.run(XBotApplication.class, args).getEnvironment();
		String serverPort = env.getProperty("server.port");
		String contextPath = env.getProperty("server.servlet.context-path");
		String protocol = "http";
		log.info("""
                        
                        ----------------------------------------------------------
                        Application '{}' is running! Access URLs:
                        API doc: \t{}://localhost:{}{}/doc.html
                        Profile(s): \t{}
                        ----------------------------------------------------------""",
				env.getProperty("spring.application.name"),
				protocol,
				serverPort,
				contextPath,
				env.getActiveProfiles());
	}

}
