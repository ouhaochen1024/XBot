package com.ouhaochen.bot.xbot;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;

@EnableAsync
@EnableScheduling
@SpringBootApplication(scanBasePackages = {"com.ouhaochen.bot.xbot", "org.dromara.hutool.extra.spring"})
public class XBotApplication {
	public static void main(String[] args) {
		SpringApplication.run(XBotApplication.class, args);
	}
}
