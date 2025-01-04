package com.ouhaochen.bot.xbot;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@MapperScan("com.ouhaochen.bot.xbot.db.mapper")
@SpringBootApplication(scanBasePackages = {"com.ouhaochen.bot.xbot", "org.dromara.hutool.extra.spring"})
public class XBotApplication {
	public static void main(String[] args) {
		SpringApplication.run(XBotApplication.class, args);
	}
}
