package com.ouhaochen.bot.xbot;

import com.ouhaochen.bot.xbot.core.context.BotContext;
import com.ouhaochen.bot.xbot.core.plugins.weather.WeatherPluginService;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.OnmyojiPluginService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = XBotApplication.class, webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class XBotApplicationTests {

	@Autowired
	private WeatherPluginService weatherPluginService;

	@Test
	void contextLoads() {
		System.out.println("Hello World!");
	}

	@Test
	void test() {
		weatherPluginService.weather("浙江，杭州");
	}


}
