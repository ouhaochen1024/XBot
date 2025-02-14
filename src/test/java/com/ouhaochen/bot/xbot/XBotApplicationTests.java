package com.ouhaochen.bot.xbot;

import com.ouhaochen.bot.xbot.core.context.BotContext;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.OnmyojiPluginService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = XBotApplication.class, webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class XBotApplicationTests {

	@Autowired
	private OnmyojiPluginService onmyojiPluginService;

	@Test
	void contextLoads() {
		System.out.println("Hello World!");
	}

	@Test
	void test() {
		BotContext<Object> subscribeList =  onmyojiPluginService.subscribeList(2260967184L, 853850788L);
		System.out.println(subscribeList);
	}


}
