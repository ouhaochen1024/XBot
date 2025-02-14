package com.ouhaochen.bot.xbot;

import com.ouhaochen.bot.xbot.core.context.BotContext;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.OnmyojiPluginService;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.some_one_feeds.Feed;
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
		BotContext<Feed> someOneFeeds =  onmyojiPluginService.getFeedsTask(2260967184L, 853850788L, "21e043e2f41145ba9678f85f72a41938");
		System.out.println(someOneFeeds);
	}


}
