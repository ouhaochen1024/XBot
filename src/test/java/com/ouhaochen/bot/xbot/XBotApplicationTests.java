package com.ouhaochen.bot.xbot;

import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.DsApi;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.Response;
import org.dromara.hutool.json.JSONUtil;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = XBotApplication.class, webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class XBotApplicationTests {

	@Test
	void contextLoads() {
		System.out.println("Hello World!");
	}

	@Test
	void test() {
		Response response = DsApi.getOfficialFeeds();
		Response.Feed feed =response.getResult().getFeeds().get(0);
		feed.setContentPO(JSONUtil.toBean(feed.getContent(), Response.Content.class));
		System.out.println(feed.getContentPO());
	}

}
