package com.ouhaochen.bot.xbot;

import com.ouhaochen.bot.xbot.extra.onmyoji.api.DsApi;
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
		System.out.println(DsApi.getOfficialFeeds());
	}

}
