package com.ouhaochen.bot.xbot;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest(classes = XBotApplication.class, webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
class XBotApplicationTests {

	@Test
	void contextLoads() {
		System.out.println("Hello World!");
	}

}
