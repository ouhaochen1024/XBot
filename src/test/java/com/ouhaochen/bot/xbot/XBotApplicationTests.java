package com.ouhaochen.bot.xbot;

import com.alibaba.fastjson2.JSON;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.Response;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.api.DsApi;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.DsResponse;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.some_one_feeds.Feed;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.some_one_feeds.FeedContent;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.some_one_feeds.SomeOneFeeds;
import com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.userinfo.UserInfo;
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

	@Test
	void test2() {
		//DsResponse<SomeOneFeeds> someOneFeeds = DsApi.getSomeOneFeeds1("5b00c224c7de46d98d33a6e0722ce28f");
		DsResponse<UserInfo> userInfoDsResponse = DsApi.getUserInfo("5b00c224c7de46d98d33a6e0722ce28f");
		//System.out.println(someOneFeeds);
		System.out.println(userInfoDsResponse);
	}


}
