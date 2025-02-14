package com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.some_one_feeds;

import com.alibaba.fastjson2.JSON;
import lombok.Data;
import org.dromara.hutool.core.text.StrUtil;

@Data
public class Feed {
    private String id;
    private String uid;
    private Integer type;
    private String content;
    private FeedContent feedContent;
    private Long deleteTime;
    private Long createTime;
    private Long updateTime;

    public void setFeedContent() {
        if (StrUtil.isNotBlank(this.content)) {
            this.feedContent = JSON.parseObject(this.content, FeedContent.class);
        }
    }
}
