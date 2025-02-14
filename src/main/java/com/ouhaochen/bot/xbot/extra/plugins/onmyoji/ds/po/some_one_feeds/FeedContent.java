package com.ouhaochen.bot.xbot.extra.plugins.onmyoji.ds.po.some_one_feeds;

import lombok.Data;

import java.util.List;

@Data
public class FeedContent {
    private Integer type;
    private Body body;
    @Data
    public static class Body {
        private String text;
        private List<Media> media;
    }
    @Data
    public static class Media {
        private String url;
        private String mimeType;
        private Integer size;
        private Integer width;
        private Integer height;
    }
}
