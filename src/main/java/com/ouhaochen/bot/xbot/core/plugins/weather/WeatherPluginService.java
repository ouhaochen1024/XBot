package com.ouhaochen.bot.xbot.core.plugins.weather;

import com.ouhaochen.bot.xbot.core.context.BotContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class WeatherPluginService {
    public BotContext<Object> weather(String cityName) {
        return BotContext.ofMsg(cityName+ "今天天气晴朗");
    }
}
