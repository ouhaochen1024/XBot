package com.ouhaochen.bot.xbot.core.plugins.weather;

import com.alibaba.fastjson2.JSON;
import com.ouhaochen.bot.xbot.core.context.BotContext;
import com.ouhaochen.bot.xbot.core.plugins.weather.po.WeatherInfo;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.dromara.hutool.http.HttpUtil;
import org.dromara.hutool.json.JSONUtil;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class WeatherPluginService {

    @Value("${third-party-api.apihz.get—url}")
    private String getUrl;
    @Value("${third-party-api.apihz.id}")
    private String id;
    @Value("${third-party-api.apihz.key}")
    private String key;

    public BotContext<Object> weather(String location) {
        try {
            // 省
            String province;
            // 地点
            String place;
            // 解析地址
            if (location.contains(",")) {
                province = location.split(",")[0];
                place = location.split(",")[1];
            } else if (location.contains("，")) {
                province = location.split("，")[0];
                place = location.split("，")[1];
            } else if (location.contains("省")) {
                int provinceIndex = location.indexOf("省");
                province = location.substring(0, provinceIndex + 1);
                place = location.substring(provinceIndex + 1);
            } else if (location.contains("自治区")) {
                int provinceIndex = location.indexOf("自治区");
                province = location.substring(0, provinceIndex + 3);
                place = location.substring(provinceIndex + 3);
            } else if (location.contains("市")) {
                int provinceIndex = location.indexOf("市");
                province = location.substring(0, provinceIndex + 1);
                place = location.substring(provinceIndex + 1);
            } else {
                return BotContext.ofMsg("地区格式错误，示例格式：【浙江省杭州市】、【浙江,杭州】、" +
                        "【浙江，杭州】、【北京市通州区】");
            }
            String getUrlBodyStr = HttpUtil.createGet(getUrl).send().bodyStr();
            String api = JSONUtil.parseObj(getUrlBodyStr).getStr("api") + "api/tianqi/tqyb.php";
            String weatherBodyStr = HttpUtil.createGet(api + "?id=" + id + "&key=" + key + "&sheng=" + province + "&place=" + place).send().bodyStr();
            WeatherInfo weatherInfo = JSON.parseObject(weatherBodyStr, WeatherInfo.class);
            String[] placeArray = weatherInfo.getPlace().split(",");
            String todayWeather;
            if (weatherInfo.getWeather1().equals(weatherInfo.getWeather2())) {
                todayWeather = weatherInfo.getWeather1();
            } else {
                todayWeather = weatherInfo.getWeather1() + "转" + weatherInfo.getWeather2();
            }
            String msg = placeArray[1] + String.format("%s，天气：%s，气温：%s(℃)，相对湿度：%s(%%)，气压：%s(hPa)，降水：%s(mm)，刮%s，风速：%s(m/s)（%s）。",
                    placeArray[2], todayWeather, weatherInfo.getTemperature(), weatherInfo.getHumidity(), weatherInfo.getPressure(), weatherInfo.getPrecipitation(),
                    weatherInfo.getWindDirection(), weatherInfo.getWindSpeed(), weatherInfo.getWindScale()
            );
            return BotContext.ofMsg(msg);
        } catch (Exception e) {
            log.error("地点：{} 天气查询失败", location, e);
            return BotContext.ofMsg("查询失败，请稍后重试");
        }
    }

}
