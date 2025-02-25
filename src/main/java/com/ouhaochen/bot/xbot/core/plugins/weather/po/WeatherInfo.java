package com.ouhaochen.bot.xbot.core.plugins.weather.po;

import lombok.Data;

@Data
public class WeatherInfo {
    private Integer code; // 状态码
    private String msg; // 消息内容
    private Double precipitation; // 降水量
    private Double temperature; // 温度
    private Double pressure; // 气压
    private Double humidity; // 湿度
    private String windDirection; // 风向
    private Integer windDirectionDegree; // 风向度
    private Double windSpeed; // 风速
    private String windScale; // 风速描述
    private String place; // 地区
    private String weather1; // 当日天气1
    private String weather2; // 当日天气2
}
