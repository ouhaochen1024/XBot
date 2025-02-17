package com.ouhaochen.bot.xbot;


import org.junit.jupiter.api.Test;

public class StaticFunTests {

    @Test
    public void test() {
        String location = "内蒙古省包头市";
        int provinceIndex = location.indexOf("省");
        String province = location.substring(0, provinceIndex + 1);
        String place = location.substring(provinceIndex + 1);
        System.out.println(province);
        System.out.println(place);
    }
}
