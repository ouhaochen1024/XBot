package com.ouhaochen.bot.xbot.commons.utils;

import jakarta.servlet.http.HttpServletRequest;
import org.dromara.hutool.core.text.StrUtil;



public class HttpUtil {

    private final static String localhost = "0:0:0:0:0:0:0:1";
    
    private final static String localhost2 = "127.0.0.1";

    private final static String unKnown = "unKnown";
    
    /**
     * 获取用户真实IP地址，不使用request.getRemoteAddr();的原因是有可能用户使用了代理软件方式避免真实IP地址,
     * <p>
     * 可是，如果通过了多级反向代理的话，X-Forwarded-For的值并不止一个，而是一串IP值，究竟哪个才是真正的用户端的真实IP呢？
     * 答案是取X-Forwarded-For中第一个非unknown的有效IP字符串。
     * <p>
     * 如：X-Forwarded-For：192.168.1.110, 192.168.1.120, 192.168.1.130,
     * 192.168.1.100
     * <p>
     * 用户真实IP为： 192.168.1.110
     *
     * @return ip
     */
    public static String getRealIpAddress(HttpServletRequest request) {
        String Xip = request.getHeader("X-Real-IP");
        String XFor = request.getHeader("X-Forwarded-For");
        //多次反向代理后会有多个ip值，第一个ip才是真实ip
        if (StrUtil.isNotBlank(XFor) && !unKnown.equalsIgnoreCase(XFor)) {
            int index = XFor.indexOf(",");
            if (index != -1) {
                return localhost.equals(XFor.substring(0, index)) ? localhost2 : XFor.substring(0, index);
            } else {
                return localhost.equals(XFor) ? localhost2 : XFor;
            }
        }
        XFor = Xip;
        if (StrUtil.isNotBlank(XFor) && !unKnown.equalsIgnoreCase(XFor))
            return localhost.equals(XFor) ? localhost2 : XFor;
        if (StrUtil.isBlank(XFor) || unKnown.equalsIgnoreCase(XFor))
            XFor = request.getHeader("Proxy-Client-IP");
        if (StrUtil.isBlank(XFor) || unKnown.equalsIgnoreCase(XFor))
            XFor = request.getHeader("WL-Proxy-Client-IP");
        if (StrUtil.isBlank(XFor) || unKnown.equalsIgnoreCase(XFor))
            XFor = request.getHeader("HTTP_CLIENT_IP");
        if (StrUtil.isBlank(XFor) || unKnown.equalsIgnoreCase(XFor))
            XFor = request.getHeader("HTTP_X_FORWARDED_FOR");
        if (StrUtil.isBlank(XFor) || unKnown.equalsIgnoreCase(XFor))
            XFor = request.getRemoteAddr();
        return localhost.equals(XFor) ? localhost2 : XFor;
    }
}
