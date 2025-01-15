package com.ouhaochen.bot.xbot.commons.utils;

import jakarta.servlet.http.HttpServletRequest;
import org.dromara.hutool.core.text.StrUtil;

import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.util.Enumeration;


public final class IPUtil {

    private static final String localhost = "0:0:0:0:0:0:0:1";
    
    private static final String localhost2 = "127.0.0.1";

    private static final String unKnown = "unKnown";
    
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


    public static String getIpAddress() {
        try {
            //从网卡中获取IP
            Enumeration<NetworkInterface> allNetInterfaces = NetworkInterface.getNetworkInterfaces();
            InetAddress ip;
            while (allNetInterfaces.hasMoreElements()) {
                NetworkInterface netInterface = allNetInterfaces.nextElement();
                //用于排除回送接口,非虚拟网卡,未在使用中的网络接口
                if (!netInterface.isLoopback() && !netInterface.isVirtual() && netInterface.isUp()) {
                    //返回和网络接口绑定的所有IP地址
                    Enumeration<InetAddress> addresses = netInterface.getInetAddresses();
                    while (addresses.hasMoreElements()) {
                        ip = addresses.nextElement();
                        if (ip instanceof Inet4Address) {
                            return ip.getHostAddress();
                        }
                    }
                }
            }
        } catch (Exception e) {
            return localhost2;
        }
        return localhost2;
    }
}
