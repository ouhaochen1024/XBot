package com.ouhaochen.bot.xbot.config.aspect.log;


import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;


/**
 * 登录自定义表单其他参数
 */
@Aspect
@Component
@RequiredArgsConstructor
public class LogAspect {

    private final Environment env;
//
//    @Autowired
//    private ExceptionLogService exceptionLogService;
//    @Autowired
//    private OperateationLogService OperateationLogService;

    /**
     * 设置操作日志切入点 记录操作日志 在注解的位置切入代码
     */
//    @Pointcut("@annotation(com.oscar.app.config.log.Log)")
//    public void logPointCut() {
//    }
//
//    /**
//    * 设置操作异常切入点记录异常日志 扫描所有controller包下操作
//    */
//    @Pointcut("execution(* com.oscar.app.controller..*.*(..))")
//    public void exceptionLogPointCut() {
//    }

    /**
     * 正常返回通知，拦截用户操作日志，连接点正常执行完成后执行， 如果连接点抛出异常，则不会执行
     *
     * @param joinPoint 切入点
     * @param keys      返回结果
     */
//    @AfterReturning(value = "logPointCut()", returning = "keys")
//    public void saveOperateLog(JoinPoint joinPoint, Object keys) throws Exception {
//        // 获取RequestAttributes
//        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
//        HttpServletRequest request = (HttpServletRequest) requestAttributes
//                           .resolveReference(RequestAttributes.REFERENCE_REQUEST);
//        OperateationLog Operatelog = new OperateationLog();
//            // 从切面织入点处通过反射机制获取织入点处的方法
//            MethodSignature signature = (MethodSignature) joinPoint.getSignature();
//            Method method = signature.getMethod();// 获取切入点所在的方法
//            OperateLog opLog = method.getAnnotation(OperateLog.class);//获取操作
//            String OperateDesc = null;
//            if (opLog !=null){
//                String OperateModul = opLog.OperateModul();
//                String OperateType = opLog.OperateType();
//                OperateDesc = opLog.OperateDesc();
//                Operatelog.setOperateModul(OperateModul); // 操作模块
//                Operatelog.setOperateType(OperateType); // 操作类型
//                Operatelog.setOperateDesc(OperateDesc); // 操作描述
//            }
//            String urlStr = request.getRequestURL().toString();
//            // 获取请求的类名
//            String className = joinPoint.getTarget().getClass().getName();
//            String methodName = method.getName();// 获取请求的方法名
//            methodName = className + "." + methodName;
//            Operatelog.setOperateMethod(methodName); // 请求方法
//            Operatelog.setOperateRequParam(getReqestParams(request,joinPoint));
//            Operatelog.setOperateRespParam(JSON.toJSONString(keys));
////            Operatelog.setOperateUserId(user.getSysId());
//            if (!Objects.equals(OperateDesc, "登录") && !Objects.equals(OperateDesc, "退出登录")){
//                UserToken userToken = TokenFactory.validateToken(request);
//                Operatelog.setOperateUserName(userToken.getUsername());
//            }
//            Operatelog.setOperateIp(HttpUtils.getRealIpAddress(request));
//            Operatelog.setOperateUrl(request.getRequestURI());
//            Operatelog.setOperateVer(env.getPrOperatety("platform.OperateVer"));
//            Operatelog.setBasePath(StrUtil.removeSuffix(urlStr, URLUtil.url(urlStr).getPath()));
//            OperateationLogService.save(Operatelog);
//    }
    /**
     * 异常返回通知，用于拦截异常日志信息 连接点抛出异常后执行
     *
     * @param joinPoint 切入点
     * @param e         异常信息
     */
//    @AfterThrowing(pointcut = "exceptionLogPointCut()", throwing = "e")
//    public void saveExceptionLog(JoinPoint joinPoint, Throwable e) {
//        // 获取RequestAttributes
//        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
//        // 从获取RequestAttributes中获取HttpServletRequest的信息
//        HttpServletRequest request = (HttpServletRequest) requestAttributes
//                .resolveReference(RequestAttributes.REFERENCE_REQUEST);
//
//        ExceptionLog excepLog = new ExceptionLog();
//        try {
//            // 从切面织入点处通过反射机制获取织入点处的方法
//            MethodSignature signature = (MethodSignature) joinPoint.getSignature();
//            // 获取切入点所在的方法
//            Method method = signature.getMethod();
//            String urlStr = request.getRequestURL().toString();
//            String className = joinPoint.getTarget().getClass().getName();
//            String methodName = method.getName();
//            methodName = className + "." + methodName;
////            Map<String, String> rtnMap = converMap(request.getParameterMap());
////            String params = JSON.toJSONString(rtnMap);
//            excepLog.setExcRequParam(getReqestParams(request,joinPoint));
////            excepLog.setExcRequParam(params);
//            excepLog.setOperateMethod(methodName);
//            excepLog.setExcName(e.getClass().getName());
//            excepLog.setExcMessage(stackTraceToString(e.getClass().getName(), e.getMessage(), e.getStackTrace()));
////            SystemUser user = SesionUtils.getSysUserSession();
////            excepLog.setOperateUserId(user.getSysId());
////            excepLog.setOperateUserName(user.getUsername());
//            excepLog.setOperateUrl(request.getRequestURI());
//            excepLog.setOperateIp(HttpUtils.getRealIpAddress(request));
//            excepLog.setOperateVer(env.getPrOperatety("platform.OperateVer"));
//            excepLog.setBasePath(StrUtil.removeSuffix(urlStr, URLUtil.url(urlStr).getPath()));
//            exceptionLogService.save(excepLog);
//        } catch (Exception e2) {
//            e2.printStackTrace();
//        }
//    }


    /**
     * @Description: 获取请求参数
     * @author: scott
     * @date: 2020/4/16 0:10
     * @param request:  request
     * @param joinPoint:  joinPoint
     * @Return: java.lang.String
     */
    private String getRequestParams(HttpServletRequest request, JoinPoint joinPoint) {
        String httpMethod = request.getMethod();
        return "";
    }

    /**
     * 转换request 请求参数
     *
     * @param paramMap request获取的参数数组
     */
     public Map<String, String> converMap(Map<String, String[]> paramMap) {
        Map<String, String> rtnMap = new HashMap<String, String>();
        for(String key : paramMap.keySet()) {
                 rtnMap.put(key, paramMap.get(key)[0]);
            }
        return rtnMap;
     }

    /**
     * 转换异常信息为字符串
     *
     * @param exceptionName    异常名称
     * @param exceptionMessage 异常信息
     * @param elements         堆栈信息
     */
     public String stackTraceToString(String exceptionName, String exceptionMessage, StackTraceElement[] elements) {
        StringBuilder strbuff = new StringBuilder();
        for (StackTraceElement stet : elements) {
            strbuff.append(stet).append("\n");
            }
         return exceptionName + ":" + exceptionMessage + "\n\t" + strbuff;
     }
}

