//package com.spacex.qianhui.config.aspect;
//
//import cn.hutool.core.util.StrUtil;
//import com.alibaba.fastjson.JSON;
//import com.alibaba.fastjson.JSONObject;
//import com.spacex.qianhui.config.annotions.Record;
//import com.spacex.qianhui.constant.Topics;
//import com.spacex.qianhui.model.ApiLogModel;
//import com.spacex.qianhui.utils.IdUtil;
//import com.spacex.qianhui.utils.IpUtil;
//import com.spacex.qianhui.utils.JsonUtil;
//import com.spacex.qianhui.utils.UserUtils;
//import io.swagger.annotations.ApiOperation;
//import lombok.extern.slf4j.Slf4j;
//import org.aspectj.lang.JoinPoint;
//import org.aspectj.lang.ProceedingJoinPoint;
//import org.aspectj.lang.annotation.Around;
//import org.aspectj.lang.annotation.Aspect;
//import org.aspectj.lang.annotation.Pointcut;
//import org.aspectj.lang.reflect.MethodSignature;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.beans.factory.annotation.Value;
//import org.springframework.kafka.core.KafkaTemplate;
//import org.springframework.stereotype.Component;
//import org.springframework.validation.BindingResult;
//import org.springframework.web.context.request.RequestAttributes;
//import org.springframework.web.context.request.RequestContextHolder;
//import org.springframework.web.multipart.MultipartFile;
//
//import javax.servlet.ServletRequest;
//import javax.servlet.ServletResponse;
//import javax.servlet.http.HttpServletRequest;
//import java.lang.reflect.Method;
//import java.time.LocalDateTime;
//import java.util.Arrays;
//import java.util.HashMap;
//import java.util.Map;
//
//@Component
//@Aspect
//@Slf4j
//public class ApiLogAspect {
//
//    @Value("${spring.profiles.active}")
//    private String active;
//    @Autowired
//    private KafkaTemplate<String, Object> kafkaTemplate;
//
//    @Pointcut("execution(* com.xxx.controller..*.*(..))")
//    public void controller() {
//    }
//
//    @Around("controller()")
//    public Object around(ProceedingJoinPoint point) throws Throwable {
//        if (!("prod").equals(active)) return point.proceed();
//        //获取请求的方法
//        MethodSignature signature = (MethodSignature) point.getSignature();
//        Method method = signature.getMethod();
//        Record need2Record = method.getAnnotation(Record.class);
//        if (null != need2Record && !need2Record.need()) return point.proceed();
//        int code = 200;
//        String status = "success";
//        String result = "";
//        Long userId = UserUtils.getUserId();
//        String username = UserUtils.getUsername();
//        if ("shuju9527".equals(username) || "17857501781".equals(username) || "13324512862".equals(username))
//            return point.proceed();
//        String nickname = UserUtils.getNickname();
//        Long companyId = UserUtils.getCompanyId();
//        LocalDateTime beginTime = LocalDateTime.now();
//        RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
//        assert requestAttributes != null;
//        HttpServletRequest request = (HttpServletRequest) requestAttributes.resolveReference(RequestAttributes.REFERENCE_REQUEST);
//        String requestParam = getMethodArgs(point);
//        String ip = IpUtil.getRealIpAddress(request);
//        assert request != null;
//        String url = request.getRequestURI();
//        String ua = request.getHeader("User-Agent");
//        String apiOperation = method.getAnnotation(ApiOperation.class).value();
//        if (StrUtil.isNotBlank(apiOperation) && apiOperation.equals("用户-登录")) return point.proceed();
//        String className = point.getTarget().getClass().getName();
//        String methodName = method.getName();
//        String methodStr = className + "." + methodName;
//        try {
//            //放行请求的主进程并拿到返回值
//            Object obj = point.proceed();
//            if (obj != null) {
//                Map<String, String> resultMap = handlerResult(obj);
//                code = Integer.parseInt(resultMap.get("code"));
//                result = resultMap.get("result");
//            }
//            status = "操作成功";
//            return obj;
//        } catch (Exception e) {
//            result = e.getMessage();
//            status = "操作失败";
//            throw e;
//        } finally {
//            saveLog(beginTime, LocalDateTime.now(), apiOperation, userId, username, nickname, companyId, ip,
//                    methodStr, url, ua, requestParam, result, code, status);
//        }
//    }
//
//    private void saveLog(LocalDateTime beginTime, LocalDateTime now, String apiOperation, Long userId,
//                         String username, String nickname, Long companyId, String ip, String methodStr, String url, String ua,
//                         String requestParam, String result, Integer code, String status) {
//        ApiLogModel model = new ApiLogModel()
//                .setId(IdUtil.nextId())
//                .setBeginTime(beginTime)
//                .setEndTime(now)
//                .setUserId(userId)
//                .setUsername(username)
//                .setNickname(nickname)
//                .setCompanyId(companyId)
//                .setApiOperation(apiOperation)
//                .setIp(ip)
//                .setMethod(methodStr)
//                .setUrl(url)
//                .setUa(ua)
//                .setRequestParam(requestParam)
//                .setResult(result)
//                .setCode(code)
//                .setStatus(status);
//
//        try {
//            kafkaTemplate.send(Topics.QH_APP_LOG_API, model);
//        } catch (Exception e) {
//            log.error("保存接口请求日志时出错", e);
//        }
//    }
//
//    private String getMethodArgs(JoinPoint point) {
//        Object[] args = point.getArgs();
//        if (args == null || args.length == 0) {
//            return "";
//        }
//        try {
//            Map<String, Object> params = new HashMap<>();
//            String[] parameterNames = ((MethodSignature) point.getSignature()).getParameterNames();
//            for (int i = 0; i < parameterNames.length; i++) {
//                Object arg = args[i];
//                // 过滤不能转换成JSON的参数
//                if ((arg instanceof BindingResult || arg instanceof ServletRequest) || (arg instanceof ServletResponse)) {
//                    continue;
//                } else if ((arg instanceof MultipartFile)) {
//                    arg = arg.toString();
//                }
//                params.put(parameterNames[i], arg);
//            }
//            String resultStr = JSONObject.toJSONString(params);
//            return JsonUtil.filterEmptyValues(JSON.parseObject(resultStr)).toJSONString();
//        } catch (Exception e) {
//            log.error("接口出入参日志打印切面处理请求参数异常", e);
//        }
//        return Arrays.toString(args);
//    }
//
//
//    /**
//     * 返回结果简单处理
//     * 1）把返回结果转成String，方便输出。
//     * 2）返回结果太长则截取（最多500个字符），方便展示。
//     *
//     * @param result 原方法调用的返回结果
//     * @return 处理后的
//     */
//    private Map<String, String> handlerResult(Object result) {
//        Map<String, String> map = new HashMap<>();
//        if (result == null) {
//            return null;
//        }
//        String resultStr;
//        try {
//            if (result instanceof String) {
//                resultStr = (String) result;
//            } else {
//                // 如果返回结果非String类型，转换成JSON格式的字符串
//                resultStr = JSONObject.toJSONString(result);
//                JSONObject jsonObject = JSON.parseObject(resultStr);
//                map.put("code", jsonObject.get("code").toString());
//            }
//            if (resultStr.length() > 500) {
//                resultStr = resultStr.substring(0, 500);
//            }
//        } catch (Exception e) {
//            resultStr = result.toString();
//            map.put("code", "9527");
//            log.error("接口出入参日志打印切面处理返回参数异常", e);
//        }
//        map.put("result", resultStr);
//        return map;
//    }
//}
