package com.ouhaochen.bot.xbot.utils;


import org.dromara.hutool.core.reflect.FieldUtil;
import org.dromara.hutool.core.util.RandomUtil;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

public final class ReflectUtil extends FieldUtil {

    public static Set<String> getProperty(Class<?> clazz) {
        //获取filed数组
        Set<String> resultList = new HashSet<>();
        Field[] fields = clazz.getDeclaredFields();
        for (Field field : fields) {
            String property = field.getName();
            if (!property.equals("serialVersionUID")) {
                resultList.add(property);
            }
        }
        return resultList;
    }

    private static final Map<Class<?>, Object> DEFAULT_VALUES = new HashMap<>();

    static {
        DEFAULT_VALUES.put(Integer.class, -1);
        DEFAULT_VALUES.put(Long.class, -1);
        DEFAULT_VALUES.put(Float.class, -1.0);
        DEFAULT_VALUES.put(Double.class, -1.0);
        DEFAULT_VALUES.put(Boolean.class, false);
        DEFAULT_VALUES.put(String.class, "");
        DEFAULT_VALUES.put(LocalDateTime.class, LocalDateTime.now());
        DEFAULT_VALUES.put(List.class, new ArrayList<>());
        DEFAULT_VALUES.put(Map.class, new HashMap<>());
        DEFAULT_VALUES.put(BigDecimal.class, BigDecimal.valueOf(-1L));
    }

    public static void setDefaultValue(Object object, String... excludes) {
        setDefaultValue(object, null, excludes);
    }

    public static void setDefaultValue(Object object, Map<Class<?>, Object> defaultValueMap, String... excludes) {
        defaultValueMap = copy(defaultValueMap, DEFAULT_VALUES);
        Field[] fieldsDirectly = getFieldsDirectly(object.getClass(), false);
        HashSet<String> ex = new HashSet<>(Arrays.asList(excludes));
        for (Field field : fieldsDirectly) {
            if (ex.contains(field.getName())) {
                continue;
            }
            setValue(object, field, defaultValueMap.get(field.getType()));
        }
    }

    public static <K, V> Map<K, V> copy(Map<K, V> target, Map<K, V> resource) {
        if (Objects.isNull(target)) target = new LinkedHashMap<>();
        for (K key : resource.keySet()) {
            target.putIfAbsent(key, resource.get(key));
        }
        return target;
    }

    public static <T> void setValue(Object object, Field field, T t) {
        if (null == getFieldValue(object, field)) {
            setFieldValue(object, field, t);
        }
    }

    public static Object convertByType(String val, Type type) {
        Object value = val;
        if (Double.class == type) {
            value = Double.parseDouble(val);
        } else if (Integer.class == type) {
            value = Integer.parseInt(val);
        } else if (Long.class == type) {
            value = Long.valueOf(val);
        } else if (Float.class == type) {
            value = Float.valueOf(val);
        } else if (Boolean.class == type) {
            value = Boolean.valueOf(val);
        } else if (Short.class == type) {
            value = Short.valueOf(val);
        } else if (BigDecimal.class == type) {
            value = BigDecimal.valueOf(Double.parseDouble(val));
        }
        return value;
    }

    public static <T> List<T> generateFakeData(Class<T> tClass, Integer num) {
        try {
            List<Method> methods = Arrays.stream(tClass.getDeclaredMethods()).filter(x -> x.getName().contains("set")).collect(Collectors.toList());
            List<T> res = new ArrayList<>(num);
            for (int i = 0; i < num; i++) {
                T t = tClass.newInstance();
                for (Method x : methods) {
                    Class<?>[] parameterTypes = x.getParameterTypes();
                    if (parameterTypes.length == 1) {
                        Class<?> parameterType = parameterTypes[0];
                        if (parameterType.equals(String.class)) {
                            x.invoke(t, RandomUtil.randomString(10));
                        } else if (parameterType.equals(Long.class)) {
                            x.invoke(t, RandomUtil.randomLong(1000000));
                        } else if (parameterType.equals(Integer.class)) {
                            x.invoke(t, RandomUtil.randomInt(1000000));
                        } else if (parameterType.equals(LocalDateTime.class)) {
                            x.invoke(t, LocalDateTime.now());
                        }
                    }
                }
                res.add(t);
            }
            return res;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
