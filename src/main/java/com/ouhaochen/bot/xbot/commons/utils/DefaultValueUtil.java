package com.ouhaochen.bot.xbot.commons.utils;

import org.dromara.hutool.core.collection.CollUtil;
import org.dromara.hutool.core.util.ObjUtil;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public final class DefaultValueUtil {

    private static final Map<Class<?>, Object> DEFAULT_VALUES = new HashMap<>();

    static {
        DEFAULT_VALUES.put(String.class, "");
        DEFAULT_VALUES.put(Integer.class, -1);
        DEFAULT_VALUES.put(Long.class, -1L);
        DEFAULT_VALUES.put(BigDecimal.class, BigDecimal.valueOf(-1));
        DEFAULT_VALUES.put(Byte.class, (byte) -1);
        DEFAULT_VALUES.put(Short.class, (short) -1);
        DEFAULT_VALUES.put(Float.class, -1.0f);
        DEFAULT_VALUES.put(Double.class, -1.0);
        DEFAULT_VALUES.put(Collection.class, Collections.EMPTY_LIST);
        DEFAULT_VALUES.put(Map.class, Collections.EMPTY_MAP);
        DEFAULT_VALUES.put(LocalDateTime.class, LocalDateTime.MIN.withYear(1970));
        DEFAULT_VALUES.put(long.class, -1L);
        DEFAULT_VALUES.put(int.class, -1);
        DEFAULT_VALUES.put(byte.class, (byte) -1);
        DEFAULT_VALUES.put(short.class, (short) -1);
        DEFAULT_VALUES.put(float.class, -1.0f);
        DEFAULT_VALUES.put(double.class, -1.0);
        DEFAULT_VALUES.put(boolean.class, false);
    }

    private static void setFieldDefaultValue(Object obj, Field field, String targetString) throws IllegalAccessException {
        Object o = field.get(obj);
        if (ObjUtil.isNull(o) || (ObjUtil.isNotNull(o) && o.toString().equals(targetString))) {
            Class<?> fieldType = field.getType();
            Object defaultValue = DEFAULT_VALUES.get(fieldType);
            if (defaultValue != null) {
                field.set(obj, defaultValue);
            }
        }
    }

    private static void processFields(Object obj, String targetString) throws IllegalAccessException {
        Class<?> clazz = obj.getClass();
        while (clazz != null) {
            for (Field field : clazz.getDeclaredFields()) {
                field.setAccessible(true);
                if (!("serialVersionUID").equals(field.getName())) {
                    setFieldDefaultValue(obj, field, targetString);
                }
            }
            clazz = clazz.getSuperclass();
        }
    }


    public static void setDefaultValue(Object obj, String targetString) {
        if (ObjUtil.isNull(obj)) {
            return;
        }
        try {
            processFields(obj, targetString);
        } catch (Exception e) {
            throw new RuntimeException("Error processing object", e);
        }
    }

    public static void setDefaultValue(Collection<Object> collection, String targetString) {
        if (CollUtil.isEmpty(collection)) {
            return;
        }
        collection.forEach(
                item -> {
                    try {
                        processFields(item, targetString);
                    } catch (Exception e) {
                        throw new RuntimeException("Error processing item in collection", e);
                    }
                });
    }
}
