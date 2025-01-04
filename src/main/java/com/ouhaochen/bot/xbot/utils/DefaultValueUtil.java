package com.ouhaochen.bot.xbot.utils;

import org.dromara.hutool.core.collection.CollUtil;
import org.dromara.hutool.core.util.ObjUtil;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.util.Collection;

public final class DefaultValueUtil {

    public static void setDefaultValue(Object obj, String targetString) throws Exception {
        for (Field field : obj.getClass().getDeclaredFields()) {
            field.setAccessible(true);
            Object o = field.get(obj);
            if (ObjUtil.isNull(o) || (ObjUtil.isNotNull(o) && o.toString().equals(targetString))) {
                if (field.getGenericType().toString().equals("class java.lang.String")) {
                    field.set(obj, "");
                } else if (field.getGenericType().toString().equals("class java.lang.Integer")) {
                    field.set(obj, -99999);
                } else if (field.getGenericType().toString().equals("class java.lang.Double")) {
                    field.set(obj, -99999.0);
                } else if (field.getGenericType().toString().equals("class java.lang.Long")) {
                    field.set(obj, -99999L);
                } else if (field.getGenericType().toString().equals("class java.math.BigDecimal")) {
                    field.set(obj, BigDecimal.valueOf(-99999));
                }
            }
        }
    }

    public static void setNullValue(Object obj, String targetString) throws Exception {
        for (Field field : obj.getClass().getDeclaredFields()) {
            field.setAccessible(true);
            Object o = field.get(obj);
            if (ObjUtil.isNotNull(o) && o.toString().equals(targetString)) {
                if (field.getGenericType().toString().equals("class java.lang.String")) {
                    field.set(obj, null);
                } else if (field.getGenericType().toString().equals("class java.lang.Integer")) {
                    field.set(obj, null);
                } else if (field.getGenericType().toString().equals("class java.lang.Double")) {
                    field.set(obj, null);
                } else if (field.getGenericType().toString().equals("class java.lang.Long")) {
                    field.set(obj, null);
                } else if (field.getGenericType().toString().equals("class java.math.BigDecimal")) {
                    field.set(obj, null);
                }
            }
        }
    }

    public static void setCollectionDefaultValue(Collection<Object> collection, String targetString, Boolean isNull) {
        if(CollUtil.isEmpty(collection)){
            return;
        }
        collection.forEach(
                item -> {
                    try {
                        if (isNull) {
                            setNullValue(item, targetString);
                        } else {
                            setDefaultValue(item, targetString);
                        }
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                });
    }
}
