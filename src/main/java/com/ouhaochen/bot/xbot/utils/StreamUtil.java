package com.ouhaochen.bot.xbot.utils;


import org.dromara.hutool.core.collection.CollUtil;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.ToDoubleFunction;
import java.util.function.ToIntFunction;
import java.util.stream.Collectors;

/**
 * author: yinpengzhou
 * date: 07/01/2023
 */
public final class StreamUtil {

    /**
     * list 某属性 求和
     */
    public static <T> Integer sum(List<T> data, ToIntFunction<? super T> fun) {
        if (CollUtil.isEmpty(data)) {
            return 0;
        }
        return data.stream().mapToInt(fun).sum();
    }

    /**
     * list 某属性 求和
     */
    public static <T> Double sum(List<T> data, ToDoubleFunction<? super T> fun) {
        if (CollUtil.isEmpty(data)) {
            return 0.0;
        }
        return data.stream().mapToDouble(fun).sum();
    }

    /**
     * 映射 默认去重
     */
    public static <T, R> List<R> mapping(List<T> data, Function<T, R> fun) {
        if (CollUtil.isEmpty(data)) {
            return Collections.emptyList();
        }
        return data.stream().map(fun).distinct().collect(Collectors.toList());
    }

    /**
     * 映射 可以选不去重
     */
    public static <T, R> List<R> mapping(List<T> data, Function<T, R> fun, Boolean isDistinct) {
        if (CollUtil.isEmpty(data)) {
            return Collections.emptyList();
        }
        if (isDistinct) {
            return data.stream().map(fun).distinct().collect(Collectors.toList());
        }
        return data.stream().map(fun).collect(Collectors.toList());
    }

    /**
     * 1对1map
     */
    public static <K, V> Map<K, V> toMap(List<V> data, Function<V, K> fun) {
        if (CollUtil.isEmpty(data)) {
            return Collections.emptyMap();
        }
        return data.stream().collect(Collectors.toMap(fun, Function.identity()));
    }

    /**
     * list转map  一对一
     */
    public static <T, K, V> Map<K, V> toMap(List<T> data, Function<T, K> fun1, Function<T, V> fun2) {
        if (CollUtil.isEmpty(data)) {
            return Collections.emptyMap();
        }
        return data.stream().collect(Collectors.toMap(fun1, fun2));
    }


    /**
     * list转groupMap 一对多
     */
    public static <T, K, V> Map<K, List<V>> toGroupMap(List<T> data, Function<T, K> keyFun, Function<T, V> valFun) {
        if (CollUtil.isEmpty(data)) {
            return Collections.emptyMap();
        }
        return data.stream().collect(Collectors.groupingBy(keyFun, Collectors.mapping(valFun, Collectors.toList())));
    }

    /**
     * 1对多分组map
     */
    public static <K, V> Map<K, List<V>> toGroupMap(List<V> data, Function<V, K> fun) {
        if (CollUtil.isEmpty(data)) {
            return Collections.emptyMap();
        }
        return data.stream().collect(Collectors.groupingBy(fun));
    }

    /**
     * 根据某个属性去重
     */
    public static <T, U extends Comparable<? super U>> List<T> distinctByProperty(
            List<T> data, Function<? super T, ? extends U> comparator) {
        if (CollUtil.isEmpty(data)) {
            return Collections.emptyList();
        }
        return data.stream().collect(Collectors.collectingAndThen(Collectors
                .toCollection(() -> new TreeSet<>(Comparator.comparing(comparator))), ArrayList::new));
    }

    /**
     * 过滤
     */
    public static <T> List<T> filter(List<T> data, Predicate<T> pre) {
        if (CollUtil.isEmpty(data)) {
            return data;
        }
        return data.stream().filter(pre).collect(Collectors.toList());
    }

    /**
     * list排序
     */
    public static <T> List<T> sorted(List<T> data, Comparator<T> comparator) {
        if (CollUtil.isEmpty(data)) {
            return data;
        }
        return data.stream().sorted(comparator).collect(Collectors.toList());
    }

    /**
     * map根据key排序
     */
    public static <K extends Comparable<? super K>, V extends Comparable<? super V>> Map<K, V> MapSortedByKey(
            Map<K, V> map, Boolean isDesc) {
        if (CollUtil.isEmpty(map)) {
            return map;
        }
        Map<K, V> result = new LinkedHashMap<>(map.size());
        if (isDesc) {
            map.entrySet().stream().sorted(Map.Entry.<K, V>comparingByKey().reversed())
                    .forEachOrdered(e -> result.put(e.getKey(), e.getValue()));
        } else {
            map.entrySet().stream().sorted(Map.Entry.comparingByKey())
                    .forEachOrdered(e -> result.put(e.getKey(), e.getValue()));
        }
        return result;
    }

    /**
     * map根据key排序后转list
     */
    public static <K extends Comparable<? super K>, V extends Comparable<? super V>> LinkedList<V> map2ListSortedByKey(
            Map<K, V> map, Boolean isDesc) {
        if (CollUtil.isEmpty(map)) {
            return new LinkedList<>();
        }
        LinkedList<V> result = new LinkedList<>();
        if (isDesc) {
            map.entrySet().stream().sorted(Map.Entry.<K, V>comparingByKey().reversed())
                    .forEachOrdered(e -> result.add(e.getValue()));
        } else {
            map.entrySet().stream().sorted(Map.Entry.comparingByKey())
                    .forEachOrdered(e -> result.add(e.getValue()));
        }
        return result;
    }

    /**
     * 根据map的value排序
     *
     * @param map    原始map
     * @param isDesc 是否降序
     * @return java.util.Map<K, V>
     */
    public static <K extends Comparable<? super K>, V extends Comparable<? super V>> Map<K, V> mapSortedByValue(
            Map<K, V> map, boolean isDesc) {
        if (CollUtil.isEmpty(map)) {
            return map;
        }
        Map<K, V> result = new LinkedHashMap<>(map.size());
        if (isDesc) {
            map.entrySet().stream().sorted(Map.Entry.<K, V>comparingByValue().reversed())
                    .forEachOrdered(e -> result.put(e.getKey(), e.getValue()));
        } else {
            map.entrySet().stream().sorted(Map.Entry.comparingByValue())
                    .forEachOrdered(e -> result.put(e.getKey(), e.getValue()));
        }
        return result;
    }

    /**
     * 根据map的value排序后转list
     *
     * @param map    原始map
     * @param isDesc 是否降序
     * @return java.util.LinkedList<V>
     */
    public static <K extends Comparable<? super K>, V extends Comparable<? super V>> LinkedList<V> map2ListSortedByValue(
            Map<K, V> map, boolean isDesc) {
        if (CollUtil.isEmpty(map)) {
            return new LinkedList<>();
        }
        LinkedList<V> result = new LinkedList<>();
        if (isDesc) {
            map.entrySet().stream().sorted(Map.Entry.<K, V>comparingByValue().reversed())
                    .forEachOrdered(e -> result.add(e.getValue()));
        } else {
            map.entrySet().stream().sorted(Map.Entry.comparingByValue())
                    .forEachOrdered(e -> result.add(e.getValue()));
        }
        return result;
    }

    /**
     * 去重
     */
    public static <T> List<T> distinct(List<T> data) {
        if (CollUtil.isEmpty(data)) {
            return data;
        }
        return data.stream().distinct().collect(Collectors.toList());
    }

    /**
     * 判断是否包含匹配元素
     */
    public static <T> boolean anyMatch(List<T> data, Predicate<T> pre) {
        if (CollUtil.isEmpty(data)) {
            return false;
        }
        return data.parallelStream().anyMatch(pre);
    }

    /**
     * 将list进行join操作
     */
    public static String join(List<String> data, String join) {
        if (CollUtil.isEmpty(data)) {
            return "";
        }
        return data.stream().collect(Collectors.joining(join == null ? "" : join));
    }

    /**
     * 将list进行检查操作 所有元素都一样返回true
     */
    public static <T> Boolean allEquals(List<T> data) {
        if (CollUtil.isEmpty(data)) {
            return true;
        }
        return data.stream().distinct().count() == 1;
    }

    /**
     * 将list中对象的某个属性进行重复检查 有重复元素返回true
     */
    public static <T> Boolean hasRepeat(List<T> data, Function<? super T, ?> fun) {
        if (CollUtil.isEmpty(data)) {
            return true;
        }
        Integer size = data.size();
        List<?> newData = data.stream().map(fun).distinct().collect(Collectors.toList());
        return !size.equals(newData.size());
    }

}
