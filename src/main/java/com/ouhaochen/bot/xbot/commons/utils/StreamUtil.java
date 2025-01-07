package com.ouhaochen.bot.xbot.commons.utils;

import org.dromara.hutool.core.collection.CollUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.ToDoubleFunction;
import java.util.function.ToIntFunction;
import java.util.stream.Collectors;

/**
 * author: ouhaochen
 * date: 07/01/2023
 */
public final class StreamUtil {

    private static final Logger logger = LoggerFactory.getLogger(StreamUtil.class);

    /**
     * list 某属性 求和
     */
    public static <T> Integer sum(List<T> data, ToIntFunction<? super T> fun) {
        if (CollUtil.isEmpty(data)) {
            logger.info("输入列表为空，返回默认求和值 0");
            return 0;
        }
        return data.stream().mapToInt(fun).sum();
    }

    /**
     * list 某属性 求和
     */
    public static <T> Double sum(List<T> data, ToDoubleFunction<? super T> fun) {
        if (CollUtil.isEmpty(data)) {
            logger.info("输入列表为空，返回默认求和值 0.0");
            return 0.0;
        }
        return data.stream().mapToDouble(fun).sum();
    }

    /**
     * 映射 默认去重
     */
    public static <T, R> List<R> mapping(List<T> data, Function<T, R> fun) {
        return mapping(data, fun, true);
    }

    /**
     * 映射 可以选不去重
     */
    public static <T, R> List<R> mapping(List<T> data, Function<T, R> fun, boolean isDistinct) {
        if (CollUtil.isEmpty(data)) {
            logger.info("输入列表为空，返回空列表");
            return Collections.emptyList();
        }
        if (isDistinct) {
            return data.stream().map(fun).distinct().collect(Collectors.toList());
        }
        return data.stream().map(fun).collect(Collectors.toList());
    }

    /**
     * 1对1 map
     */
    public static <K, V> Map<K, V> toMap(List<V> data, Function<V, K> fun) {
        if (CollUtil.isEmpty(data)) {
            logger.info("输入列表为空，返回空Map");
            return Collections.emptyMap();
        }
        return data.stream().collect(Collectors.toMap(fun, Function.identity()));
    }

    /**
     * list 转 map 一对一
     */
    public static <T, K, V> Map<K, V> toMap(List<T> data, Function<T, K> fun1, Function<T, V> fun2) {
        if (CollUtil.isEmpty(data)) {
            logger.info("输入列表为空，返回空Map");
            return Collections.emptyMap();
        }
        return data.stream().collect(Collectors.toMap(fun1, fun2));
    }

    /**
     * list 转 groupMap 一对多
     */
    public static <T, K, V> Map<K, List<V>> toGroupMap(List<T> data, Function<T, K> keyFun, Function<T, V> valFun) {
        if (CollUtil.isEmpty(data)) {
            logger.info("输入列表为空，返回空Map");
            return Collections.emptyMap();
        }
        return data.stream().collect(Collectors.groupingBy(keyFun, Collectors.mapping(valFun, Collectors.toList())));
    }

    /**
     * 1对多 分组 map
     */
    public static <K, V> Map<K, List<V>> toGroupMap(List<V> data, Function<V, K> fun) {
        if (CollUtil.isEmpty(data)) {
            logger.info("输入列表为空，返回空Map");
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
            logger.info("输入列表为空，返回空列表");
            return Collections.emptyList();
        }
        return data.stream().collect(Collectors.collectingAndThen(
                Collectors.toCollection(() -> new TreeSet<>(Comparator.comparing(comparator))),
                ArrayList::new));
    }

    /**
     * 过滤
     */
    public static <T> List<T> filter(List<T> data, Predicate<T> pre) {
        if (CollUtil.isEmpty(data)) {
            logger.info("输入列表为空，返回空列表");
            return Collections.emptyList();
        }
        return data.stream().filter(pre).collect(Collectors.toList());
    }

    /**
     * list 排序
     */
    public static <T> List<T> sorted(List<T> data, Comparator<T> comparator) {
        if (CollUtil.isEmpty(data)) {
            logger.info("输入列表为空，返回空列表");
            return Collections.emptyList();
        }
        return data.stream().sorted(comparator).collect(Collectors.toList());
    }

    /**
     * map 根据 key 排序
     */
    public static <K extends Comparable<? super K>, V> Map<K, V> mapSortedByKey(Map<K, V> map, boolean isDesc) {
        if (CollUtil.isEmpty(map)) {
            logger.info("输入Map为空，返回空Map");
            return Collections.emptyMap();
        }
        Map<K, V> result = new LinkedHashMap<>(map.size());
        map.entrySet().stream()
                .sorted(isDesc ? Map.Entry.<K, V>comparingByKey().reversed() : Map.Entry.comparingByKey())
                .forEachOrdered(e -> result.put(e.getKey(), e.getValue()));
        return result;
    }

    /**
     * map 根据 key 排序后转 list
     */
    public static <K extends Comparable<? super K>, V> List<V> map2ListSortedByKey(Map<K, V> map, boolean isDesc) {
        if (CollUtil.isEmpty(map)) {
            logger.info("输入Map为空，返回空列表");
            return Collections.emptyList();
        }
        return map.entrySet().stream()
                .sorted(isDesc ? Map.Entry.<K, V>comparingByKey().reversed() : Map.Entry.comparingByKey())
                .map(Map.Entry::getValue)
                .collect(Collectors.toList());
    }

    /**
     * 根据 map 的 value 排序
     */
    public static <K, V extends Comparable<? super V>> Map<K, V> mapSortedByValue(Map<K, V> map, boolean isDesc) {
        if (CollUtil.isEmpty(map)) {
            logger.info("输入Map为空，返回空Map");
            return Collections.emptyMap();
        }
        Map<K, V> result = new LinkedHashMap<>(map.size());
        map.entrySet().stream()
                .sorted(isDesc ? Map.Entry.<K, V>comparingByValue().reversed() : Map.Entry.comparingByValue())
                .forEachOrdered(e -> result.put(e.getKey(), e.getValue()));
        return result;
    }

    /**
     * 根据 map 的 value 排序后转 list
     */
    public static <K, V extends Comparable<? super V>> List<V> map2ListSortedByValue(Map<K, V> map, boolean isDesc) {
        if (CollUtil.isEmpty(map)) {
            logger.info("输入Map为空，返回空列表");
            return Collections.emptyList();
        }
        return map.entrySet().stream()
                .sorted(isDesc ? Map.Entry.<K, V>comparingByValue().reversed() : Map.Entry.comparingByValue())
                .map(Map.Entry::getValue)
                .collect(Collectors.toList());
    }

    /**
     * 去重
     */
    public static <T> List<T> distinct(List<T> data) {
        if (CollUtil.isEmpty(data)) {
            logger.info("输入列表为空，返回空列表");
            return Collections.emptyList();
        }
        return data.stream().distinct().collect(Collectors.toList());
    }

    /**
     * 判断是否包含匹配元素
     */
    public static <T> boolean anyMatch(List<T> data, Predicate<T> pre) {
        if (CollUtil.isEmpty(data)) {
            logger.info("输入列表为空，返回 false");
            return false;
        }
        return data.stream().anyMatch(pre);
    }

    /**
     * 将 list 进行 join 操作
     */
    public static String join(List<String> data, String delimiter) {
        if (CollUtil.isEmpty(data)) {
            logger.info("输入列表为空，返回空字符串");
            return "";
        }
        return data.stream().collect(Collectors.joining(delimiter == null ? "" : delimiter));
    }

    /**
     * 将 list 进行检查操作 所有元素都一样返回 true
     */
    public static <T> boolean allEquals(List<T> data) {
        if (CollUtil.isEmpty(data)) {
            logger.info("输入列表为空，返回 true");
            return true;
        }
        return data.stream().distinct().count() == 1;
    }

    /**
     * 将 list 中对象的某个属性进行重复检查 有重复元素返回 true
     */
    public static <T> boolean hasRepeat(List<T> data, Function<? super T, ?> fun) {
        if (CollUtil.isEmpty(data)) {
            logger.info("输入列表为空，返回 false");
            return false;
        }
        Integer size = data.size();
        List<?> newData = data.stream().map(fun).distinct().collect(Collectors.toList());
        return !size.equals(newData.size());
    }
}
