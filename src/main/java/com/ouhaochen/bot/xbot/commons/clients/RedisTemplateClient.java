package com.ouhaochen.bot.xbot.commons.clients;


import com.ouhaochen.bot.xbot.commons.exception.BusinessException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.dromara.hutool.core.collection.CollUtil;
import org.dromara.hutool.core.text.StrUtil;
import org.springframework.data.redis.core.RedisCallback;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.concurrent.TimeUnit;

@Slf4j
@Component
@RequiredArgsConstructor
public class RedisTemplateClient {

    private final RedisTemplate<String, Object> redisTemplate;

    /**
     * 根据key读取数据
     */
    public Object get(final String key) {
        if (StrUtil.isBlank(key)) {
            return null;
        }
        try {
            return redisTemplate.opsForValue().get(key);
        } catch (Exception e) {
            throw new BusinessException("获取缓存key： " + key + "失败");
        }
    }

    /**
     * 写入对象数据
     */
    public void set(final String key, Object value) {
        if (StrUtil.isBlank(key)) {
            return;
        }
        try {
            redisTemplate.opsForValue().set(key, value);
            log.info("存入redis成功，key：{}，value：{}", key, value);
        } catch (Exception e) {
            throw new BusinessException("key：" + key + "，value：" + value + "，存入redis失败");
        }
    }

    /**
     * 写入设定过期时间的数据
     */
    public boolean set(final String key, Object value, long timeout, TimeUnit unit) {
        if (StrUtil.isBlank(key)) {
            return false;
        }
        try {
            redisTemplate.opsForValue().set(key, value, timeout, unit);
            log.info("存入redis成功，key：{}，value：{}", key, value);
            return true;
        } catch (Exception e) {
            throw new BusinessException("key：" + key + "，value：" + value + "，存入redis失败");
        }
    }

    /**
     * 获得Hash对象value数据
     */
    public Object getHashValue(final String key, Object hashKey) {
        if (StrUtil.isBlank(key)) {
            return null;
        }
        try {
            return redisTemplate.opsForHash().get(key, hashKey.toString());
        } catch (Exception e) {
            throw new BusinessException("获取缓存key： " + key + "失败");
        }
    }

    /**
     * 获得Hash对象value数据
     */
    public void putHash(final String key, Object hashKey, Object hashValue) {
        try {
            redisTemplate.opsForHash().put(key, hashKey.toString(), hashValue);
        } catch (Exception e) {
            throw new BusinessException("删除缓存key： " + key + "失败");
        }
    }

    /**
     * 写入Hash对象数据 k-map
     */
    public boolean putHash(final String key, Map<String, Object> map) {
        if (StrUtil.isBlank(key)) {
            return false;
        }
        try {
            redisTemplate.opsForHash().putAll(key, map);
            log.info("存入redis成功，key：{}，map：{}", key, map);
            return true;
        } catch (Exception e) {
            throw new BusinessException("key：" + key + "，map：" + map + "，存入redis失败");
        }
    }

    /**
     * 写入带过期时间的Hash对象数据 k-map
     */
    public boolean putHash(final String key, Map<String, Object> map, long timeout, TimeUnit unit) {
        if (StrUtil.isBlank(key)) {
            return false;
        }
        try {
            redisTemplate.opsForHash().putAll(key, map);
            log.info("存入redis成功，key：{}，map：{}", key, map);
            redisTemplate.opsForHash().getOperations().expire(key, timeout, unit);
            return true;
        } catch (Exception e) {
            throw new BusinessException("key：" + key + "，map：" + map + "，存入redis失败");
        }
    }

    /**
     * 删除
     *
     * @return
     */
    public void delete(final String key) {
        if (StrUtil.isBlank(key)) {
            return;
        }
        try {
            redisTemplate.delete(key);
        } catch (Exception e) {
            throw new BusinessException("删除key： " + key + "失败");
        }
    }

    public void deleteHash(final String key, Object... hashKey) {
        if (StrUtil.isBlank(key)) {
            return;
        }
        try {
            redisTemplate.opsForHash().delete(key, Arrays.toString(hashKey));
        } catch (Exception e) {
            throw new BusinessException("删除key： " + key + "失败");
        }
    }

    public ZSetOperations<String, Object> zSetOperations() {
        return redisTemplate.opsForZSet();
    }

    public void expire(String key, long time, TimeUnit timeUnit) {
        redisTemplate.expire(key, time, timeUnit);
    }

    public Map<String, String> hGetAll(String key) {
        return redisTemplate.execute((RedisCallback<Map<String, String>>) con -> {
            Map<byte[], byte[]> result = con.hashCommands().hGetAll(key.getBytes());
            if (CollUtil.isEmpty(result)) {
                return new HashMap<>(0);
            }
            Map<String, String> ans = new HashMap<>(result.size());
            for (Map.Entry<byte[], byte[]> entry : result.entrySet()) {
                ans.put(new String(entry.getKey()), new String(entry.getValue()));
            }
            return ans;
        });
    }

    public Map<String, Map<String, String>> hGetAll(Set<String> keys) {
        return redisTemplate.execute((RedisCallback<Map<String, Map<String, String>>>) con -> {
            Iterator<String> it = keys.iterator();
            Map<String, Map<String, String>> mapList = new HashMap<>();
            while (it.hasNext()) {
                String key = it.next();
                Map<byte[], byte[]> result = con.hashCommands().hGetAll(key.getBytes());
                Map<String, String> ans;
                if (CollUtil.isEmpty(result)) {
                    return new HashMap<>(0);
                }
                ans = new HashMap<>(result.size());
                for (Map.Entry<byte[], byte[]> entry : result.entrySet()) {
                    ans.put(new String(entry.getKey()), new String(entry.getValue()));
                }
                mapList.put(key, ans);
            }
            return mapList;
        });
    }

}
