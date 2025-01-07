package com.ouhaochen.bot.xbot.commons.redis.clients;


import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.dromara.hutool.core.collection.CollUtil;
import org.dromara.hutool.core.text.StrUtil;
import org.springframework.data.redis.core.RedisCallback;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.stereotype.Component;

import java.lang.RuntimeException;
import java.util.*;
import java.util.concurrent.TimeUnit;

@Slf4j
@Component
@RequiredArgsConstructor
public class RedisTemplateClient {

    private final RedisTemplate<String, Object> redisTemplate;

    public boolean hasKey(final String key) {
        if (StrUtil.isBlank(key)) {
            return false;
        }
        try {
            return redisTemplate.hasKey(key);
        } catch (Exception e) {
            throw new RuntimeException("操作缓存失败", e);
        }
    }

    public boolean hasHashKey(String key, String hashKey) {
        if (StrUtil.isBlank(key) || StrUtil.isBlank(hashKey)) {
            return false;
        }
        try {
            return redisTemplate.opsForHash().hasKey(key, hashKey);
        } catch (Exception e) {
            throw new RuntimeException("操作缓存失败", e);
        }
    }

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
            throw new RuntimeException("操作缓存失败", e);
        }
    }

    public Set<Object> getSet(final String key) {
        if (StrUtil.isBlank(key)) {
            return null;
        }
        try {
            return redisTemplate.opsForSet().members(key);
        } catch (Exception e) {
            throw new RuntimeException("操作缓存失败", e);
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
        } catch (Exception e) {
            throw new RuntimeException("操作缓存失败", e);
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
            return true;
        } catch (Exception e) {
            throw new RuntimeException("操作缓存失败", e);
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
            return true;
        } catch (Exception e) {
            throw new RuntimeException("操作缓存失败", e);
        }
    }

    /**
     * 获得Hash对象value数据
     */
    public Object getHashValue(final String key, final String hashKey) {
        if (StrUtil.isBlank(key)) {
            return null;
        }
        try {
            return redisTemplate.opsForHash().get(key, hashKey);
        } catch (Exception e) {
            throw new RuntimeException("操作缓存失败", e);
        }
    }

    /**
     * 获得Hash对象value数据
     */
    public void putHash(final String key, final String hashKey, Object hashValue) {
        try {
            redisTemplate.opsForHash().put(key, hashKey, hashValue);
        } catch (Exception e) {
            throw new RuntimeException("操作缓存失败", e);
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
            redisTemplate.opsForHash().getOperations().expire(key, timeout, unit);
            return true;
        } catch (Exception e) {
            throw new RuntimeException("操作缓存失败", e);
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
            throw new RuntimeException("操作缓存失败", e);
        }
    }

    public void deleteHash(final String key,final String... hashKey) {
        if (StrUtil.isBlank(key)) {
            return;
        }
        try {
            redisTemplate.opsForHash().delete(key, Arrays.toString(hashKey));
        } catch (Exception e) {
            throw new RuntimeException("操作缓存失败", e);
        }
    }

    public ZSetOperations<String, Object> zSetOperations() {
        return redisTemplate.opsForZSet();
    }

    public void expire(final String key, long time, TimeUnit timeUnit) {
        redisTemplate.expire(key, time, timeUnit);
    }

    public Map<String, String> hGetAll(final String key) {
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

    public Map<String, Map<String, String>> hGetAll(final Set<String> keys) {
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

    public void putSet(final String key, Object... value) {
        if (StrUtil.isBlank(key)) {
            return;
        }
        try {
            redisTemplate.opsForSet().add(key, value);
        } catch (Exception e) {
            throw new RuntimeException("操作缓存失败", e);
        }
    }
}
