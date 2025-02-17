package com.ouhaochen.bot.xbot.commons.redis.clients;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

@Slf4j
@Component
@RequiredArgsConstructor
public class RedisTemplateClient {

    private final RedisTemplate<String, Object> redisTemplate;

    public boolean hasKey(final String key) {
        if (key == null || key.isEmpty()) {
            return false;
        }
        try {
            return redisTemplate.hasKey(key);
        } catch (Exception e) {
            throw new RuntimeException("操作缓存失败", e);
        }
    }

    public boolean hasHashKey(String key, String hashKey) {
        if (key == null || key.isEmpty() || hashKey == null || hashKey.isEmpty()) {
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
        if (key == null || key.isEmpty()) {
            return null;
        }
        try {
            return redisTemplate.opsForValue().get(key);
        } catch (Exception e) {
            throw new RuntimeException("操作缓存失败", e);
        }
    }

    public Set<Object> getSet(final String key) {
        if (key == null || key.isEmpty()) {
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
        if (key == null || key.isEmpty()) {
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
        if (key == null || key.isEmpty()) {
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
        if (key == null || key.isEmpty()) {
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
        if (key == null || key.isEmpty()) {
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
        if (key == null || key.isEmpty()) {
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
     */
    public void delete(final String key) {
        if (key == null || key.isEmpty()) {
            return;
        }
        try {
            redisTemplate.delete(key);
        } catch (Exception e) {
            throw new RuntimeException("操作缓存失败", e);
        }
    }

    public void deleteHash(final String key,final Object... hashKey) {
        if (key == null || key.isEmpty()) {
            return;
        }
        try {
            redisTemplate.opsForHash().delete(key, hashKey);
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

    public Map<Object, Object> getEntries(final String key) {
        if (key == null || key.isEmpty()) {
            return null;
        }
        try {
            return redisTemplate.opsForHash().entries(key);
        } catch (Exception e) {
            throw new RuntimeException("操作缓存失败", e);
        }
    }

    public void putSet(final String key, Object... value) {
        if (key == null || key.isEmpty()) {
            return;
        }
        try {
            redisTemplate.opsForSet().add(key, value);
        } catch (Exception e) {
            throw new RuntimeException("操作缓存失败", e);
        }
    }

    public String tryLock(String key, long expireTime, TimeUnit timeUnit) {
        String lockKey = "REDIS_LOCK:" + key;
        String value = UUID.randomUUID().toString();
        Boolean result = redisTemplate.opsForValue().setIfAbsent(lockKey, value, expireTime, timeUnit);
        return Boolean.TRUE.equals(result) ? value : null;
    }

    public void releaseLock(String key, String value) {
        String lockKey = "REDIS_LOCK:" + key;
        Object currentValue = redisTemplate.opsForValue().get(lockKey);
        if (currentValue != null && currentValue.equals(value)) {
            // 校验值正确，删除键并返回删除的数量
            redisTemplate.delete(lockKey);
        }
    }
}
