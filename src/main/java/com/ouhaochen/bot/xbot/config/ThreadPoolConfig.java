package com.ouhaochen.bot.xbot.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.aop.interceptor.AsyncUncaughtExceptionHandler;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.AsyncConfigurer;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.Executor;
import java.util.concurrent.ThreadPoolExecutor;

/**
 * 重写默认线程池配置
 *
 * @author yinpengzhou
 */
@Slf4j
@Configuration
@EnableAsync
public class ThreadPoolConfig implements AsyncConfigurer {

    @Value("${spring.task.execution.thread-name-prefix}")
    private String namePrefix;

    @Value("${spring.task.execution.pool.core-size}")
    private int corePoolSize;

    @Value("${spring.task.execution.pool.max-size}")
    private int maxPoolSize;

    @Value("${spring.task.execution.pool.queue-capacity}")
    private int queueCapacity;

    @Value("${spring.task.execution.pool.keep-alive}")
    private int keepAliveSeconds;


    @Override
    @Bean("myThreadPool")
    public Executor getAsyncExecutor() {
        ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        // 最大线程数
        executor.setMaxPoolSize(maxPoolSize);
        // 核心线程数
        executor.setCorePoolSize(corePoolSize);
        // 任务队列的大小
        executor.setQueueCapacity(queueCapacity);
        // 线程前缀名
        executor.setThreadNamePrefix(namePrefix);
        // 线程存活时间
        executor.setKeepAliveSeconds(keepAliveSeconds);
    /*
      当poolSize已达到maxPoolSize，如何处理新任务（是拒绝还是交由其它线程处理）
      CallerRunsPolicy：不在新线程中执行任务，而是由调用者所在的线程来执行
     */
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.CallerRunsPolicy());
        executor.initialize();
        return executor;
    }

    /**
     * 异步任务中异常处理
     */
    @Override
    public AsyncUncaughtExceptionHandler getAsyncUncaughtExceptionHandler() {
        return (ex, method, params) -> {
            log.error("=========================={}=======================", ex.getMessage(), ex);
            log.error("exception method:{}", method.getName());
        };
    }
}