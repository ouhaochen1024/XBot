package com.ouhaochen.bot.xbot.configs;

import com.baomidou.mybatisplus.annotation.DbType;
import com.baomidou.mybatisplus.extension.plugins.MybatisPlusInterceptor;
import com.baomidou.mybatisplus.extension.plugins.inner.PaginationInnerInterceptor;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@MapperScan("com.ouhaochen.bot.xbot.mapper")
public class MybatisPlusConfig {

    @Value("${spring.datasource.driver-class-name}")
    private String driverClassName;

    /**
     * 添加分页插件
     */
    @Bean
    public MybatisPlusInterceptor mybatisPlusInterceptor() {
        MybatisPlusInterceptor interceptor = new MybatisPlusInterceptor();
        switch (driverClassName) {
            case "com.mysql.cj.jdbc.Driver", "com.mysql.jdbc.Driver":
                interceptor.addInnerInterceptor(new PaginationInnerInterceptor(DbType.MYSQL));
                break;
            case "org.postgresql.Driver":
                interceptor.addInnerInterceptor(new PaginationInnerInterceptor(DbType.POSTGRE_SQL));
                break;
            default:
                break;
        }
        return interceptor;
    }
}