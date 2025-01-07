package com.ouhaochen.bot.xbot.web.config;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import org.springdoc.core.models.GroupedOpenApi;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class Knife4jConfiguration {

    @Value("${xbot.version}")
    private String version;

    @Bean
    public OpenAPI openAPI() {
        return new OpenAPI()
                .info(new Info()
                        .title("XBot API")
                        .contact(new Contact().name("ouhaochen").email("yipengzhou@outlook.com"))
                        .license(new License().name("GPL-3.0 license").url("https://github.com/ouhaochen1024/XBot/blob/master/LICENSE"))
                        .version(version)
                        .description("XBot的接口文档"));
    }

    @Bean
    public GroupedOpenApi defaultGroup() {
        return GroupedOpenApi.builder().group("defaultGroup")
                .packagesToScan("com.ouhaochen.bot.xbot.web.controller")
                .pathsToMatch("/apis/**")
                .build();
    }
}