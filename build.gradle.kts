plugins {
    java
    id("io.freefair.lombok") version "8.11"
    id("org.springframework.boot") version "3.4.1"
    id("io.spring.dependency-management") version "1.1.7"
}

group = "com.ouhaochen.bot"
version = "0.0.1-SNAPSHOT"

java {
    toolchain {
        languageVersion = JavaLanguageVersion.of(17)
    }
}

configurations {
    compileOnly {
        extendsFrom(configurations.annotationProcessor.get())
    }
    configureEach {
        // 排除掉 logback 和 log4j
        exclude("ch.qos.logback", "logback-classic")
        exclude("org.apache.logging.log4j", "log4j-to-slf4j")
    }
}

repositories {
    // 使用阿里云的 Maven 仓库以提高国内网络环境下的依赖解析速度
    maven { url = uri("https://maven.aliyun.com/repository/public") }
    maven { url = uri("https://maven.aliyun.com/repository/google") }
    // 使用官方的 Gradle 插件仓库
    gradlePluginPortal()
    // 使用官方的 Maven Central 仓库
    mavenCentral()
}

dependencies {
    //springboot
    implementation("org.springframework.boot:spring-boot-starter-data-redis")
    implementation("org.springframework.boot:spring-boot-starter-web")
    testImplementation("org.springframework.boot:spring-boot-starter-test")
    implementation("org.springframework.boot:spring-boot-starter-aop")
    implementation("org.springframework.boot:spring-boot-starter-log4j2")
    implementation("org.springframework.boot:spring-boot-starter-mail")
    //validation
    implementation("jakarta.validation:jakarta.validation-api:3.1.0")
    //test
    testRuntimeOnly("org.junit.platform:junit-platform-launcher:1.11.4")
    //testImplementation("org.junit.jupiter:junit-jupiter-api:5.11.4")
    //lombok
    compileOnly("org.projectlombok:lombok")
    annotationProcessor("org.projectlombok:lombok")
    //db&mybatis-plus&hikari-cp
    runtimeOnly("org.postgresql:postgresql")
    implementation("com.baomidou:mybatis-plus-spring-boot3-starter:3.5.9")
    implementation("com.baomidou:mybatis-plus-jsqlparser:3.5.9")
    implementation("hikari-cp:hikari-cp:3.2.0")
    //hutool
    implementation("org.dromara.hutool:hutool-all:6.0.0-M19")
    //knife4j
    implementation("com.github.xiaoymin:knife4j-openapi3-jakarta-spring-boot-starter:4.4.0")
    //额外需要的依赖
    //qq-bot-shiro
    implementation("com.mikuac:shiro:2.3.5")
}

tasks.withType<Test> {
    useJUnitPlatform()
}
