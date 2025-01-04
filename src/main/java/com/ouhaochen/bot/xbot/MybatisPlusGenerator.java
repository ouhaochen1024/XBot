package com.ouhaochen.bot.xbot;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.generator.FastAutoGenerator;
import com.baomidou.mybatisplus.generator.config.OutputFile;
import com.baomidou.mybatisplus.generator.config.converts.PostgreSqlTypeConvert;
import com.baomidou.mybatisplus.generator.config.rules.NamingStrategy;
import com.baomidou.mybatisplus.generator.engine.FreemarkerTemplateEngine;
import com.baomidou.mybatisplus.generator.fill.Column;
import com.ouhaochen.bot.xbot.db.domain.entity.BaseEntity;
import org.dromara.hutool.core.array.ArrayUtil;

import java.util.Collections;

/**
 * 代码生成工具类
 */
public class MybatisPlusGenerator {

    /**
     * 数据库链接地址
     */
    private static final String URL = "jdbc:postgresql://192.168.31.117:5432/bot?currentSchema=dev&useUnicode=true&characterEncoding=utf8&useSSL=false&serverTimezone=Asia/Shanghai&nullCatalogMeansCurrent=true";

    /**
     * 用户名
     */
    private static final String USERNAME = "ggbond";

    /**
     * 密码
     */
    private static final String PASSWORD = "42454434";

    /**
     * 作者
     */
    private static final String AUTHOR = "ouhaochen";

    /**
     * 表名的前缀,从表生成代码时会去掉前缀
     */
    private static final String TABLE_PREFIX = "t_";

    /**
     * 需要生成的表名，必填
     */
    private static final String[] TABLES = {
            "t_bot_group"
    };

    public static void main(String[] args) {
        if (ArrayUtil.hasBlank(TABLES)) {
            System.out.println("请输入需要生成的表名!");
            return;
        }

        String projectPath = System.getProperty("user.dir");
        FastAutoGenerator.create(URL, USERNAME, PASSWORD)
                .dataSourceConfig(builder ->
                                builder.typeConvert(new PostgreSqlTypeConvert() {
                                    // 自定义数据库表字段类型转换【可选】
//                            @Override
//                            public IColumnType processTypeConvert(GlobalConfig globalConfig, String fieldType) {
//                                if (fieldType.contains("timestamp")) {
//                                    return DbColumnType.LOCAL_DATE_TIME;
//                                }
//                                return super.processTypeConvert(globalConfig, fieldType);
//                            }
                                })
                )
                .globalConfig(builder -> builder
                        .author(AUTHOR)
                        .outputDir(projectPath + "/src/main/java")
                        .commentDate("yyyy-MM-dd")
                        .disableOpenDir()
                )
                .packageConfig(builder -> builder
                        .parent("com.ouhaochen.bot.xbot.db")
                        .entity("domain.entity")
                        .mapper("mapper")
                        .service("service")
                        .serviceImpl("service.impl")
                        .pathInfo(Collections.singletonMap(OutputFile.xml, projectPath + "/src/main/resources/mapper"))
                )
                .strategyConfig(builder -> builder
                        .addTablePrefix(TABLE_PREFIX)
                        .addInclude(TABLES)
                        //实体配置
                        .entityBuilder()
                        .formatFileName("%sEntity")
                        .enableChainModel()
                        .enableTableFieldAnnotation()
                        .enableActiveRecord()
                        .enableFileOverride()
                        .enableRemoveIsPrefix()
                        .enableLombok()
                        //字段名驼峰命名
                        .columnNaming(NamingStrategy.underline_to_camel)
                        //表名驼峰命名
                        .naming(NamingStrategy.underline_to_camel)
                        .addTableFills(new Column("create_time", FieldFill.INSERT), new Column("update_time", FieldFill.INSERT_UPDATE))
                        //entity公共父类
                        .superClass(BaseEntity.class)
                        //公共字段
                        .addSuperEntityColumns("updateTime", "createTime", "createBy", "updateBy", "delFlag")
                        // 逻辑删除字段，标记@TableLogic
                        //.logicDeleteColumnName("del_flag")
                        //mapper
                        .mapperBuilder()
                        .formatMapperFileName("%sMapper")
                        .formatXmlFileName("%sMapper")
                        .enableFileOverride()
                        .enableBaseResultMap()
                        .enableBaseColumnList()
                        //service
                        .serviceBuilder()
                        .formatServiceFileName("%sService")
                        .formatServiceImplFileName("%sServiceImpl")
                        //controller
                        .controllerBuilder()
                        .disable()
                        .build()
                )
                .templateEngine(new FreemarkerTemplateEngine())
                .execute();
        System.out.println("========================================  相关代码生成完毕  =====================================");
    }
}
