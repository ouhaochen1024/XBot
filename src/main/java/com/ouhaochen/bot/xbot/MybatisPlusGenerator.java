package com.ouhaochen.bot.xbot;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.generator.AutoGenerator;
import com.baomidou.mybatisplus.generator.config.*;
import com.baomidou.mybatisplus.generator.config.converts.MySqlTypeConvert;
import com.baomidou.mybatisplus.generator.config.rules.DateType;
import com.baomidou.mybatisplus.generator.config.rules.NamingStrategy;
import org.dromara.hutool.core.array.ArrayUtil;
import org.springframework.beans.factory.annotation.Value;

import java.util.Collections;

/**
 * 代码生成工具类
 */
public class MybatisPlusGenerator {

    /**
     * 数据库链接地址
     */
    @Value("${spring.datasource.url}")
    private static String URL;

    /**
     * 用户名
     */
    @Value("${spring.datasource.username}")
    private static String USERNAME;

    /**
     * 密码
     */
    @Value("${spring.datasource.password}")
    private static String PASSWORD;

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

        String projectPath = System.getProperty("user.dir") + "/db";

        //全局配置
        GlobalConfig config = new GlobalConfig.Builder()
                .author(AUTHOR)                                      //作者
                .outputDir(projectPath + "/src/main/java")          //生成文件输出路径(写到java目录)
                .enableSwagger()                                  //开启swagger
                .commentDate("yyyy-MM-dd HH:mm:ss")                 //注释日期格式
                .dateType(DateType.TIME_PACK)                       //设置时间对应类型
                .disableOpenDir()                                   //生成后不要打开目录
                .build();

        //包名配置
        PackageConfig packageConfig = new PackageConfig.Builder()
                .parent("com.ouhaochen.bot.xbot")                      //父包名
                .moduleName("db")           //模块名
                .controller("controller")       //controller包名
                .service("service")           //service接口包名
                .serviceImpl("service.impl")         //service实现类包名
                .entity("entity")               //实体类包名
                .mapper("mapper")               //mapper包名
                .xml("xml")                  //mapper对应的xml包名
                .pathInfo(Collections.singletonMap(OutputFile.xml, projectPath + "/src/main/resources/mapper"))                  //mapper对应的xml路径
                .build();

        //策略配置
        StrategyConfig strategyConfig = new StrategyConfig.Builder()
                .addTablePrefix(TABLE_PREFIX)                       //需要抵消的表前缀
                .addInclude(TABLES)                                 //设置生成需要映射的表名
                //.enableCapitalMode()                              //策略开启⼤写命名

                .serviceBuilder()                                   //构建Service层
//                .enableFileOverride()                               //覆盖文件(需谨慎使用)
                .formatServiceFileName("%sService")
                .formatServiceImplFileName("%ssServiceImpl")             //业务层接口实现类命名
//                .superServiceImplClass(BaseService.class)         //service公共父类

                .entityBuilder()                                    //构建实体类
                .enableFileOverride()                               //覆盖文件(需谨慎使用)
                .formatFileName("%sEntity")
//                .superClass(BaseEntity.class)                       //entity公共父类
//                .addSuperEntityColumns("id","updateTime","createTime") // 公共字段
                .columnNaming(NamingStrategy.underline_to_camel)    //字段名驼峰命名
                .naming(NamingStrategy.underline_to_camel)          //表名驼峰命名
                .enableLombok()                                     //添加lombock的getter、setter注解
                //.enableChainModel()                               //启动链式写法@Accessors(chain = true)
                //.enableColumnConstant()                           //启动属性转常量功能@FieldNameConstants
                //.logicDeleteColumnName("deleted")                 //逻辑删除字段，标记@TableLogic
                .enableTableFieldAnnotation()                       //启动字段注解
//                .addTableFills(new Column("create_time", FieldFill.INSERT), new Column("update_time", FieldFill.INSERT_UPDATE))   //属性值填充策略

                .controllerBuilder()                                //构建Controller类
                .enableFileOverride()                               //覆盖文件(需谨慎使用)
                .enableHyphenStyle()                                //映射路径使用连字符格式，而不是驼峰
                .formatFileName("")                                 //Controller类命名 不生成
//                .superClass(BaseController.class)                   //Controller 类继承 BaseController
                .enableRestStyle()                                  //标记@RestController注解

                .mapperBuilder()                                    //构建mapper接口类
                .enableFileOverride()                               //覆盖文件(需谨慎使用)
                .enableBaseResultMap()                              //生成基本的resultMap
                .formatMapperFileName("%sMapper")                   //Mapper接口类明名
                .superClass(BaseMapper.class)                       //Mapper接口类集成 BaseMapper
                //.enableMapperAnnotation()                         //标记@Mapper注解
                .formatXmlFileName("%sMapper")                      //Mapper.xml命名
                .enableBaseColumnList()                             //生成基本的SQL片段

                .build();

        //数据源配置
        DataSourceConfig.Builder dataSourceConfigBuilder = new DataSourceConfig.Builder(URL, USERNAME, PASSWORD);
        //数据库类型转换器
        dataSourceConfigBuilder.typeConvert(new MySqlTypeConvert() {
            /*@Override
            public IColumnType processTypeConvert(GlobalConfig globalConfig, String tableField) {
                if (tableField.toLowerCase().contains("date") || tableField.toLowerCase().contains("timestamp") || tableField.toLowerCase().contains("datetime")) {
                    return DbColumnType.STRING;
                }
                return super.processTypeConvert(globalConfig, tableField);
            }*/
        });

        //创建代码生成器对象，加载配置
        AutoGenerator autoGenerator = new AutoGenerator(dataSourceConfigBuilder.build());
        autoGenerator.global(config);
        autoGenerator.packageInfo(packageConfig);
        autoGenerator.strategy(strategyConfig);

        //执行
        autoGenerator.execute();
        System.out.println("========================================  相关代码生成完毕  =====================================");
    }
}
