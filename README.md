# XBot

基于 [Shiro](https://github.com/MisakaTAT/Shiro) 框架的QQ机器人服务端  查阅 [Shiro文档](https://misakatat.github.io/shiro-docs/advanced.html)

<p style="text-align: center;">
    <a href="https://github.com/MisakaTAT/Shiro"><img alt="Shiro" src="https://img.shields.io/badge/Shiro-2.3.5-yellow.svg" /></a>
    <a href="https://openjdk.org/projects/jdk/"><img alt="JDK" src="https://img.shields.io/badge/JDK-17+-red.svg" /></a>
    <a href="https://spring.io/projects/spring-boot"> <img alt="SpringBoot" src="https://img.shields.io/badge/SpringBoot-3.0.0+-brightgreen.svg" /></a>
    <a href="https://www.postgresql.org/"><img alt="PostgreSQL" src="https://img.shields.io/badge/PostgreSQL-15+-blue.svg" /></a>
    <a href="https://gradle.org/"><img alt="PostgreSQL" src="https://img.shields.io/badge/Gradle-8.0+-22afc8" /></a>
    <a href="https://redis.io/"><img alt="Redis" src="https://img.shields.io/badge/Redis-7.0+-ff4438" /></a>
</p>  

机器人客户端推荐使用 [NapCat](https://github.com/NapNeko/NapCatQQ)

已经实现的功能（插件）：

BasePlugin：

1.插件状态管理（开关除BasePlugin以外的插件）

2.插件权限管理（是否为机器人的管理者、QQ群是否有权限使用机器人）

目前在开发的插件：

GroupManagePlugin（QQ群管理插件）

后续预计新增的插件：

天气查询

签到

互动小游戏
## 示例插件

### 注解调用

> 编写 `application.yaml` 配置文件
> 或参考 [进阶配置文件](https://misakatat.github.io/shiro-docs/advanced.html#进阶配置文件)

```yaml
server:
  port: 5000
```

```java

@Shiro
@Component
public class ExamplePlugin {
    // 更多用法详见 @MessageHandlerFilter 注解源码

    // 当机器人收到的私聊消息消息符合 cmd 值 "hi" 时，这个方法会被调用。
    @PrivateMessageHandler
    @MessageHandlerFilter(cmd = "hi")
    public void fun1(Bot bot, PrivateMessageEvent event, Matcher matcher) {
        // 构建消息
        String sendMsg = MsgUtils.builder().face(66).text("Hello, this is shiro demo.").build();
        // 发送私聊消息
        bot.sendPrivateMsg(event.getUserId(), sendMsg, false);
    }

    // 如果 at 参数设定为 AtEnum.NEED 则只有 at 了机器人的消息会被响应
    @GroupMessageHandler
    @MessageHandlerFilter(at = AtEnum.NEED)
    public void fun2(GroupMessageEvent event) {
        // 以注解方式调用可以根据自己的需要来为方法设定参数
        // 例如群组消息可以传递 GroupMessageEvent, Bot, Matcher 多余的参数会被设定为 null
        System.out.println(event.getMessage());
    }

    // 同时监听群组及私聊消息 并根据消息类型（私聊，群聊）回复
    @AnyMessageHandler
    @MessageHandlerFilter(cmd = "say hello")
    public void fun3(Bot bot, AnyMessageEvent event) {
        bot.sendMsg(event, "hello", false);
    }
}
```

### 重写父类方法

- 注解方式编写的插件无需在插件列表 `plugin-list`定义
- 服务端配置文件 `resources/application.yaml` 追加如下内容
- 插件列表为顺序执行，如果前一个插件返回了 `MESSAGE_BLOCK` 将不会执行后续插件

> 编写 `application.yaml` 配置文件
> 或参考 [进阶配置文件](https://misakatat.github.io/shiro-docs/advanced.html#进阶配置文件)

```yaml
server:
  port: 5000
shiro:
  plugin-list:
    - com.example.bot.plugins.ExamplePlugin
```

```java

@Component
public class ExamplePlugin extends BotPlugin {

    @Override
    public int onPrivateMessage(Bot bot, PrivateMessageEvent event) {
        if ("hi".equals(event.getMessage())) {
            // 构建消息
            String sendMsg = MsgUtils.builder()
                    .face(66)
                    .text("hello, this is shiro example plugin.")
                    .build();
            // 发送私聊消息
            bot.sendPrivateMsg(event.getUserId(), sendMsg, false);
        }
        // 返回 MESSAGE_IGNORE 执行 plugin-list 下一个插件，返回 MESSAGE_BLOCK 则不执行下一个插件
        return MESSAGE_IGNORE;
    }

    @Override
    public int onGroupMessage(Bot bot, GroupMessageEvent event) {
        if ("hi".equals(event.getMessage())) {
            // 构建消息
            String sendMsg = MsgUtils.builder()
                    .at(event.getUserId())
                    .face(66)
                    .text("hello, this is shiro example plugin.")
                    .build();
            // 发送群消息
            bot.sendGroupMsg(event.getGroupId(), sendMsg, false);
        }
        // 返回 MESSAGE_IGNORE 执行 plugin-list 下一个插件，返回 MESSAGE_BLOCK 则不执行下一个插件
        return MESSAGE_IGNORE;
    }

}
```
# Client

[Shiro](https://github.com/MisakaTAT/Shiro) 以 [OneBot-v11](https://github.com/howmanybots/onebot/tree/master/v11/specs)标准协议进行开发，兼容所有支持反向WebSocket的OneBot协议客户端

| 项目                                                        | 描述                                             | 备注 |
| ----------------------------------------------------------- | ------------------------------------------------ | ---- |
| [NapCat](https://github.com/NapNeko/NapCatQQ)               | NapCatQQ 是现代化的基于 NTQQ 的 Bot 协议端实现   | 推荐 |
| [LLOneBot](https://github.com/LLOneBot/LLOneBot)            | 使你的 NTQQ 支持 OneBot11 协议进行 QQ 机器人开发 |      |
| [Lagrange.Core](https://github.com/KonataDev/Lagrange.Core) | NTQQ 的协议实现                                  |      |
| [OpenShamrock](https://github.com/whitechi73/OpenShamrock)  | 基于 Xposed 实现 OneBot 标准的机器人框架         |      |

# License

This product is licensed under the GNU General Public License version 3. The license is as published by the Free
Software Foundation published at https://www.gnu.org/licenses/gpl-3.0.html.

Alternatively, this product is licensed under the GNU Lesser General Public License version 3 for non-commercial use.
The license is as published by the Free Software Foundation published at https://www.gnu.org/licenses/lgpl-3.0.html.

Feel free to contact us if you have any questions about licensing or want to use the library in a commercial closed
source product.
