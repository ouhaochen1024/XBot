# XBot

基于 [Shiro](https://github.com/MisakaTAT/Shiro) 框架的QQ机器人服务端  查阅 [Shiro文档](https://misakatat.github.io/shiro-docs/advanced.html)

<p style="text-align: center;">
    <a href="https://github.com/MisakaTAT/Shiro"><img alt="Shiro" src="https://img.shields.io/badge/Shiro-2.3.5-yellow.svg" /></a>
    <a href="https://openjdk.org/projects/jdk/"><img alt="JDK" src="https://img.shields.io/badge/JDK-17+-red.svg" /></a>
    <a href="https://spring.io/projects/spring-boot"> <img alt="SpringBoot" src="https://img.shields.io/badge/SpringBoot-3.0+-brightgreen.svg" /></a>
    <a href="https://www.postgresql.org/"><img alt="PostgreSQL" src="https://img.shields.io/badge/PostgreSQL-15+-blue.svg" /></a>
    <a href="https://gradle.org/"><img alt="PostgreSQL" src="https://img.shields.io/badge/Gradle-8.0+-22afc8" /></a>
    <a href="https://redis.io/"><img alt="Redis" src="https://img.shields.io/badge/Redis-7.0+-ff4438" /></a>
</p>  

机器人客户端推荐使用 [NapCat](https://github.com/NapNeko/NapCatQQ)

已经实现的功能（插件）：

BasePlugin：

1.插件全局状态管理（开关除BasePlugin以外的插件）

2.插件权限管理（是否为机器人的管理者、QQ群是否有权限使用机器人）

目前在开发的功能（插件）：

GroupManagePlugin：

QQ群管理

后续预计新增的功能（插件）：

天气查询

签到

互动小游戏

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
