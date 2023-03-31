# 简介

用于游戏服务器数据存储,不同数据类型上锁,ets作内缓存,mysql持久化。

# 环境

otp23以上,mysql5.5以上

# 启动
application:start(c_log).
application:start(c_db).
两个application相关依赖,c_db中的log以c_log实现。
也可以用script/*启动脚本启动,比如wstart.bat，start.bat

# 案例
test_db.erl测试案例

# 源数据
config/db/main目录下,有源数据用于生成持久化数据代码及结构，生成结果位于apps/c_db/out目录。
apps/c_db/out目录下的代码是生成，无需改动。