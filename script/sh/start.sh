#!/bin/sh

source ./script/sh/work.cfg
erl -hidden -detached -pa $ERL_PATH +P 200000 +e 2000000 -config $CONFIG -setcookie $ERL_COOKIE -name $NODE_NAME -statue online -env ERL_MAX_PORTS 200000 -env ERL_CRASH_DUMP log/a_erl_crash.dump -s c_sys_lib -kernel inet_dist_listen_min 9000 -kernel inet_dist_listen_max 9500
