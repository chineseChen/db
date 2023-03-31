#!/bin/sh

source ./script/sh/work.cfg
erl -pa _build/default/lib/c_lib/ebin  -config config/sys -noshell -hidden -sname dbFile -env ERL_CRASH_DUMP log/a_erl_crash.dump -s c_sys_lib clear_db $NODE_NAME
