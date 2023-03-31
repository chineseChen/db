#!/bin/sh

source ./script/sh/work.cfg
erl -pa $ERL_PATH -noshell -hidden -name stop$RANDOM@127.0.0.1 -eval -eval "rpc:call('$NODE_NAME', c_sys_lib, stop, [])."  -s c q -setcookie $ERL_COOKIE
