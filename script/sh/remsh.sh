#!/bin/sh

source ./script/sh/work.cfg
erl -name remshNode$RANDOM@127.0.0.1 -setcookie $ERL_COOKIE -remsh $NODE_NAME
