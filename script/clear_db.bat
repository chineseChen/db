@echo off
cd ../
set nodeName=g01@192.168.2.16
erl -pa _build/default/lib/c_lib/ebin  -config config/sys -noshell -hidden -sname dbFile -env ERL_CRASH_DUMP log/a_erl_crash.dump -s c_sys_lib clear_db %nodeName%
pause