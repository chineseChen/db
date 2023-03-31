@echo off
cd ../
erl -pa _build/default/lib/c_lib/ebin -noshell -hidden -sname dbFile -env ERL_CRASH_DUMP log/a_erl_crash.dump -s db_file_bat do %*
pause