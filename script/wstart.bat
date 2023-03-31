@echo off

::rd /s /q ..\log

:: 查找本机ip
ipconfig ^ | findstr "IPv4" >ipadd.txt
for /f "tokens=2 delims=:" %%i in (ipadd.txt) do set ipstr=%%i
for /f "tokens=1 delims= " %%i in ('echo %ipstr%') do set ip=%%i
del ipadd.txt
::
set cookie=test
set nodeName=test@%ip%
cd ../
:: echo %cd%
start werl -pa _build/default/lib/c_lib/ebin +e 2000000 -config config/sys -hidden -setcookie %cookie% -name %nodeName% -statue test -env ERL_CRASH_DUMP log/a_erl_crash.dump -s c_sys_lib  -kernel inet_dist_listen_min 9000 -kernel inet_dist_listen_max 9500

exit