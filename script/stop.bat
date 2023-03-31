@echo off

:: 查找本机ip
ipconfig ^ | findstr "IPv4" >ipadd.txt
for /f "tokens=2 delims=:" %%i in (ipadd.txt) do set ipstr=%%i
for /f "tokens=1 delims= " %%i in ('echo %ipstr%') do set ip=%%i
del ipadd.txt
::
set cookie=g01
set nodeName=g01@%ip%
set erlPath=_build/default/lib/c_lib/ebin

cd ../
erl -pa %erlPath% -noshell -hidden -name stop@127.0.0.1 -eval "rpc:call('%nodeName%', c_sys_lib, stop, [])." -s c q -setcookie %cookie%

pause