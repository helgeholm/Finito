@echo off
ghc --make Data.hs Parser.hs TestRun.hs -o testrun.exe && goto yay
goto boo
:yay
echo Parsing example01.finito ...
echo.
testrun.exe < example01.finito && goto yayer
goto boo
:yayer
echo.
echo :-D
goto end
:boo
echo :-(
:end