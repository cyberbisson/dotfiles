@ECHO OFF

REM Show my version thingy
VER

REM "(me@mymachine):directory> "
PROMPT $C%COMPUTERNAME%@%USERNAME%$F:$P$G 

DOSKEY /INSERT
DOSKEY emacs=runemacs.exe --background-color #D8D0C8 $*
