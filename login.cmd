@ECHO OFF

REM Show my version thingy
VER

REM "(me@mymachine):directory> "
PROMPT $C%COMPUTERNAME%@%USERNAME%$F:$P$G 

rem IF NOT DEFINED USING_WIN_EV set VS90COMNTOOLS = C:\Program Files\Microsoft Visual Studio 9.0\Common7\Tools
rem IF NOT DEFINED USING_WIN_EV set Path = C:\Program Files\Microsoft DirectX SDK (August 2007)\Utilities\Bin\x86;C:\bin;%SystemRoot%\system32;%SystemRoot%;%SystemRoot%\System32\Wbem;C:\Program Files\CVSNT;C:\Program Files\ATI Technologies\ATI Control Panel;C:\emacs-22.1\bin;C:\Program Files\doxygen\bin;C:\Program Files\Graphviz2.16\bin;C:\cygwin\bin;C:\cygwin\usr\bin;C:\cygwin\usr\local\bin

DOSKEY /INSERT

DOSKEY emacs=runemacs.exe $*
DOSKEY emacsc=emacsclientw.exe -c -n $*
DOSKEY emacsd=runemacs.exe --force-high-dpi --daemon $*
rem This is a bunch of nonsense now.  The background color is specifically for
rem Emacsen prior to v23, which do not honor the Windows registry "Xdefaults".
rem DOSKEY emacs=runemacs.exe --background-color #D8D0C8 $*
rem DOSKEY emacssrv=runemacs.exe --background-color #D8D0C8 -f server-start $*
rem DOSKEY vcenv="%VS90COMNTOOLS%\vsvars32.bat"
