echo '--------'Session started: `date`'--------' >> /home/mbisson/tmp/Xsession_log
if [ -x /usr/openwin/bin/twm ] ; then
    /bin/echo '--------OPENWIN--------' >> /home/mbisson/tmp/Xsession_log
    /usr/openwin/bin/xclock -geometry 100x100-0+0 &
    exec /usr/openwin/bin/twm -f /home/mbisson/.twmrc >> /home/mbisson/tmp/Xsession_log 2>&1
#   exec /usr/openwin/bin/xterm -l
    exec /usr/openwin/bin/olwm >> /home/mbisson/tmp/barfoff 2>&1 
else
    /usr/bin/X11/xclock -geometry 100x100-0+0 &
    exec /usr/bin/X11/twm -f /home/mbisson/.twmrc >> /home/mbisson/tmp/Xsession_log 2>&1
fi
