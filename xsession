#!/bin/sh

TWM_WALLPAPER=${HOME}/doc/twm.wallpaper.jpg
LOGFILE=${HOME}/tmp/Xsession.log

echo '--------'Session started: `date`'--------' >> ${LOGFILE}

#if [ -x /usr/bin/beryl-xgl ] ; then
#	WINDOW_MANAGER="beryl-manager" exec gnome-session
#	/usr/bin/beryl-xgl --replace &
#	exec /usr/bin/beryl-manager
#elif [ -x /usr/openwin/bin/twm ] ; then
if [ -x /usr/openwin/bin/twm ] ; then
    /bin/echo '--------OPENWIN--------' >> ${LOGFILE}
    /usr/openwin/bin/xclock -geometry 100x100-0+0 &
    exec /usr/openwin/bin/twm -f ${HOME}/.twmrc >> ${LOGFILE} 2>&1
#   exec /usr/openwin/bin/xterm -l
    exec /usr/openwin/bin/olwm >> ${HOME}/tmp/barfoff 2>&1 
elif [ -x /usr/X11/bin/twm ] ; then
    if [ -f ${TWM_WALLPAPER} ] ; then
        /usr/X11/bin/xview -onroot ${TWM_WALLPAPER}
    fi

    /usr/X11/bin/xclock -geometry 100x100-0+0 &
    exec /usr/X11/bin/twm -f ${HOME}/.twmrc >> ${LOGFILE} 2>&1
else
    if [ -f ${TWM_WALLPAPER} ] ; then
        /usr/bin/xview -onroot ${TWM_WALLPAPER}
    fi

    /usr/bin/xclock -geometry 100x100-0+0 &
    exec /usr/bin/twm -f ${HOME}/.twmrc >> ${LOGFILE} 2>&1
fi
