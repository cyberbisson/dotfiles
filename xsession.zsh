#!/bin/zsh
# .xsession.zsh
###############################################################################
# X Windows Session Start-up (assuming ZSH):
#
# Most X Windows display managers will read this file (or can be instructed to
# do so) when an X console log-in succeed.  This file then explicitly kicks off
# the window manager for my session.  Typically, the file permissions must be
# set to "rwx" for the user creating the session.
###############################################################################
# Matt Bisson

# Change this to something other than /dev/null (obviously) if you want to see
# what's going on post-mortem.
#log_file="${HOME}/tmp/Xsession.log"
log_file="/dev/null"
if [ -w '/dev/console' ] ; then
    console_log='/dev/console'
else
    console_log='/dev/null'
fi

echo '--------Session started: '`date`'--------' >>${log_file} >${console_log}
if [ console_log != '/dev/null' ] ; then
    chmod 600 ${log_file}
fi

# Not the most flexible way to do this, but it will work for now.  Define
# "ignore" variables to skip WMs that you don't want to use.
ignore_cde=1
ignore_compiz=1
#ignore_fvwm=1
ignore_gnome=1
ignore_kde=1
ignore_openwin=1
ignore_xfce4=1

# NOTE: If possible -- root access is required -- go into (something like)
# /usr/lib64/X11/xdm/Xsession and remove the "--login" flag from the #!/bin/sh
# invocation, as it likely pertains to bash, ksh, or even Bourne.  This is
# redundant, and possibly harmful, as we're about to load up the ZSH settings.
source "${HOME}/.zprofile" >>${log_file} >${console_log} 2>&1

# TODO: This logic can be factored out a bit to conditionally run common
# services (read: URxvtd and Xscreensaver), and have a single "exec" line at the
# end of this conditional code that runs the desired DM.

# Try a number of window managers until finally falling back to twm.
if [ ! "${ignore_openwin}" ] && [ `command -v olwm` ] ; then
    exec olwm >>${log_file} >${console_log} 2>&1
elif [ ! "${ignore_cde}" ] && [ -x '/usr/dt/bin/Xsession' ] ; then
    exec '/usr/dt/bin/Xsession' >>${log_file} >${console_log} 2>&1
elif [ ! "${ignore_compiz}" ] && [ `command -v beryl-xgl` ] ; then
#   WINDOW_MANAGER="beryl-manager" exec gnome-session >>${log_file} 2>&1
    beryl-xgl --replace >>${log_file} >${console_log} 2>&1 &
    exec beryl-manager >>${log_file} >${console_log} 2>&1
elif [ ! "${ignore_kde}" ] && [ `command -v startkde` ] ; then
    exec startkde >>${log_file} >${console_log} 2>&1
elif [ ! "${ignore_gnome}" ] && [ `command -v gnome-session` ] ; then
    # TODO: UNTESTED...
    exec gnome-session >>${log_file} >${console_log} 2>&1
elif [ ! "${ignore_xfce4}" ] && [ `command -v xfce4-session` ] ; then
    exec xfce4-session >>${log_file} >${console_log} 2>&1
elif [ ! "${ignore_fvwm}" ] && [ `command -v fvwm` ] ; then
    # This will be ignored on non-KDE systems, but when running KDE
    # applications, the preferences are not applied unless this is set.
    export KDE_FULL_SESSION='true'
    # xdg-open does bad things when KDE_FULL_SESSION is true, but nothing else
    # is set.
    export XDG_CURRENT_DESKTOP='X-Generic'

    if [ `command -v xrdb` ] && [ -f "${HOME}/.Xresources" ] ; then
        xrdb -merge "${HOME}/.Xresources" >>${log_file} >${console_log} 2>&1
        if [ -f "${HOME}/.Xresources.generated" ] ; then
            xrdb -merge "${HOME}/.Xresources.generated" \
                 >>${log_file} >${console_log} 2>&1
        fi
    fi

    # Just using URxvt on FVWM for now -- I guess TWM should use "plain" XTerm,
    # or else, what's the point?
    if [ `command -v urxvtd` ] ; then
        urxvtd -o -f >>${log_file} >${console_log} 2>&1
    fi
    if [ `command -v xscreensaver` ] ; then
        xscreensaver -nosplash >>${log_file} >${console_log} 2>&1 &
    fi
    exec fvwm >>${log_file} >${console_log} 2>&1
elif [ `command -v twm` ] ; then
    twm_wallpaper="${HOME}/doc/twm.wallpaper.jpg"

    # Work around odd TWM bug...
    LC_CTYPE=en_US
    export LC_CTYPE

    # Set a nice image to the background if possible.
    if [ -f ${twm_wallpaper} ] ; then
        if [ `command -v xview` ] ; then
            xview -onroot ${twm_wallpaper} >>${log_file} >${console_log}2>&1
        elif [ `command -v xli` ] ; then
            xli -onroot ${twm_wallpaper} >>${log_file} >${console_log}2>&1
        else
            echo "Not able to find a way to set twm wallpaper." \
                 >>${log_file} >${console_log}
        fi
    fi

    # Start the screen saver daemon if it exists on the system.
    if [ `command -v xscreensaver` ] ; then
        xscreensaver -nosplash >>${log_file} >${console_log} 2>&1 &
    fi

    xclock -geometry 100x100-0+0 >>${log_file} >${console_log} 2>&1 &
    exec twm -f "${HOME}/.twmrc" >>${log_file} >${console_log} 2>&1
else
    echo "FATAL: Could not determine suitable window manager for your session." \
         >>${log_file} >${console_log}
fi
