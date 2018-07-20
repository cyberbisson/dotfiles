#!/bin/sh
# .xsession
###############################################################################
# X Windows Session Start-up:
#
# Most X Windows display managers will read this file (or can be instructed to
# do so) when an X console log-in succeed.  This file then explicitly kicks off
# the window manager for my session.  Typically, the file permissions must be
# set to "rwx" for the user creating the session.
###############################################################################
# Matt Bisson

log_file="${HOME}/tmp/Xsession.log"

echo '--------Session started: '`date`'--------' >> ${log_file}

# Not the most flexible way to do this, but it will work for now.  Define
# "ignore" variables to skip WMs that you don't want to use.
ignore_compiz=1
#ignore_fvwm=1
ignore_openwin=1

# We're going to look for a suitable location under which we can assume to find
# all X11 applications.
possible_x_dirs="/usr/openwin/bin /usr/local/bin /usr/X11/bin /usr/bin"

# Fall back to /usr/bin if we can't find anything better below...
using_x_dir="/usr/bin"
for dir in ${possible_x_dirs} ; do
    if [ -x "${dir}/twm" ] ; then
        using_x_dir="${dir}"
        break
    fi
done

# TODO: This logic can be factored out a bit to conditionally run common
# services (read: URxvtd and Xscreensaver), and have a single "exec" line at the
# end of this conditional code that runs the desired DM.

# Try a number of window managers until finally falling back to twm.
if [ ! "${ignore_openwin}" ] && [ -x "${using_x_dir}/olwm" ] ; then
    exec "${using_x_dir}/olwm" >> ${log_file} 2>&1
elif [ ! "${ignore_compiz}" ] && [ -x "${using_x_dir}/beryl-xgl" ] ; then
#   WINDOW_MANAGER="beryl-manager" exec gnome-session >> ${log_file} 2>&1
    "${using_x_dir}/beryl-xgl" --replace  >> ${log_file} 2>&1 &
    exec "${using_x_dir}/beryl-manager" >> ${log_file} 2>&1
elif [ ! "${ignore_fvwm}" ] && [ -x "${using_x_dir}/fvwm" ] ; then
    # Just using URxvt on FVWM for now -- I guess TWM should use "plain" XTerm,
    # or else, what's the point?
    if [ -x "${using_x_dir}/urxvtd" ] ; then
        "${using_x_dir}/urxvtd" -o -f >> ${log_file} 2>&1
    fi
    if [ -x "${using_x_dir}/xscreensaver" ] ; then
        "${using_x_dir}/xscreensaver" -nosplash >> ${log_file} 2>&1 &
    fi
    exec "${using_x_dir}/fvwm" >> ${log_file} 2>&1
elif [ -x "${using_x_dir}/twm" ] ; then
    twm_wallpaper="${HOME}/doc/twm.wallpaper.jpg"

    # Work around odd TWM bug...
    LC_CTYPE=en_US
    export LC_CTYPE

    # Set a nice image to the background if possible.
    if [ -f ${twm_wallpaper} ] ; then
        if [ -x "${using_x_dir}/xview" ] ; then
            "${using_x_dir}/xview" -onroot ${twm_wallpaper} >> ${log_file} 2>&1
        elif [ -x "${using_x_dir}/xli" ] ; then
            "${using_x_dir}/xli" -onroot ${twm_wallpaper} >> ${log_file} 2>&1
        else
            echo "Not able to find a way to set twm wallpaper." >> ${log_file}
        fi
    fi

    # Start the screen saver daemon if it exists on the system.
    if [ -x "${using_x_dir}/xscreensaver" ] ; then
        "${using_x_dir}/xscreensaver" -nosplash >> ${log_file} 2>&1 &
    fi

    "${using_x_dir}/xclock" -geometry 100x100-0+0  >> ${log_file} 2>&1 &
    exec "${using_x_dir}/twm" -f "${HOME}/.twmrc" >> ${log_file} 2>&1
else
    echo "FATAL: Could not determine suitable window manager for your session." \
         >> ${log_file}
fi
