#!/bin/ksh -x
# .profile
###############################################################################
# Login script:
# This file is read by sh/ksh when you have started a "login" shell. 
# It will do more serious initialization in .kshrc
###############################################################################
# For CCS machines:
# This file loads the central .profile that is stored in /ccs/etc/dotfiles.
# This is done because the Systems Group will occasionally update that 
# file to reflect changes in the network, and you may be better off if
# those changes are made for you, but it is doubtful.
###############################################################################
# Matt Bisson

# Read the CCS's .profile file
if [ -f '/ccs/etc/dotfiles/.profile' ] ; then
    . '/ccs/etc/dotfiles/.profile'
fi

###############################################################################
# If TERM is undefined, or it is not an acceptable type
###############################################################################
if [ ! "${TERM}" ] | [ 'unknown' = ${TERM} ] | [ 'ANSI' = ${TERM} ] | \
   [ 'network' = ${TERM} ] ; then

    echo "Note: Changing terminal type..."
    set   noglob
    eval `tset -s -I -Q "vt100"`
    unset noglob

fi

###############################################################################
# Now read my real initialization file
###############################################################################
# Use simulator stuff
USE_HOLLY=1; export USE_HOLLY

###############################################################################
# Get computer information
###############################################################################
hwclass=`uname -m`
host=`uname -n | awk -F. '{print $1}'`
OSrelease=`uname -r`
OSname=`uname -s`
OSver=`uname -v`
curuser=`whoami`

# Make sysV braindamage look like berzerkeley braindamage
TTY=`tty`; export TTY
if [ $? = 0 ] ; then
    isatty=1
else
    isatty=0
fi

# What type of CPU
if [ -x '/bin/machine' ] | [ -x '/usr/bin/machine' ] | 
   [ -x '/usr/local/bin/machine' ] | [ -x "${HOME}/machine" ] ; then
    CPU=`machine`; export CPU
    CCPU="${CPU}"; export CCPU
fi

###############################################################################
# Perform machine specific initializations
###############################################################################
case ${OSname} in

########################################
## Linux
########################################
Linux)
    if   [ -f '/etc/gentoo-release' ] ; then
        distro="gentoo"
    elif [ -f '/etc/redhat-release' ] ; then
        tmp=`grep -q Enterprise /etc/redhat-release`
        if [ 0 = $? ] ; then
            distro="rhel"
        else
            distro="rhat"
        fi
        unset tmp
    elif [ -f '/etc/fedora-release' ] ; then
        distro="fedora"
    elif [ -f '/etc/lsb-release' ] ; then
        distro="ubuntu"
    elif [ -f '/etc/redhat_version' ] ; then
        distro="rhat"
    elif [ -f '/etc/debian_release' ] | [ -f '/etc/debian_version' ] ; then
        distro="deb"
    elif [ -f '/etc/slackware-release' ] | [ -f '/etc/slackware-version' ] ; then
        distro="slack"
    elif [ -f '/etc/mandrake-release' ] ; then
        distro="mndrk"
    elif [ -f '/etc/SuSE-release' ] ; then
        distro="suse"
    elif [ -f '/etc/yellowdog-release' ] ; then
        distro="yellow"
    elif [ -f '/etc/UnitedLinux-release' ] ; then
        distro="united"
    else
        distro="lnux"
    fi

    machtype="${distro}-${hwclass}"
    unset distro

    machdirs=
    machman=
    XENVIRONMENT="${HOME}/.Xdefaults"; export XENVIRONMENT
    ;;

########################################
## SUN specific processing
########################################
SunOS)
    OPENWINHOME="/usr/openwin"; export OPENWINHOME

    shortrel=`echo $OSrelease | awk -F. '{print $1}'`
    machtype="sun${shortrel}"

    if [ 5 <= ${shortrel} ] ; then
        machdirs="/usr/ccs/bin /opt/SUNWspro/bin ${OPENWINHOME}/bin"
        LD_LIBRARY_PATH="/opt/SUNWspro/lib:/usr/ccs/lib:${OPENWINHOME}/lib:/usr/lib"; export LD_LIBRARY_PATH

        # Look for the Sun Java packages
        if [ -d /opt/java122 ] ; then
            machdirs="$machdirs /opt/java122/bin"
        fi
        if [ -d /opt/JDK120 ] ; then
            machdirs="$machdirs /opt/JDK120/bin"
        fi
    else
        machdirs=
    fi

    machman="/opt/SUNWspro/man ${OPENWINHOME}/man"
    XTERM="${OPENWINHOME}/bin/xterm"; export XTERM

    unset shortrel
    ;;

########################################
## HP-UX specific processing
########################################
HP-UX)
    shortrel=`echo ${OSrelease} | awk -F. '{print $2}'`
    if [ `echo ${hwclass} | awk -F/ '{print " "$2}' | grep " 8"` ] ; then
        tempCPU=800
    else
        tempCPU=pa
    fi

    machtype="hp${shortrel}_${tempCPU}"
    unset shortrel
    unset tempCPU

    machdirs="/usr/contrib/bin"
    machman="/usr/contrib"
    ;;

########################################
## SGI specific processing
########################################
IRIX | IRIX64)

    shortrel=`echo $OSrelease | awk -F. '{print $1}'`
    machtype="sgi${shortrel}"
    unset shortrel

    machdirs="/usr/bsd"
    machman=
    ;;

########################################
## IBM AIX
########################################
AIX)
    machtype="aix${OSver}_power"

    if [ -d /usr/lpp/cmvc/bin ] ; then
        cmvcpath="/usr/lpp/cmvc/bin /usr/lpp/cmvc/samples"
    else
        cmvcpath=
    fi

    machdirs="/usr/include/X11/bitmaps /usr/lib/X11 /usr/lib/X11/app-defaults ${cmvcpath}"
    machman=

    unset cmvcpath
    ;;

########################################
## Digital/Compaq UNIX
########################################
OSF1)
    machtype="osf1_axp"
    machdirs="/usr/bin/mh"
    machman=
    ;;

########################################
## SCO UNIXWare
########################################
UnixWare)
    machtype=uw2_x86
    machdirs=
    machman=
    ;;

########################################
## System V type UNIX
########################################
UNIX_SV)
    machtype=att3_x86
    machdirs=
    machman=
    ;;

########################################
## Siemans-Nixdorf UNIX
########################################
SINIX-N)
    shortrel=`echo $OSrelease | awk -F. '{print  $1}'`
    machtype="sni${shortrel}"

    unset shortrel

    machdirs=
    machman=
    ;;

########################################
## CygWin (for Win2k & XP)
########################################
CYGWIN_NT-5.1)
    machtype="cygwin-${hwclass}"


    machdirs=`cmd /c path`
    machdirs=`echo "${machdirs};"                             | \
                  sed 's/^PATH\\=/;/'                         | \
                  sed 's/\\([A-Za-z]\\):/\\/cygdrive\\/\\1/g' | \
                  sed 's/\\\\/\\//g'                          | \
                  sed 's/[^;]*cygwin[^;]*;//g'                | \
                  sed 's/ /\\\\ /g'                           | \
                  sed 's/;/ /g'`
    machman=
    ;;

########################################
## Undeterminable UNIX type
########################################
*)
    machdirs=
    machman=
    ;;
esac

##############################################################################
# Set up my 'simulator's environment
##############################################################################
if [ ${USE_HOLLY} ] ; then
    # Hollywood tools crap
    #HOLLY_ARCH="arm-linux"
    HOLLY_ARCH="i386-linux"
    HOLLY_DEBREL="debug"
    HOLLY_RESULT_ROOT="${HOME}/tmp/toilet/${HOLLY_ARCH}"
    HOLLY_SYSTEM_ROOT="/opt/holly/sysroot/${HOLLY_ARCH}"
    HOLLY_TOOL_ROOT="/opt/holly/toolroot"
    export HOLLY_ARCH HOLLY_DEBREL HOLLY_RESULT_ROOT HOLLY_SYSTEM_ROOT
    export HOLLY_TOOL_ROOT

    # Perforce crap
    P4USER='MattBisson'
    P4PASSWD='p4sucks'
    P4EDITOR='/usr/bin/emacs'
#   P4PORT='achilles:1667'
    P4CLIENT='mbisson'
    P4PORT='grace.palmone.com:1667'
    P4CLIENT='mbisson-isis'
    export P4USER P4PASSWD P4EDITOR P4PORT P4CLIENT

    # Hollywood source crap
    HOLLY_DEV_BRANCH="1.0/Dev"
    HOLLY_DIR="${HOME}/ws/mbisson-holly"
    HOLLY_BASE="${HOLLY_DIR}/Source/Platform/Holly/${HOLLY_DEV_BRANCH}"
    export HOLLY_DEV_BRANCH HOLLY_DIR HOLLY_BASE

    # PBS crap
    if [ -f "${HOME}/PBS/.pbsInit-sh" ] ; then
        . "${HOME}/PBS/.pbsInit-sh"
    fi

    MALLOC_CHECK_=2; export MALLOC_CHECK_
fi

###############################################################################
# Build paths and man pages paths (This is a lot of optimized crap so that I
# can go onto any other computer and not have a 10 line path).
###############################################################################

########################################
## Root doesn't get to use '.'
########################################
userid=`id | sed 's/[^0-9]*\([0-9]*\).*/\1/'`
if [ ! ${userid} = 0 ] ; then
    _path="."
    _manpath="."
fi
unset userid

########################################
## Set up my 'simulator' environment
########################################
if [ "${USE_HOLLY}" ] && [ -d /opt/holly ] ; then
    hollydirs="${HOLLY_TOOL_ROOT}/${HOLLY_ARCH}/local/bin ${HOLLY_TOOL_ROOT}/bin"

    for dir in ${hollydirs} ; do
        if [ -d ${dir} ] ; then
            _path="${_path}:${dir}"
        fi
    done

    unset hollydirs
fi

########################################
## I want these dirs in the path even if they don't exist
########################################
basedirs="~/bin ~/bin/Linux ~/bin/scripts /bin /sbin /usr/bin"
baseman="~/man /usr/man"

for dir in ${basedirs} ; do
    _path="${_path}:${dir}"
done
for dir in ${baseman} ; do
    _manpath="${_manpath}:${dir}"
done

unset basedirs
unset baseman

########################################
## Machine-specific paths gathered above
########################################
for dir in ${machdirs} ; do
    if [ -d ${dir} ] ; then
        _path="${_path}:${dir}"
    fi
done
for dir in ${machman} ; do
    if [ -d ${dir} ] ; then
        _manpath="${_manpath}:${dir}"
    fi
done

unset machdirs
unset machman

########################################
## These are very common dirs
########################################
commondirs="/usr/sbin /usr/ucb /usr/share/bin"
commonman="/usr/share/man /usr/catman"

for dir in ${commondirs} ; do
    if [ -d ${dir} ] ; then
        _path="${_path}:${dir}"
    fi
done
for dir in ${commonman} ; do
    if [ -d ${dir} ] ; then
        _manpath="${_manpath}:${dir}"
    fi
done

unset commondirs
unset commonman

########################################
## Search the /usr/local area
########################################
if [ -d /usr/local ] ; then
    localdirs="/usr/local/bin /usr/local/sbin"
    localman="/usr/local/man"

    for dir in ${localdirs} ; do
        if [ -d ${dir} ] ; then
            _path="${_path}:${dir}"
        fi
    done
    for dir in ${localman} ; do
        if [ -d ${dir} ] ; then
            _manpath="${_manpath}:${dir}"
        fi
    done

    unset localdirs
    unset localman
fi

########################################
## If a root-local dir exists
########################################
if [ -d /local ] ; then
    localdirs="/local/bin /local/gnu/bin /local/apps/mh"
    localman="/local/man /local/apps/X11R5/man /local/gnu/man /local/apps/mh/man"

    for dir in ${localdirs} ; do
        if [ -d ${dir} ] ; then
            _path="${_path}:${dir}"
        fi
    done
    for dir in ${localman} ; do
        if [ -d ${dir} ] ; then
            _manpath="${_manpath}:${dir}"
        fi
    done

    unset localdirs
    unset localman
fi

########################################
## CDE paths
########################################
if [ -d /usr/dt ] ; then
    cdedirs="/usr/dt/bin"
    cdeman="/usr/dt/man"

    for dir in ${cdedirs} ; do
        if [ -d ${dir} ] ; then
            _path="${_path}:${dir}"
        fi
    done
    for dir in ${cdeman} ; do
        if [ -d ${dir} ] ; then
            _manpath="${_manpath}:${dir}"
        fi
    done

    unset cdedirs
    unset cdeman
fi

########################################
## Basic X-windows stuff
########################################
if [ -d /usr/bin/X11 ] ; then
    xdirs="/usr/bin/X11 /usr/bin/X11/demos"

    for dir in ${xdirs} ; do
        if [ -d ${dir} ] ; then
            _path="${_path}:${dir}"
        fi
    done

    unset xdirs
fi

########################################
## Any GNU stuff that's been broken out...
########################################
if [ -d /usr/gnu ] ; then
    gnudirs="/usr/gnu/bin /usr/gnu/bin"
    gnuman="/usr/gnu/man"

    for dir in ${gnudirs} ; do
        if [ -d ${dir} ] ; then
            _path="${_path}:${dir}"
        fi
    done
    for dir in ${gnuman} ; do
        if [ -d ${dir} ] ; then
            _manpath="${_manpath}:${dir}"
        fi
    done

    unset gnudirs
    unset gnuman
fi

########################################
## Oracle environment setup
########################################
if [ -f "${HOME}/.oracle.ksh" ] ; then
    . "${HOME}/.oracle.ksh"

    oradirs="${ORACLE_HOME}/bin ${ORACLE_HOME}/oc4j/bin"

    for dir in ${oradirs} ; do
        if [ -d ${dir} ] ; then
            _path="${_path}:${dir}"
        fi
    done

    # I want to use a specific login script for PL/SQL
    if [ -f ~/sql/login.sql ] ; then
        SQLPATH="${HOME}/sql"
        export SQLPATH
    fi

    unset oradirs
fi

########################################
## Northeastern's CCS keeps arch-specific stuff separate on an NFS share
########################################
if [ -d /arch ] ; then
    ccsdirs="/arch/X11R6/bin /arch/Xapps/bin /arch/adm/bin /arch/beta/bin /arch/com/bin /arch/daemons/bin /arch/gnu/bin /arch/unix/bin"
    ccsman="/arch/unix/man /arch/com/man /arch/unix/packages/j2sdk1_3_0beta/man /arch/gnu/man /arch/Xapps/man /arch/X11R6/man"

    for dir in ${ccsdirs} ; do
        if [ -d ${dir} ] ; then
            _path="${_path}:${dir}"
        fi
    done
    for dir in ${ccsman} ; do
        if [ -d ${dir} ] ; then
            _manpath="${_manpath}:${dir}"
        fi
    done

    unset ccsdirs
    unset ccsman
fi
if [ -d /share ] ; then
    ccsdirs="/share/unix/bin /share/com/bin /share/Xapps/bin"
    ccsman="/share/unix/man /share/com/man /share/Xapps/man /share/Xapps/man"

    for dir in ${ccsdirs} ; do
        if [ -d ${dir} ] ; then
            _path="${_path}:${dir}"
        fi
    done
    for dir in ${ccsman} ; do
        if [ -d ${dir} ] ; then
            _manpath="${_manpath}:${dir}"
        fi
    done

    unset ccsdirs
    unset ccsman
fi
if [ -d /ccs ] ; then
    ccsdirs="/ccs/bin"
    ccsman="/ccs/man"

    for dir in ${ccsdirs} ; do
        if [ -d ${dir} ] ; then
            _path="${_path}:${dir}"
        fi
    done
    for dir in ${ccsman} ; do
        if [ -d ${dir} ] ; then
            _manpath="${_manpath}:${dir}"
        fi
    done

    unset ccsdirs
    unset ccsman
fi

## Is ClearCase on this machine?
if [ -d /usr/atria ] ; then
    ccdirs="/usr/atria/bin"
    ccman="/usr/atria/doc /usr/atria/doc/man"

    for dir in ${ccdirs} ; do
        if [ -d ${dir} ] ; then
            _path="${_path}:${dir}"
        fi
    done
    for dir in ${ccman} ; do
        if [ -d ${dir} ] ; then
            _path="${_path}:${dir}"
        fi
    done

    unset ccdirs
    unset ccman
fi

########################################
## Now we've got a real path!!!
########################################
PATH="${_path}"
MANPATH="${_manpath}"

# Strip any ':' at the beginning of the path
PATH=`echo ${PATH}       | sed 's/^://'`; export PATH
MANPATH=`echo ${MANPATH} | sed 's/^://'`; export MANPATH

unset _path
unset _manpath

###############################################################################
# Set the prompt
###############################################################################
if [ "${LOGNAME}" != "${curuser}" ] ; then
    hostprompt="${curuser}@${host}"
else
    hostprompt="${host}"
fi
if [ ! ${machtype} ] ; then
    machprompt=
else
    machprompt="[${machtype}]:"
fi

PS1="$machprompt$hostprompt# "

if [ "" != "${BASH}" ] ; then
    case ${TERM} in
    cygwin | dtterm | linux | rxvt | xterm | xterm-color | xterm-xfree86)
        PS1="\[\033[01;39m\]${PS1}\[\033[00m\]"
        ;;
    esac
fi

PS2='> '
PS4='+ '

export PS1 PS2 PS4

unset machtype
unset machprompt
unset hostprompt

###############################################################################
# Miscellaneous settings
###############################################################################
# Make user and login name the same thing
if [ ! "${USER}" ] && [ "${LOGNAME}" ] ; then
    USER="${LOGNAME}"; export USER
fi

umask 022

EDITOR="emacs"
VISUAL="emacs"
WINEDITOR="emacs"
export EDITOR VISUAL WINEDITOR

ENSCRIPT="-2 -C -E -G -r -T4 --color=1 --style=mbisson --margins=15:15:15:15 --mark-wrapped-lines=arrow"
export ENSCRIPT

# Get the timezone set... sigh
if [ ! "${TZ}" ] ; then
    TZ="EST5EDT"; export TZ
fi

###############################################################################
# If the login is on an X display but the DISPLAY variable has not 
# been set, ask the user what the DISPLAY should be (e.g., rlogin)
###############################################################################
case ${TERM} in
    dtterm | xterm*)
        if [ ! "${DISPLAY}" ] ; then

            echo "What DISPLAY are you using [default: NONE]? "
            `read response`

            if [ "${response}" != "" ] ; then
                if [ ! "`echo ${response} | grep :`" ] ; then
                    echo "Setting DISPLAY to ${response}"
                    DISPLAY="${response}"; export DISPLAY
                else
                    echo "Setting DISPLAY to ${response}:0.0"
                    DISPLAY="${response}:0.0"; export DISPLAY
                fi
#           else
                # Allow this to be undefined, and we will display things
                # on the terminal window.
            fi
        fi
        ;;
esac

unset response

###############################################################################
# Old STTY settings.  Uncomment for fun and edutainment...
###############################################################################
if [ ${isatty} ] ; then
    STTY_PARAM='intr ^C quit ^\\ kill ^U eof ^D start ^Q stop ^S susp ^Z ixany'
    export STTY_PARAM

    tmp=`uname`

    # ^H isn't right for Linux, it wants ^? instead
#    if [ ${tmp} != 'Linux' ] ; then
#       setenv STTY_PARAM "${STTY_PARAM} erase ^H"
#    fi

    # OS Specific stuff
    case ${tmp} in
        OSF1)
            STTY_PARAM="${STTY_PARAM} status ^T"; export STTY_PARAM
            ;;
        *)
            # I don't have anything to say here...
            ;;
    esac

    unset tmp
    stty ${STTY_PARAM}
fi

###############################################################################
# Unset all unnecessary variables...
###############################################################################
unset curuser
unset dir
unset host
unset hwclass
unset isatty
unset OSname
unset OSrelease
unset OSver

unset LS_COLORS

###############################################################################
# Now read my real initialization file
###############################################################################
if   [ "" != "${KSH_VERSION}" ] ; then
    . "${HOME}/.kshrc"
elif [ "" != "${BASH}" ] ; then
    . "${HOME}/.bashrc"
else
    . "${HOME}/.shrc"
fi
