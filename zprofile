#!/bin/zsh -x
# .zprofile
################################################################################
# Z-Shell Profile script:
#
# This is the file that zsh reads first when invoked as a "log-in" shell.  Put
# any common initialization tasks in this file (like setting up the PATH), while
# leaving things specific to "interactive" shell set-up in .zshrc.
################################################################################
# Matt Bisson

if [ ${+SAW_ZPROFILE_SCRIPT} -ne 0 ] && [ ${SAW_ZPROFILE_SCRIPT} -ne 0 ] ; then
    return
fi

export SAW_ZPROFILE_SCRIPT=1

################################################################################
# If TERM is undefined, or it is not an acceptable type
################################################################################

if [ ${+TERM} -eq 0 ] ; then
    echo "Changing terminal type..."
    export TERM='vt100'
else
    case ${TERM} in
        'ANSI'|'network'|'unknown')
            echo "Changing terminal type..."
            export TERM='vt100'
            ;;
    esac
fi

################################################################################
# Perform machine specific initializations
################################################################################

hwclass=`uname -m`
OSrelease=`uname -r`

case `uname -s` in

########################################
## Linux
########################################
'Linux')
    export XENVIRONMENT="${HOME}/.Xdefaults"
    if [ -f '/etc/gentoo-release' ] ; then

        distro='gentoo'

        if [ -f '/usr/bin/kde5' ] ; then
            kdever=5
        elif [ -f '/usr/bin/kde4' ] ; then
            kdever=4
        else
            # Nothing better to do...
            kdever=3.5
        fi

        # TODO: All these hard-coded paths are silly and wrong.
        machdirs=( '/opt/bin' /usr/kde/${kdever}/sbin /usr/kde/${kdever}/bin '/usr/qt/3/bin' '/opt/vmware/workstation/bin' '/opt/ghc/bin' '/usr/games/bin' )
        machman=( '/usr/local/share/man' '/usr/share/man' '/usr/share/binutils-data/i686-pc-linux-gnu/2.17/man' '/usr/share/gcc-data/i686-pc-linux-gnu/4.1.2/man' '/opt/sun-jdk-1.4.2.13/man' '/etc/java-config/system-vm/man' /usr/kde/${kdever}/share/man '/usr/qt/3/doc/man' '/opt/vmware/workstation/man' '/opt/bin' )
        unset kdever

        # Try to read configuration files to determine where GCC is.  Failing
        # this, use the gcc-config command, which is much slower.
        if [ -x '/etc/env.d/gcc' ] ; then
            for file in /etc/env.d/gcc/`uname -m`-*-linux-gnu-* ; do
                tmp_gccpath=`grep '^GCC_PATH=' ${file} | \
                             sed 's/^GCC_PATH=//' | sed 's/"//g'`
                tmp_manpath=`grep '^MANPATH=' ${file} | \
                             sed 's/^MANPATH=//' | sed 's/"//g'`
                machdirs=( ${machdirs} ${tmp_gccpath} )
                machman=( ${machman} ${tmp_manpath} )
                unset tmp_gccpath
                unset tmp_manpath
            done
        elif [ -x '/usr/bin/gcc-config' ] ; then
            gcc_config=`/usr/bin/gcc-config -B`
            machdirs=( ${machdirs} ${gcc_config} )
            machman=( ${machman} "${gcc_config}/man" )
            unset gcc_config
        fi

    elif [ -f '/etc/redhat-release' ] ; then
        grep -q 'Enterprise' '/etc/redhat-release'
        if [ 0 -eq $? ] ; then
            distro='rhel'
        else
            distro='rhat'
        fi
    elif [ -f '/etc/fedora-release' ] ; then
        distro='fedora'
    elif [ -f '/etc/redhat_version' ] ; then
        distro='rhat'
    elif [ -f '/etc/debian_release' ] || [ -f '/etc/debian_version' ] ; then
        distro='deb'
    elif [ -f '/etc/slackware-release' ] ||
         [ -f '/etc/slackware-version' ] ; then
        distro='slack'
    elif [ -f '/etc/mandrake-release' ] ; then
        distro='mndrk'
    elif [ -f '/etc/SuSE-release' ] ; then

        ver=`grep VERSION /etc/SuSE-release | awk '{print $3}' | awk -F. '{print $1}'`

        grep -q 'openSUSE' '/etc/SuSE-release'
        if [ 0 -eq $? ] ; then
            machtype='openSuSE-'${ver}
        else
            machtype='sles'${ver}
        fi

        machdirs=( '/opt/gnome/sbin' '/opt/gnome/bin' '/opt/kde3/sbin' \
                   '/opt/kde3/bin' )
        machman=( '/opt/gnome/share/man' )
        unset ver

    elif [ -f '/etc/yellowdog-release' ] ; then
        distro='yellow'
    elif [ -f '/etc/UnitedLinux-release' ] ; then
        distro='united'
    elif [ -f '/etc/lsb-release' ] ; then
        distro='ubuntu'
    else
        distro='lnux'
    fi

    if [ ${+machtype} -eq 0 ] ; then
        machtype=${distro}-${hwclass}
    fi
    unset distro
    ;;

########################################
## FreeBSD
########################################
'FreeBSD')
    shortrel=`echo $OSrelease | sed 's/\([0-9]*\.[0-9]*\).*/\1/'`
    machtype='bsd'${shortrel}'-'${hwclass}
    unset shortrel
    ;;

########################################
## CygWin (for Win2k & XP)
########################################
CYGWIN_NT-* | CYGWIN_NT-*-WOW64)
    machtype='cygwin-'${hwclass}
    winpath=`cygpath -u -S`

    # Invoke the Windows command interpreter, and call the PATH command.  Then
    # strip off the "PATH=" bit, convert spaces to '%', change "X:" to
    # "/cygdrive/x", and '\' to '/'.  Remove any cygwin stuff in the Windows
    # path, since it will be redundant in our Cygwin environment.  Lastly, throw
    # in ':' where there were ';'.
    windows_path=( `${winpath}/cmd /c path                 | \
                    sed 's/^PATH\=/;/'                     | \
                    sed 's/ /%/g'                          |\
                    sed 's/\([A-Za-z]\):/\/cygdrive\/\1/g' | \
                    sed 's/\\\\/\//g'                      | \
                    sed 's/[^;]*cygwin[^;]*;//g'           | \
                    sed 's/ /\\ /g'                        | \
                    sed 's/;/ /g'` )
    unset winpath
    ;;

########################################
## SUN specific processing
########################################
'SunOS')
    export OPENWINHOME='/usr/openwin'

    shortrel=`echo $OSrelease | awk -F. '{print $1}'`
    machtype='sun'${shortrel}

    if [ ${shortrel} -gt 4 ] ; then
        machdirs=( '/usr/ccs/bin' '/opt/SUNWspro/bin' '${OPENWINHOME}/bin' )
        export LD_LIBRARY_PATH="/opt/SUNWspro/lib:/usr/ccs/lib:${OPENWINHOME}/lib:/usr/lib"

        # Look for the Sun Java packages
        if [ -d '/opt/java122' ] ; then
            machdirs=( ${machdirs} '/opt/java122/bin' )
        fi
        if [ -d '/opt/JDK120' ] ; then
            machdirs=( ${machdirs} '/opt/JDK120/bin' )
        fi
    fi

    machman=( '/opt/SUNWspro/man' ${OPENWINHOME}/man )
    export XTERM="${OPENWINHOME}/bin/xterm"

    unset shortrel
    ;;

########################################
## HP-UX specific processing
########################################
'HP-UX')
    shortrel=`echo ${OSrelease} | awk -F. '{print $2}'`
    if [ `echo ${hwclass} | awk -F/ '{print " "$2}' | grep " 8"` ] ; then
        tempCPU='800'
    else
        tempCPU='pa'
    fi

    machtype='hp'${shortrel}_${tempCPU}
    unset shortrel
    unset tempCPU

    machdirs=( '/usr/contrib/bin' )
    machman=( '/usr/contrib' )
    ;;

########################################
## SGI specific processing
########################################
'IRIX'|'IRIX64')
    shortrel=`echo $OSrelease | awk -F. '{print $1}'`
    machtype='sgi'${shortrel}
    unset shortrel

    machdirs=( '/usr/bsd' )
    ;;

########################################
## IBM AIX
########################################
'AIX')
    machtype='aix'${OSver}'_power'

    if [ -d '/usr/lpp/cmvc/bin' ] ; then
        cmvcpath=( '/usr/lpp/cmvc/bin' '/usr/lpp/cmvc/samples' )
    else
        cmvcpath=( )
    fi

    machdirs=( '/usr/include/X11/bitmaps' '/usr/lib/X11' '/usr/lib/X11/app-defaults' ${cmvcpath} )
    unset cmvcpath
    ;;

########################################
## Digital/Compaq UNIX
########################################
'OSF1')
    machtype='osf1_axp'
    machdirs='/usr/bin/mh'
    ;;

########################################
## SCO UNIXWare
########################################
'UnixWare')
    machtype='uw2_x86'
    ;;

########################################
##  Mac OS X
########################################
'Darwin')
    # What type of CPU
    if [ -x '/bin/machine' ] || [ -x '/usr/bin/machine' ] ||
       [ -x '/usr/local/bin/machine' ] || [ -x "${HOME}/machine" ] ; then
        CPU=`machine`
    fi
    machtype='osx_'${CPU}
    unset CPU
    ;;

########################################
## System V type UNIX
########################################
'UNIX_SV')
    machtype='att3_x86'
    ;;

########################################
## Siemans-Nixdorf UNIX
########################################
'SINIX-N')
    shortrel=`echo $OSrelease | awk -F. '{print  $1}'`
    machtype='sni'${shortrel}
    unset shortrel
    ;;

########################################
## Undeterminable UNIX type
########################################
*)
    ;;

esac

if [ ${+machdirs} -eq 0 ] ; then
    machdirs=(  )
fi
if [ ${+machman} -eq 0 ] ; then
    machman=(  )
fi
unset hwclass
unset OSrelease

################################################################################
# Build paths and man pages paths (This is a lot of optimized crap so that I
# can go onto any other computer and not have a 10 line path).
################################################################################

########################################
## Root doesn't get to use '.'
########################################
userid=`id | sed 's/[^0-9]*\([0-9]*\).*/\1/'`
if [ ${userid} -ne 0 ] ; then
    _path="."
    _manpath="."
fi
unset userid

########################################
## I want these dirs in the path even if they don't exist
########################################
basedirs=( ~/bin ~/bin/Linux ~/bin/scripts )
baseman=( ~/man )

for dir in ${basedirs} ; do
    _path="${_path}:${dir}"
done
for dir in ${baseman} ; do
    _manpath="${_manpath}:${dir}"
done

unset basedirs
unset baseman

########################################
## Search the /usr/local area
########################################
if [ -d '/usr/local' ] ; then
    localdirs=( '/usr/local/bin' '/usr/local/sbin' )
    localman=( '/usr/local/man' )

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
if [ -d '/local' ] ; then
    localdirs=( '/local/bin' '/local/gnu/bin' '/local/apps/mh' )
    localman=( '/local/man' '/local/apps/X11R5/man' '/local/gnu/man' '/local/apps/mh/man' )

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
## VMware-specific areas
########################################
if [ -d '/mts/git' ] ; then
    gitdirs=( '/mts/git/tools/bin' )
    gitman=( )
    for dir in ${gitdirs} ; do
        if [ -d ${dir} ] ; then
            _path="${_path}:${dir}"
        fi
    done
    for dir in ${gitman} ; do
        if [ -d ${dir} ] ; then
            _manpath="${_manpath}:${dir}"
        fi
    done

    unset gitdirs
    unset gitman
fi

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
commondirs=( '/usr/sbin' '/usr/ucb' '/bin' '/sbin' '/usr/bin' '/usr/share/bin' )
commonman=( '/usr/share/man' '/usr/man' '/usr/catman' )

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
## CDE paths
########################################
if [ -d '/usr/dt' ] ; then
    cdedirs=( '/usr/dt/bin' )
    cdeman=( '/usr/dt/man' )

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
if [ -d '/usr/bin/X11' ] ; then
    xdirs=( '/usr/bin/X11' '/usr/bin/X11/demos' )

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
if [ -d '/usr/gnu' ] ; then
    gnudirs=( '/usr/gnu/bin' '/usr/gnu/bin' )
    gnuman=( '/usr/gnu/man' )

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
if [ -f "${HOME}/.oracle.zsh" ] ; then
    source "${HOME}/.oracle.zsh"

    oradirs=( ${ORACLE_HOME}/bin ${ORACLE_HOME}/oc4j/bin )

    for dir in ${oradirs} ; do
        if [ -d ${dir} ] ; then
            _path="${_path}:${dir}"
        fi
    done

    # I want to use a specific login script for PL/SQL
    if [ -f ~/sql/login.sql ] ; then
        export SQLPATH="${HOME}/sql"
    fi

    unset oradirs
fi

########################################
## Northeastern's CCS keeps arch-specific stuff separate on an NFS share
########################################
if [ -d '/arch' ] ; then
    ccsdirs=( '/arch/X11R6/bin' '/arch/Xapps/bin' '/arch/adm/bin' '/arch/beta/bin' '/arch/com/bin' '/arch/daemons/bin' '/arch/gnu/bin' '/arch/unix/bin' )
    ccsman=( '/arch/unix/man' '/arch/com/man' '/arch/unix/packages/j2sdk1_3_0beta/man' '/arch/gnu/man' '/arch/Xapps/man' '/arch/X11R6/man' )

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
if [ -d '/share' ] ; then
    ccsdirs=(' /share/unix/bin' '/share/com/bin' '/share/Xapps/bin' )
    ccsman=( '/share/unix/man' '/share/com/man' '/share/Xapps/man' '/share/Xapps/man' )

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
if [ -d '/ccs' ] ; then
    ccsdirs=( '/ccs/bin' )
    ccsman=( '/ccs/man' )

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

########################################
## Is ClearCase on this machine?
########################################
if [ -d '/usr/atria' ] ; then
    ccdirs=( '/usr/atria/bin' )
    ccman=( '/usr/atria/doc' '/usr/atria/doc/man' )

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
## Machpath in too soon in path for cygwin
########################################
if [ ${+windows_path} -ne 0 ] ; then
    for dir in ${windows_path} ; do
        dir=`echo ${dir} | sed 's/%/ /g'`
        if [ -d ${dir} ] ; then
            _path="${_path}:${dir}"
        fi
    done

    unset windows_path
fi

export PATH="${_path}"
export MANPATH="${_manpath}"
unset _path
unset _manpath


################################################################################
# Set the prompt
################################################################################

# This is a fix for cygwin so that we don't pick up the Windows native whoami,
# which will give a different result than cygwin commands.
# NOTE: Use -f because there is a cygwin bug where /bin executables are 000!
if [ -f '/bin/whoami' ] ; then
    WHOAMI='/bin/whoami'
else
    WHOAMI='whoami'
fi
curuser=`${WHOAMI} | sed 's/\\\\/\\//g'`
host=`uname -n | awk -F. '{print $1}'`

if [ "${LOGNAME}" != "${curuser}" ] ; then
    hostprompt="${curuser}@${host}"
else
    hostprompt="${host}"
fi
if [ ${+machtype} -ne 0 ] ; then
    machprompt="[${machtype}]:"
else
    machprompt=
fi

unset curuser
unset host

export BASE_PROMPT="${machprompt}${hostprompt}%#"

unset hostprompt
unset machprompt
unset machtype

################################################################################
# Miscellaneous settings
################################################################################
# Make user and login name the same thing
if [ ${+USER} -eq 0 ] && [ ${+LOGNAME} -ne 0 ] ; then
    export USER=${LOGNAME}
fi

umask 022

# Only set Emacs as the editor when we find it in /usr/bin.  This is a
# simplification because (at least) Git on Cygwin doesn't like to use the
# Windows Emacs when it exists in the path.  If a valid Emacs lives in a custom
# location, we can make fixes at that time.
if [ -x "/Applications/Emacs.app/Contents/MacOS/Emacs" ] ; then
    export EDITOR='/Applications/Emacs.app/Contents/MacOS/Emacs'
    export VISUAL='/Applications/Emacs.app/Contents/MacOS/Emacs'
    export WINEDITOR='/Applications/Emacs.app/Contents/MacOS/Emacs'
elif [ -x /usr/bin/emacs ] ; then
    export EDITOR='emacs'
    export VISUAL='emacs'
    export WINEDITOR='emacs'
fi

export ENV='~/.zprofile'

export ENSCRIPT='-2 -C -E -G -r -T4 --color=1 --style=mbisson --margins=15:15:15:15 --mark-wrapped-lines=arrow'

# Get the timezone set... sigh
if [ ${+TZ} -eq 0 ] ; then
    export TZ='EST5EDT'
fi

################################################################################
# Old STTY settings.  Uncomment for fun and edutainment...
################################################################################

# Make sysV braindamage look like berzerkeley braindamage
export TTY=`tty`
if [ $? -eq 0 ] ; then
    isatty=1
else
    isatty=0
fi

if [ ${isatty} ] ; then
    export STTY_PARAM='intr ^C quit ^\\ kill ^U eof ^D start ^Q stop ^S susp ^Z ixany'

    tmp=`uname`

    # ^H isn't right for Linux, it wants ^? instead
    if [ "${tmp}" != "Linux" ] ; then
#       export STTY_PARAM="${STTY_PARAM} erase ^H"
    fi

    # OS Specific stuff
    case ${tmp} in
        OSF)
            export STTY_PARAM="${STTY_PARAM} status ^T"
            ;;
        *)
            # I don't have anything to say here...
            ;;
    esac

    unset tmp
#   stty ${STTY_PARAM}
fi

unset isatty

################################################################################
# Set limits for the environment
################################################################################
ulimit -c unlimited                     # Core file size (blocks)

################################################################################
# Run any host-specific initialization
################################################################################
if [ -f "${HOME}/.zprofile.${HOSTNAME}" ] ; then
    source "${HOME}/.zprofile.${HOSTNAME}"
fi
