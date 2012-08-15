#!/bin/tcsh -X
# .login
###############################################################################
# Login script:
# This file is read by csh/tcsh when you have started a "login" shell. 
# It will do more serious initialization in .tcshrc
###############################################################################
# For CCS machines:
# This file loads the central .login that is stored in /ccs/etc/dotfiles.
# This is done because the Systems Group will occasionally update that 
# file to reflect changes in the network, and you may be better off if
# those changes are made for you, but it is doubtful.
###############################################################################
# Matt Bisson

if (${?SAW_LOGIN_SCRIPT}) then
    exit
endif

setenv SAW_LOGIN_SCRIPT 1

# Read the CCS's .login file
if (-f '/ccs/etc/dotfiles/.login') then
    source '/ccs/etc/dotfiles/.login'
endif

# Read HollyOS profile
if ( -f "${HOME}/.hollyrc.csh" ) then
    source "${HOME}/.hollyrc.csh"
endif

# Read CVS environment
if ( -f "${HOME}/.cvsrc.csh" ) then
    source "${HOME}/.cvsrc.csh"
endif

###############################################################################
# If TERM is undefined, or it is not an acceptable type
###############################################################################

# Annoying
if (0 == ${?TERM}) then
    setenv TERM "vt100"
else if (("unknown" == ${TERM}) || ("ANSI" == ${TERM}) || \
    ("network" == ${TERM})) then
    setenv TERM "vt100"
endif

#if ((0 == ${?TERM}) && (("unknown" == ${TERM}) || ("ANSI" == ${TERM}) || \
#    ("network" == ${TERM}))) then

#    echo "Note: Changing terminal type..."
#    set   noglob
#    eval `tset -s -I -Q "vt100"`
#    unset noglob

#endif

###############################################################################
# Get computer information
###############################################################################

# This is a fix for cygwin so that we don't pick up the Windows native whoami,
# which will give a different result than cygwin commands.
if (-x '/bin/whoami') then
    set WHOAMI='/bin/whoami'
else
    set WHOAMI='whoami'
endif

set hwclass=`uname -m`
set host=`uname -n | awk -F. '{print $1}'`
set OSrelease=`uname -r`
set OSname=`uname -s`
set OSver=`uname -v`
set curuser=`${WHOAMI} | sed 's/\\/\//g'`

# Make sysV braindamage look like berzerkeley braindamage
setenv TTY `tty`
if (${status} == 0) then
    set isatty="1"
else
    set isatty="0"
endif

# What type of CPU
if ((-x '/bin/machine') || (-x '/usr/bin/machine') || \
    (-x '/usr/local/bin/machine') || (-x "${HOME}/machine")) then
    setenv CPU  `machine`
    setenv CCPU ${CPU}
endif

###############################################################################
# Perform machine specific initializations
###############################################################################
switch (${OSname})

########################################
## Linux
########################################
case "Linux"

    set machdirs=(  )
    set machman=(  )
    setenv XENVIRONMENT "${HOME}/.Xdefaults"

    if      (-f '/etc/gentoo-release') then
        set distro="gentoo"

        set machdirs=( /opt/bin /usr/kde/3.5/sbin /usr/kde/3.5/bin /usr/qt/3/bin /opt/vmware/workstation/bin /opt/ghc/bin )
        set machman=( /usr/local/share/man /usr/share/man /usr/share/binutils-data/i686-pc-linux-gnu/2.17/man /usr/share/gcc-data/i686-pc-linux-gnu/4.1.2/man /opt/sun-jdk-1.4.2.13/man /etc/java-config/system-vm/man /usr/kde/3.5/share/man /usr/qt/3/doc/man /opt/vmware/workstation/man /opt/bin )

    else if (-f '/etc/redhat-release') then
        set tmp=`grep -q Enterprise /etc/redhat-release`
        if (0 == ${status}) then
            set distro="rhel"
        else
            set distro="rhat"
        endif
        unset tmp
    else if (-f '/etc/fedora-release') then
        set distro="fedora"
    else if (-f '/etc/redhat_version') then
        set distro="rhat"
    else if ((-f '/etc/debian_release') || (-f '/etc/debian_version')) then
        set distro="deb"
    else if ((-f '/etc/slackware-release') || (-f '/etc/slackware-version')) then
        set distro="slack"
    else if (-f '/etc/mandrake-release') then
        set distro="mndrk"
    else if (-f '/etc/SuSE-release') then
        set ver=`grep VERSION /etc/SuSE-release | awk '{print $3}' | awk -F. '{print $1}'`

        grep -q openSUSE /etc/SuSE-release
        if (0 == ${status}) then
            set machtype="openSuSE-${ver}"
        else
            set machtype="sles${ver}"
        endif

        set machdirs=( /opt/gnome/sbin /opt/gnome/bin /opt/kde3/sbin /opt/kde3/bin )
        set machman=( /opt/gnome/share/man )
        unset ver
    else if (-f '/etc/yellowdog-release') then
        set distro="yellow"
    else if (-f '/etc/UnitedLinux-release') then
        set distro="united"
    else if (-f '/etc/lsb-release') then
        set distro="ubuntu"
    else
        set distro="lnux"
    endif

    if (0 == ${?machtype}) then
        set machtype=${distro}-${hwclass}
    endif
    unset distro
    breaksw

########################################
## FreeBSD
########################################
case "FreeBSD":
    set shortrel=`echo $OSrelease | sed 's/\([0-9]*\.[0-9]*\).*/\1/'`
    set machtype="bsd${shortrel}-${hwclass}"
    set machdirs=(  )
    set machman=(  )
    unset shortrel
    breaksw

########################################
## SUN specific processing
########################################
case "SunOS":
    setenv OPENWINHOME "/usr/openwin"

    set shortrel=`echo $OSrelease | awk -F. '{print $1}'`
    set machtype=sun${shortrel}

    if (${shortrel} >= 5) then
        set machdirs=( /usr/ccs/bin /opt/SUNWspro/bin ${OPENWINHOME}/bin )
        setenv LD_LIBRARY_PATH "/opt/SUNWspro/lib:/usr/ccs/lib:${OPENWINHOME}/lib:/usr/lib"

        # Look for the Sun Java packages
        if ( -d /opt/java122 ) then
            set machdirs=( $machdirs /opt/java122/bin )
        endif
        if ( -d /opt/JDK120 ) then
            set machdirs=( $machdirs /opt/JDK120/bin )
        endif
    else
        set machdirs=(  )
    endif

    set machman= ( /opt/SUNWspro/man ${OPENWINHOME}/man )
    setenv XTERM ${OPENWINHOME}/bin/xterm

    unset shortrel
    breaksw

########################################
## HP-UX specific processing
########################################
case "HP-UX":
    set shortrel=`echo ${OSrelease} | awk -F. '{print $2}'`
    if (`echo ${hwclass} | awk -F/ '{print " "$2}' | grep " 8"`) then
        set tempCPU=800
    else
        set tempCPU=pa
    endif

    set machtype=hp${shortrel}_${tempCPU}
    unset shortrel
    unset tempCPU

    set machdirs=( /usr/contrib/bin )
    set machman=( /usr/contrib )
    breaksw

########################################
## SGI specific processing
########################################
case "IRIX":
case "IRIX64":
    set shortrel=`echo $OSrelease | awk -F. '{print $1}'`
    set machtype=sgi${shortrel}
    unset shortrel

    set machdirs=( /usr/bsd )
    set machman=(  )
    breaksw

########################################
## IBM AIX
########################################
case "AIX":
    set machtype=aix${OSver}_power

    if ( -d /usr/lpp/cmvc/bin ) then
        set cmvcpath=( /usr/lpp/cmvc/bin /usr/lpp/cmvc/samples )
    else
        set cmvcpath=(  )
    endif

    set machdirs=( /usr/include/X11/bitmaps /usr/lib/X11 /usr/lib/X11/app-defaults ${cmvcpath} )
    set machman=(  )

    unset cmvcpath
    breaksw

########################################
## Digital/Compaq UNIX
########################################
case "OSF1":
    set machtype=osf1_axp
    set machdirs=( /usr/bin/mh )
    set machman=(  )
    breaksw

########################################
## SCO UNIXWare
########################################
case "UnixWare":
    set machtype=uw2_x86
    set machdirs=(  )
    set machman=(  )
    breaksw

########################################
##  Mac OS X
########################################
case "Darwin":
    set machtype="osx_${CPU}"
    set machdirs=(  )
    set machman=(  )
    breaksw

########################################
## System V type UNIX
########################################
case "UNIX_SV":
    set machtype=att3_x86
    set machdirs=(  )
    set machman=(  )
    breaksw

########################################
## Siemans-Nixdorf UNIX
########################################
case "SINIX-N":
    set shortrel=`echo $OSrelease | awk -F. '{print  $1}'`
    set machtype=sni${shortrel}

    unset shortrel

    set machdirs=(  )
    set machman=(  )
    breaksw

########################################
## CygWin (for Win2k & XP)
########################################
case "CYGWIN_NT-6.0":
case "CYGWIN_NT-6.1-WOW64":
case "CYGWIN_NT-5.1":
    set machtype="cygwin-${hwclass}"
    set winpath=`cygpath -u -S`
    set windows_path=( `${winpath}/cmd /c path               | \
                      sed 's/^PATH\=/;/'                     | \
                      sed 's/\([A-Za-z]\):/\/cygdrive\/\1/g' | \
                      sed 's/\\/\//g'                        | \
                      sed 's/[^;]*cygwin[^;]*;//g'           | \
                      sed 's/ /\\ /g'                        | \
                      sed 's/;/ /g'`)
    set machdirs=( )
    set machman=( )

# TODO: This is Intellivid specific!
setenv CXX_DEBUG_INFO         off
setenv CXX_OPTIMIZATION_MODE  on
setenv CXX_RUNTIME_CHECKS     off
setenv CXX_STDLIB_DEBUG       off
setenv CXX_NO_DEFAULT_INLINES off
setenv CXX_NOSYNC             1

setenv CVS_RSH                'ssh'
setenv TEMP                   '/tmp'
setenv TMP                    '/tmp'
#
    breaksw;

########################################
## Undeterminable UNIX type
########################################
default:
    set machdirs=(  )
    set machman=(  )
    breaksw
endsw

###############################################################################
# Build paths and man pages paths (This is a lot of optimized crap so that I
# can go onto any other computer and not have a 10 line path).
###############################################################################

########################################
## Root doesn't get to use '.'
########################################
set userid=`id | sed 's/[^0-9]*\([0-9]*\).*/\1/'`
if (${userid} != "0") then
    set _path="."
    set _manpath="."
endif
unset userid

########################################
## Set up my 'simulator' environment
########################################
if ((${?USE_HOLLY}) && (-d /opt/holly)) then
    set hollydirs=( ${HOLLY_TOOL_ROOT}/i386-linux/local/bin ${HOLLY_TOOL_ROOT}/bin )

    foreach dir ( ${hollydirs} )
        if (-d ${dir}) then
            set _path="${_path}:${dir}"
        endif
    end

    unset hollydirs
endif

########################################
## I want these dirs in the path even if they don't exist
########################################
set basedirs=( ~/bin ~/bin/Linux ~/bin/scripts )
set baseman=( ~/man )

foreach dir ( ${basedirs} )
    set _path="${_path}:${dir}"
end
foreach dir ( ${baseman} )
    set _manpath="${_manpath}:${dir}"
end

unset basedirs
unset baseman

########################################
## Search the /usr/local area
########################################
if (-d /usr/local) then
    set localdirs=( /usr/local/bin /usr/local/sbin )
    set localman=( /usr/local/man )

    foreach dir ( ${localdirs} )
        if (-d ${dir}) then
            set _path="${_path}:${dir}"
        endif
    end
    foreach dir ( ${localman} )
        if (-d ${dir}) then
            set _manpath="${_manpath}:${dir}"
        endif
    end

    unset localdirs
    unset localman
endif

########################################
## If a root-local dir exists
########################################
if (-d /local) then
    set localdirs=( /local/bin /local/gnu/bin /local/apps/mh )
    set localman=( /local/man /local/apps/X11R5/man /local/gnu/man /local/apps/mh/man )

    foreach dir ( ${localdirs} )
        if (-d ${dir}) then
            set _path="${_path}:${dir}"
        endif
    end
    foreach dir ( ${localman} )
        if (-d ${dir}) then
            set _manpath="${_manpath}:${dir}"
        endif
    end

    unset localdirs
    unset localman
endif

########################################
## Machine-specific paths gathered above
########################################
foreach dir ( ${machdirs} )
    if (-d ${dir}) then
        set _path="${_path}:${dir}"
    endif
end
foreach dir ( ${machman} )
    if (-d ${dir}) then
        set _manpath="${_manpath}:${dir}"
    endif
end

unset machdirs
unset machman

########################################
## These are very common dirs
########################################
set commondirs=( /usr/sbin /usr/ucb /bin /sbin /usr/bin /usr/share/bin )
set commonman=( /usr/man /usr/share/man /usr/catman )

foreach dir ( ${commondirs} )
    if (-d ${dir}) then
        set _path="${_path}:${dir}"
    endif
end
foreach dir ( ${commonman} )
    if (-d ${dir}) then
        set _manpath="${_manpath}:${dir}"
    endif
end

unset commondirs
unset commonman

########################################
## CDE paths
########################################
if (-d /usr/dt) then
    set cdedirs=( /usr/dt/bin )
    set cdeman=( /usr/dt/man )

    foreach dir ( ${cdedirs} )
        if (-d ${dir}) then
            set _path="${_path}:${dir}"
        endif
    end
    foreach dir ( ${cdeman} )
        if (-d ${dir}) then
            set _manpath="${_manpath}:${dir}"
        endif
    end

    unset cdedirs
    unset cdeman
endif

########################################
## Basic X-windows stuff
########################################
if (-d /usr/bin/X11) then
    set xdirs=( /usr/bin/X11 /usr/bin/X11/demos )

    foreach dir ( ${xdirs} )
        if (-d ${dir}) then
            set _path="${_path}:${dir}"
        endif
    end

    unset xdirs
endif

########################################
## Any GNU stuff that's been broken out...
########################################
if (-d /usr/gnu) then
    set gnudirs=( /usr/gnu/bin /usr/gnu/bin )
    set gnuman=( /usr/gnu/man )

    foreach dir ( ${gnudirs} )
        if (-d ${dir}) then
            set _path="${_path}:${dir}"
        endif
    end
    foreach dir ( ${gnuman} )
        if (-d ${dir}) then
            set _manpath="${_manpath}:${dir}"
        endif
    end

    unset gnudirs
    unset gnuman
endif

########################################
## Oracle environment setup
########################################
if (-f "${HOME}/.oracle.csh") then
    source "${HOME}/.oracle.csh"

    set oradirs=( ${ORACLE_HOME}/bin ${ORACLE_HOME}/oc4j/bin )

    foreach dir ( ${oradirs} )
        if (-d ${dir}) then
            set _path="${_path}:${dir}"
        endif
    end

    # I want to use a specific login script for PL/SQL
    if (-f ~/sql/login.sql) then
        setenv SQLPATH "${HOME}/sql"
    endif

    unset oradirs
endif

########################################
## Northeastern's CCS keeps arch-specific stuff separate on an NFS share
########################################
if (-d /arch) then
    set ccsdirs=( /arch/X11R6/bin /arch/Xapps/bin /arch/adm/bin /arch/beta/bin /arch/com/bin /arch/daemons/bin /arch/gnu/bin /arch/unix/bin )
    set ccsman=( /arch/unix/man /arch/com/man /arch/unix/packages/j2sdk1_3_0beta/man /arch/gnu/man /arch/Xapps/man /arch/X11R6/man )

    foreach dir ( ${ccsdirs} )
        if (-d ${dir}) then
            set _path="${_path}:${dir}"
        endif
    end
    foreach dir ( ${ccsman} )
        if (-d ${dir}) then
            set _manpath="${_manpath}:${dir}"
        endif
    end

    unset ccsdirs
    unset ccsman
endif
if (-d /share) then
    set ccsdirs=( /share/unix/bin /share/com/bin /share/Xapps/bin )
    set ccsman=( /share/unix/man /share/com/man /share/Xapps/man /share/Xapps/man )

    foreach dir ( ${ccsdirs} )
        if (-d ${dir}) then
            set _path="${_path}:${dir}"
        endif
    end
    foreach dir ( ${ccsman} )
        if (-d ${dir}) then
            set _manpath="${_manpath}:${dir}"
        endif
    end

    unset ccsdirs
    unset ccsman
endif
if (-d /ccs) then
    set ccsdirs=( /ccs/bin )
    set ccsman=( /ccs/man )

    foreach dir ( ${ccsdirs} )
        if (-d ${dir}) then
            set _path="${_path}:${dir}"
        endif
    end
    foreach dir ( ${ccsman} )
        if (-d ${dir}) then
            set _manpath="${_manpath}:${dir}"
        endif
    end

    unset ccsdirs
    unset ccsman
endif

## Is ClearCase on this machine?
if (-d /usr/atria) then
    set ccdirs=( /usr/atria/bin )
    set ccman=( /usr/atria/doc /usr/atria/doc/man )

    foreach dir ( ${ccdirs} )
        if (-d ${dir}) then
            set _path="${_path}:${dir}"
        endif
    end
    foreach dir ( ${ccman} )
        if (-d ${dir}) then
            set _path="${_path}:${dir}"
        endif
    end

    unset ccdirs
    unset ccman
endif

########################################
## Machpath in too soon in path for cygwin
########################################
if ( ${?windows_path} ) then
    foreach dir ( ${windows_path} )
        if (-d ${dir}) then
            set _path="${_path}:${dir}"
        endif
    end

    unset windows_path
endif

########################################
## Now we've got a real path!!!
########################################
setenv PATH "${_path}"
setenv MANPATH "${_manpath}"

# Strip any ':' at the beginning of the path
setenv PATH    `echo ${PATH}    | sed 's/^://'`
setenv MANPATH `echo ${MANPATH} | sed 's/^://'`

unset _path
unset _manpath

###############################################################################
# Set the prompt
###############################################################################
if ("${LOGNAME}" != "${curuser}") then
    set hostprompt="${curuser}@${host}"
else
    set hostprompt="${host}"
endif
if (${?machtype}) then
    set machprompt="[${machtype}]:"
else
    set machprompt=
endif

setenv PROMPT  "$machprompt$hostprompt"
setenv PROMPT2 '%R>    '
setenv PROMPT3 'Correct (%R)? [y|n|e|a]: '

unset machtype
unset machprompt
unset hostprompt

###############################################################################
# Miscellaneous settings
###############################################################################
# Make user and login name the same thing
if ((${?USER} == "0") && (${?LOGNAME} != "0")) then
    setenv USER ${LOGNAME}
endif

umask 022

setenv EDITOR    "emacs"
setenv VISUAL    "emacs"
setenv WINEDITOR "emacs"
setenv ENV       "~/.profile"

setenv ENSCRIPT  "-2 -C -E -G -r -T4 --color=1 --style=mbisson --margins=15:15:15:15 --mark-wrapped-lines=arrow"

# Get the timezone set... sigh
if ( ! ${?TZ} ) then
    setenv TZ "EST5EDT"
endif

###############################################################################
# If the login is on an X display but the DISPLAY variable has not 
# been set, ask the user what the DISPLAY should be (e.g., rlogin)
###############################################################################

# Annoying...
#if (1 == ${?TERM}) then
#    if (((${TERM} =~ dtterm) || (${TERM} =~ xterm*)) && (0 == ${?DISPLAY})) then

#        echo -n "What DISPLAY are you using [default: NONE]? "
#        set response=($<)

#        if ("${response}" != "") then
#            if ("$response" =~ *:*) then
#                echo "Setting DISPLAY to $response"
#                setenv DISPLAY "$response"
#            else
#                echo "Setting DISPLAY to ${response}:0".0
#                setenv DISPLAY "${response}":0.0
#            endif
#        else
#            # Allow this to be undefined, and we will display things
#            # on the terminal window.
#        endif
#    endif
#endif

###############################################################################
# Old STTY settings.  Uncomment for fun and edutainment...
###############################################################################
if (${isatty}) then
    setenv STTY_PARAM 'intr ^C quit ^\\ kill ^U eof ^D start ^Q stop ^S susp ^Z ixany'

    set tmp=`uname`

    # ^H isn't right for Linux, it wants ^? instead
    if (${tmp} != "Linux") then
#       setenv STTY_PARAM "${STTY_PARAM} erase ^H"
    endif

    # OS Specific stuff
    switch (${tmp})
    case OSF1:
        setenv STTY_PARAM "${STTY_PARAM} status ^T"
        breaksw
    default:
        # I don't have anything to say here...
        breaksw
    endsw

    unset tmp
    stty ${STTY_PARAM}
endif

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

unsetenv LS_COLORS

###############################################################################
# Now read my real initialization file
###############################################################################
#if (${SHELL} =~ *tcsh) then            # This seems to persist in nested shells.
if (${?version}) then
    setenv USING_TCSH 1
    source "${HOME}/.tcshrc"
else
    setenv USING_TCSH 0
    source "${HOME}/.cshrc"
endif
