#!/bin/ksh -x
# .kshrc
################################################################################
# Bourne/Korn-Shell Interactive start-up script:
#
# This is the script that [k]sh reads when starting an interactive shell.  It is
# supposed to be read after .profile.  Place configuration in this file that
# customizes how you interact with your shell (prompt, completions, etc.).
#
# NOTE: For the benefit of Bourne shells, use $HOME instead of "~".
################################################################################
# For CCS machines:
# This file loads the central .kshrc that is stored in /ccs/etc/dotfiles.  This
# is done because the Systems Group will occasionally update that file to
# reflect changes in the network, and you may be better off if those changes are
# made for you.
################################################################################
# Matt Bisson

#echo '******** RUNNING kshrc    '`date '+%M:%S.%N'`

# If we've not seen the login script, we need to load it.  Also, if we have seen
# the thing, but were configured for a different shell, we need to reload it.
if [ "" != "${USING_BASH}" ] ; then
    if [ "1" != "${USING_BASH}" ] ; then
        if [ "" != "${BASH}" ] ; then
            # Was set for bash, now using sh
            SAW_LOGIN_SCRIPT=
            . "${HOME}/.profile"
            return > /dev/null 2>&1
        fi
    else
        if [ "" = "${BASH}" ] ; then
            # Was set for sh, now using bash
            SAW_LOGIN_SCRIPT=
            . "${HOME}/.profile"
            return > /dev/null 2>&1
        fi
    fi
else
    # We've never seen the LOGIN script
    SAW_LOGIN_SCRIPT=
    . "${HOME}/.profile"
    return > /dev/null 2>&1
fi

# This is the only way I know of to determine if I am on a CCS computer.  Place
# CCS only commands here...
if [ -f "/ccs/etc/dotfiles/.kshrc" ] ; then
    . "/ccs/etc/dotfiles/.kshrc"
fi

# Put [k]sh aliases all in one file, in order to collect them in one place.  If
# you create more aliases, just add them to that file.
alias_file="${HOME}/.alias.ksh"
if [ "${KSH_VERSION}" = "" ] ; then
    if [ "${USING_BASH}" != "1" ] ; then
        # Bourne Shell doesn't use "alias", but I've provided functions that
        # have the same effect for many KSH aliases.
        alias_file="${HOME}/.alias.sh"
    fi
fi

if [ -f "${alias_file}" ] ; then
    . "${alias_file}"
fi
unset alias_file

# Add special environment for VMware development if desired
if [ "${VMWARE_PROFILE}" ] ; then
    . "${HOME}/sb/dotfiles/scripts/use_devprofile.sh"
fi

################################################################################
# Terminal-specific customization
################################################################################

case ${TERM} in
cygwin | dtterm | eterm-color | linux | rxvt | rxvt-unicode | rxvt-*color | \
sun-color | xterm | xterm-*color | xterm-xfree86)
    if [ "" != "${BASH}" ] ; then
        PS1="\[\033[01;39m\]${BASE_PROMPT}\[\033[00m\] "
    elif [ "" != "${KSH_VERSION}" ] ; then
        PS1=$'\E[01;39m${BASE_PROMPT}\E[00m '
    else
        PS1="${BASE_PROMPT} "
    fi
    ;;
*)
    PS1="${BASE_PROMPT} "
    ;;
esac
export PS1

# For terminal emulators that advertise their color scheme with $COLORFGBG
# (rxvt, for one), having a value of "default" means it cannot represent the
# background color from 0-15.  This will be a problem for (terminal) Emacs and
# friends.
if [ "${COLORFGBG}" = "" ] ; then
    case ${TERM} in
        rxvt*)
            reset_color_fgbg=1
            ;;
    esac
else
    case ${COLORFGBG} in
        *default*)
            reset_color_fgbg=1
            ;;
    esac
fi
# Bource shell does not support fuzzy matching with ${VAR#str}, so we have the
# temporary variable and switches above.
if [ ${reset_color_fgbg} ] ; then
    stty -icanon -echo min 0 time 1 ; printf '\e]11;?\e\\' ; read tmp_bg
    tmp_bg_avg=`echo $tmp_bg | cat -vet | \
                sed 's/.*rgb:\([a-f0-9]*\)\/\([a-f0-9]*\)\/\([a-f0-9]*\).*/0x\1 0x\2 0x\3/' | \
                awk '{ print int(((0+$1)+(0+$2)+(0+$3))/3) }'`
    if [ $? = 0 ] ; then
        if [[ $tmp_bg_avg > 32784 ]] ; then
            COLORFGBG='0;15'
        else
            COLORFGBG='15;0'
        fi
        export COLORFGBG
    fi

    unset reset_color_fgbg
    unset tmp_bg
    unset tmp_bg_avg
fi

################################################################################
# Miscellaneous settings
################################################################################

# History file settings
HISTFILE=${HOME}/.sh_histfile; export HISTFILE
HISTSIZE=1024; export HISTSIZE
SAVEHIST=1024; export SAVEHIST

# Hate hate hate colored ls output
unset LS_COLORS

set +o ignoreeof                        # I want to log out with ^D

# Use emacs keybindings
if   [ "" != "${KSH_VERSION}" ] ; then
    set -o emacs
elif [ "" != "${BASH}" ] ; then
    set -o emacs
fi

#echo '******** DONE   kshrc    '`date '+%M:%S.%N'`
