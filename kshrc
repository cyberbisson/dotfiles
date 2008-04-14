#! /bin/ksh -x
###############################################################################
# This file is read by ksh every time you start a shell.
###############################################################################
# For CCS machines:
# This file loads the central .kshrc that is stored in /ccs/etc/dotfiles.
# This is done because the Systems Group will occasionally update that 
# file to reflect changes in the network, and you may be better off if
# those changes are made for you.
###############################################################################
# Matt Bisson

# If we've not seen the login script, we need to load it.  Also, if we have
# seen the thing, but were configured for a different shell, we need to
# reload it.
if [ "" != "${USING_BASH}" ] ; then
    if [ "1" != "${USING_BASH}" ] ; then
        if [ "" != "${BASH}" ] ; then
            # Was set for bash, now using sh
            SAW_LOGIN_SCRIPT=
            . "${HOME}/.profile"
            return
        fi
    else
        if [ "" = "${BASH}" ] ; then
            # Was set for sh, now using bash
            SAW_LOGIN_SCRIPT=
            . "${HOME}/.profile"
            return
        fi
    fi
else
    # We've never seen the LOGIN script
    SAW_LOGIN_SCRIPT=
    . "${HOME}/.profile"
    return
fi

# This is the only way I know of to determine if I am on a CCS computer
# Place CCS only commands here...
if [ -f "/ccs/etc/dotfiles/.kshrc" ] ; then
    . "/ccs/etc/dotfiles/.kshrc"
fi

# Put csh aliases all in one file, in order to collect them in one
# place.  If you create more aliases, just add them to that file. 
if [ -f "${HOME}/.alias.ksh" ] ; then
    . "${HOME}/.alias.ksh"
fi

###############################################################################
# Miscellaneous settings
###############################################################################

# Use emacs keybindings
if   [ "" != "${KSH_VERSION}" ] ; then
    set -o emacs
elif [ "" != "${BASH}" ] ; then
    set -o emacs
fi

set +o ignoreeof                         # I want to log out with ^D

##############################################################################
# Set limits for the environment
##############################################################################
ulimit -c unlimited                     # Core file size (blocks)
