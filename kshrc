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

set +o ignoreeof                         # I want to log out with ^D

# Use emacs keybindings
case ${SHELL} in
    */sh)
        # Do nothing
        ;;

    *)
        set -o emacs
        ;;
esac

##############################################################################
# Set limits for the environment
##############################################################################
ulimit -c unlimited                     # Core file size (blocks)
