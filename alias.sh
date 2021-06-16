#!/bin/sh -x
# .alias.sh
################################################################################
# Aliases set during .shrc.
#
# This file defines all the aliases for my Bourne Shell type environments.  Note
# that "alias" isn't a thing in Bourne Shell, so I've created functions to do
# the job.  There will be fewer of these than for other shell types, because
# it's a little more costly to update.
################################################################################
# Matt Bisson

# Place chars after certain file types
if [ `uname -s` = 'Linux' ] ; then
    ls() {
        env ls -Fv $*
    }
else
    ls() {
        env ls -F $*
    }
fi

# This is where emacs lives if compiled natively on MacOS.
if [ -x "${CARBON_EMACS_DIR}/Emacs" ] ; then
    emacs() {
        Emacs $*
    }
fi

# Make shorter names for Emacs client/server commands.
emacss() {
    emacs -f server-start $*
}
emacsc() {
    emacsclient $*
}

if [ -x '/usr/local/bin/less' ] || [ -x '/usr/bin/less' ] ; then
    more() {                            # Less IS more...
        less $*
    }
fi

# TTY Setting aliases
restty() {
    stty ${STTY_PARAM}
}
unstty() {
    stty `echo $STTY_PARAM | sed "s/\^./^@/g"`
}

# Simple functions
keb() {                                 # "Kill Emacs Backup Files"
    rm -f "*~"
    rm -f "#*#"
}
realias() {                             # Reread this file
    . "${HOME}/.alias.sh"
}
rexrdb() {                              # Reload the X defaults
    xrdb -m "${HOME}/.Xdefaults"
}
