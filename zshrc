#!/bin/zsh -x
# .zshrc
################################################################################
# Z-Shell Interactive start-up script:
#
# This is the script that zhs reads when starting an interactive shell.  It is
# supposed to be read after .zprofile.  Place configuration in this file that
# customizes how you interact with your shell (prompt, completions, etc.).
################################################################################
# Matt Bisson

# If we've not seen the login script, we need to load it.  Also, if we have
# seen the thing, but were configured for a different shell, we need to
# reload it.
if [ ! ${SAW_ZPROFILE_SCRIPT} ] ; then
    source "${HOME}/.zprofile"
fi

# Put zsh aliases all in one file, in order to collect them in one place.  If
# you create more aliases, just add them to that file.
if [ -f "${HOME}/.alias.zsh" ] ; then
    source "${HOME}/.alias.zsh"
fi

if [ ${SRM_BRANCH} ] ; then
    if [ -f "${HOME}/.zshrc.${SRM_BRANCH}" ] ; then
        source "${HOME}/.zshrc.${SRM_BRANCH}"
    else
        echo ".zshrc: No loadable profile for SRM branch: '${SRM_BRANCH}'." \
        > /dev/stderr
    fi
fi

###############################################################################
# Miscellaneous settings
###############################################################################

case ${TERM} in
cygwin | dtterm | linux | rxvt | xterm | xterm-*color | xterm-xfree86)
    PROMPT=%{$'\e[01;39m'%}${PROMPT}%{$'\e[00m'%}' '
    ;;
esac

##############################################################################
# Set limits for the environment
##############################################################################
ulimit -c unlimited                     # Core file size (blocks)

###############################################################################
# Miscellaneous settings
###############################################################################

# History file settings
HISTFILE=${HOME}/.zsh_histfile
HISTSIZE=1024
SAVEHIST=1024

# Use emacs keybindings
bindkey -e

# TODO: What??
zstyle :compinstall filename "${HOME}/.zshrc"

autoload -Uz compinit && compinit
