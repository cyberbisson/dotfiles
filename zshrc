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
if [ ${+SAW_ZPROFILE_SCRIPT} -eq 0 ] || [ ${SAW_ZPROFILE_SCRIPT} -eq 0 ] ; then
    source "${HOME}/.zprofile"
fi

# Put zsh aliases all in one file, in order to collect them in one place.  If
# you create more aliases, just add them to that file.
if [ -f "${HOME}/.alias.zsh" ] ; then
    source "${HOME}/.alias.zsh"
fi

# Add special environment for VMware development if desired
if [ ${+VMWARE_BRANCH} -ne 0 ] ; then
    if [ -f "${HOME}/.zshrc.${VMWARE_BRANCH}" ] ; then
        source "${HOME}/.zshrc.${VMWARE_BRANCH}"
    else
        echo ".zshrc: No loadable profile for VMware branch: '${VMWARE_BRANCH}'." \
        > /dev/stderr
    fi
fi

################################################################################
# Miscellaneous settings
################################################################################

case ${TERM} in
cygwin | dtterm | eterm-color | linux | rxvt | xterm | xterm-*color | \
xterm-xfree86)
    PROMPT=%{$'\e[01;39m'%}${BASE_PROMPT}%{$'\e[00m'%}' '
    ;;
*)
    PROMPT=${BASE_PROMPT}' '
    ;;
esac

################################################################################
# Miscellaneous settings
################################################################################

# History file settings
export HISTFILE=${HOME}/.zsh_histfile
export HISTSIZE=1024
export SAVEHIST=1024

# Hate hate hate colored ls output
unset LS_COLORS

# Set up ZSH options:
unsetopt BG_NICE
setopt   C_BASES
unsetopt IGNORE_EOF
unsetopt HUP
setopt   LIST_PACKED
unsetopt RM_STAR_SILENT # Might be good since I alias rm -f sometimes.
setopt   SH_NULL_CMD

# Emacs shell-mode does all the driving, so take this control from ZSH
if [ "${EMACS}" = "t" ] ; then
    unsetopt zle
fi

# Use emacs keybindings
bindkey -e

# Ctrl+LEFT / Ctrl+RIGHT move entire words
bindkey ';5C' forward-word
bindkey ';5D' backward-word

# Delete ahead with delete key
bindkey '\e[3~' delete-char

# TODO: What??  I think this is just marking to compinstall that the
# configuration was written here.
zstyle :compinstall filename "${HOME}/.zshrc"

# Set up command completions
autoload -Uz compinit && compinit
