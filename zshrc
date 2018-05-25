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
cygwin | dtterm | eterm-color | linux | rxvt | rxvt-*color | xterm | \
xterm-*color | xterm-xfree86)
    PROMPT=%{$'\e[01;39m'%}${BASE_PROMPT}%{$'\e[00m'%}' '
    ;;
*)
    PROMPT=${BASE_PROMPT}' '
    ;;
esac

# For terminal emulators that advertize their color scheme with $COLORFGBG
# (rxvt, for one), having a value of "default" means it cannot represent the
# background color from 0-15.  This will be a problem for (terminal) Emacs and
# friends.
if ([ ${+COLORFGBG} -eq 0 ] && [[ ${TERM} =~ "rxvt" ]]) ||
       [[ "${COLORFGBG}" =~ "default" ]] ; then
    stty -icanon -echo min 0 time 1 ; printf '\e]11;?\e\\' ; read tmp_bg
    tmp_bg_avg=`echo $tmp_bg | cat -A | \
                sed 's/.*rgb:\([a-f0-9]*\)\/\([a-f0-9]*\)\/\([a-f0-9]*\).*/0x\1 0x\2 0x\3/' | \
                awk '{ print (strtonum($1)+strtonum($2)+strtonum($3))/3 }'`
    if [ $? -eq 0 ] ; then
        if [ $tmp_bg_avg -gt 32784 ] ; then
            export COLORFGBG='0;15'
        else
            export COLORFGBG='15;0'
        fi
    fi

    unset tmp_bg
    unset tmp_bg_avg
fi

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
# RXVT versions here, as well.
bindkey '^[Oc' forward-word
bindkey '^[Od' backward-word

# Delete ahead with delete key
bindkey '\e[3~' delete-char

# TODO: What??  I think this is just marking to compinstall that the
# configuration was written here.
zstyle :compinstall filename "${HOME}/.zshrc"

# Set up command completions
autoload -Uz compinit && compinit
