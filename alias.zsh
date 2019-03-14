
# .alias.zsh
# This file defines all the aliases for my CSH type environments.
# Matt Bisson

# ClearCase aliases
if [ -x '/usr/atria/bin/cleartool' ] ; then
    alias ct=cleartool
fi
if [ -x '/usr/atria/bin/multitool' ] ; then
    alias mt=multitool
fi

# Shell command aliases
#alias rm='rm -f'
alias hi=history
alias nes='tuxnes -E2'
alias gboy='xgnuboy --scale=2 --bind alt +a --bind ctrl +b'
alias snes='snes9x -r 7'
alias ls='ls -Fv'                       # Place chars after certain file types

# This is where emacs lives if compiled natively on MacOS
CARBON_EMACS_DIR="/Applications/Emacs.app/Contents/MacOS"
if [ -d ${CARBON_EMACS_DIR} ] ; then
    alias emacs="${CARBON_EMACS_DIR}/Emacs"
fi

# Make shorter names for Emacs client/server commands.
alias emacss="emacs -f server-start"
alias emacsc="emacsclient"

# A convenience macro for the Mickey Mouse Cicso VPN client
VPNDIR="/etc/CiscoSystemsVPNClient/Profiles"
if [ -d ${VPNDIR} ] ; then
    alias vpnstart="'cp' -f ${VPNDIR}/palm.mod.pcf ${VPNDIR}/palm.pcf; /opt/cisco-vpnclient/bin/vpnclient connect palm"
fi

if [ -x '/usr/local/bin/less' ] || [ -x '/usr/bin/less' ] ; then
    alias  more=less                    # Less IS more...
fi

# TTY Setting aliases
alias restty='stty ${STTY_PARAM}'
alias unstty='stty `echo $STTY_PARAM | sed "s/\^./^@/g"`'

# stuff for setting Xterm window names (title bar and icon name respectively)
#alias title='echo -n "^[]2;\!:1^G"'
#alias wname='echo -n "^[]1;\!:1^G"'
#alias Xname='title \!:1; wname \!:1'

# Simple functions
alias keb='rm -f *~; rm -f #*#'         # "Kill Emacs Backup Files"
alias realias='source ~/.alias.zsh'     # Reread this file
alias rexrdb='xrdb -m ~/.Xdefaults'     # Reload the X defaults
