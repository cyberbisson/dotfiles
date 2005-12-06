#!/bin/ksh -x
# .alias.ksh
# This file defines all the aliases for my KSH type environments.
# Matt Bisson

# Unset frivolous aliases
unalias -a

# ClearCase aliases
if [ -x /usr/atria/bin/cleartool ] ; then
    alias ct=cleartool
fi
if [ -x /usr/atria/bin/multitool ] ; then
    alias mt=multitool
fi

# Shell command aliases
alias push='set lwd=$cwd; pushd \!*'
alias pop='set lwd=$cwd; popd \!*'
alias ret='set __tmp__=$cwd;cd $lwd;set lwd=$__tmp__;unset __tmp__;dirs'
alias lwd='echo $lwd'
alias rm='rm -f'
alias hi=history
alias nes='tuxnes -E2'
alias gboy='xgnuboy --scale=2 --bind alt +a --bind ctrl +b'
alias snes='snes9x -r 7'
alias ls='ls -F'                        # Place chars after certain file types

# A convenience macro for the Mickey Mouse Cicso VPN client
VPNDIR="/etc/CiscoSystemsVPNClient/Profiles"
if [ -d ${VPNDIR} ] ; then
	alias vpnstart="'cp' -f ${VPNDIR}/palm.mod.pcf ${VPNDIR}/palm.pcf; /opt/cisco-vpnclient/bin/vpnclient connect palm"
fi

if [ -e /usr/local/bin/less ] || [ -e /usr/bin/less ]; then
    alias  more=less                    # Less IS more...
    export PAGER=less
fi

# TTY Setting aliase
alias restty='stty ${STTY_PARAM}'
alias unstty='stty `echo $STTY_PARAM | sed "s/\^./^@/g"`'

# stuff for setting Xterm window names (title bar and icon name respectively)
#alias title='echo -n "^[]2;\!:1^G"'
#alias wname='echo -n "^[]1;\!:1^G"'
#alias Xname='title \!:1; wname \!:1'

# Simple functions
alias keb='rm -f *~; rm -f #*#'         # "Kill Emacs Backup Files"
alias realias='. ~/.alias.ksh'          # Reread this file
alias rexrdb='xrdb -m ~/.Xdefaults'     # Reload the X defaults

if [ "${USE_HOLLY}" ] ; then
    # Alias crap
    alias armrel="${HOLLY_BASE}/build/scripts/setup-holly arm-linux  release"
    alias simrel="${HOLLY_BASE}/build/scripts/setup-holly i386-linux release"
    alias armdbg="${HOLLY_BASE}/build/scripts/setup-holly arm-linux  debug"
    alias simdbg="${HOLLY_BASE}/build/scripts/setup-holly i386-linux debug"
    # Dumb
    alias sqlite="${HOLLY_SYSTEM_ROOT}/usr/bin/sqlite3"
	alias hoseenv="LD_LIBRARY_PATH=\"${HOLLY_SYSTEM_ROOT}/usr/lib\"; export LD_LIBRARY_PATH"
fi
