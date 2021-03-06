#!/bin/tcsh -X
# .alias.csh
################################################################################
# Aliases set during .[t]cshrc.
#
# This file defines all the aliases for my (T)CSH type environments.
################################################################################
# Matt Bisson

# Unset frivolous aliases
unalias alais
unalias csl
unalias d
unalias helpcommand
unalias l
unalias ll
unalias maek
unalias sl

# ClearCase aliases
if (-x '/usr/atria/bin/cleartool') then
    alias ct   cleartool
endif
if (-x '/usr/atria/bin/multitool') then
    alias mt   multitool
endif

# Shell command aliases
alias hi   history
alias nes  'tuxnes -E2'
alias gboy 'xgnuboy --scale=2 --bind alt +a --bind ctrl +b'
alias snes 'snes9x -r 7'

# Place chars after certain file types
if (`uname -s` == 'Linux') then
    alias ls 'ls -Fv'
else
    alias ls 'ls -F'
endif

# This is where emacs lives if compiled natively on MacOS.
if (-x "${CARBON_EMACS_DIR}/Emacs") then
    alias emacs "Emacs"
endif

# Make shorter names for Emacs client/server commands.
alias emacss "emacs -f server-start"
alias emacsc "emacsclient"

# A convenience macro for the Mickey Mouse Cicso VPN client
set VPNDIR="/etc/CiscoSystemsVPNClient/Profiles"
if (-d ${VPNDIR}) then
    alias vpnstart "'cp' -f ${VPNDIR}/palm.mod.pcf ${VPNDIR}/palm.pcf; /opt/cisco-vpnclient/bin/vpnclient connect palm"
endif

if ((-x '/usr/local/bin/less') || (-x '/usr/bin/less')) then
    alias more 'less'                   # Less IS more...
endif

# TTY Setting aliases
alias restty 'stty ${STTY_PARAM}'
alias unstty 'stty `echo $STTY_PARAM | sed "s/\^./^@/g"`'

# stuff for setting Xterm window names (title bar and icon name respectively)
#alias title 'echo -n "^[]2;\!:1^G"'
#alias wname 'echo -n "^[]1;\!:1^G"'
#alias Xname 'title \!:1; wname \!:1'

# Simple functions
alias keb     'rm -f *~; rm -f #*#'     # "Kill Emacs Backup Files"
alias realias 'source ~/.alias.csh'     # Reread this file
alias rexrdb  'xrdb -m ~/.Xdefaults'    # Reload the X defaults

if (${?USE_HOLLY}) then
    # Alias crap
    alias armrel  "${HOLLY_BASE}/build/scripts/setup-holly arm-linux  release"
    alias simrel  "${HOLLY_BASE}/build/scripts/setup-holly i386-linux release"
    alias armdbg  "${HOLLY_BASE}/build/scripts/setup-holly arm-linux  debug"
    alias simdbg  "${HOLLY_BASE}/build/scripts/setup-holly i386-linux debug"
    # Dumb
    alias sqlite  "${HOLLY_SYSTEM_ROOT}/usr/bin/sqlite3"
    alias hoseenv "setenv LD_LIBRARY_PATH ${HOLLY_SYSTEM_ROOT}/usr/lib"
endif
