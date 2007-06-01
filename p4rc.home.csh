#!/bin/tcsh -x
# .p4rc.csh
###############################################################################
# Set up the perforce environment for work
###############################################################################
# Matt Bisson

setenv P4CLIENT "`whoami`-`hostname`"
setenv P4DIFF   'p4diff'
setenv P4EDITOR '/usr/bin/emacs'
setenv P4PASSWD 'p4sucks'
setenv P4PORT   'achilles.palm1.palmone.com:1667'
#setenv P4PORT  'grace.palm.com:1667'
setenv P4USER   'MattBisson'
