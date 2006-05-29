#!/bin/ksh -x
# .p4rc.ksh
###############################################################################
# Set up the perforce environment for work
###############################################################################
# Matt Bisson

P4CLIENT='mbisson-isis'
P4EDITOR='/usr/bin/emacs'
P4PASSWD='p4sucks'
P4PORT='achilles:1667'
#P4PORT='grace.palmone.com:1667'
P4USER='MattBisson'

export P4CLIENT P4EDITOR P4PASSWD P4PORT P4USER
