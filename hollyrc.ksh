#!/bin/ksh -x
# .hollyrc.ksh
###############################################################################
# Sets up the Holly-specific crapola
###############################################################################
# Matt Bisson

# This is available for convenience...
USE_HOLLY=1; export USE_HOLLY

##############################################################################
# Set up my 'simulator's environment
##############################################################################

# Hollywood tools crap
#HOLLY_ARCH="arm-linux"
HOLLY_ARCH="i386-linux"
#HOLLY_DEBREL="release"
HOLLY_DEBREL="debug"
HOLLY_RESULT_ROOT="${HOME}/tmp/toilet/${HOLLY_ARCH}"
HOLLY_SYSTEM_ROOT="/opt/holly/sysroot/${HOLLY_ARCH}"
HOLLY_TOOL_ROOT="/opt/holly/toolroot"
export HOLLY_ARCH HOLLY_DEBREL HOLLY_RESULT_ROOT HOLLY_SYSTEM_ROOT
export HOLLY_TOOL_ROOT

# Hollywood source crap
HOLLY_DEV_BRANCH="1.0/Dev"
HOLLY_DIR="${HOME}/ws/mbisson-holly"
HOLLY_BASE="${HOLLY_DIR}/Source/Platform/Holly/${HOLLY_DEV_BRANCH}"
export HOLLY_DEV_BRANCH HOLLY_DIR HOLLY_BASE

# Skip all the fucking shit.
HOLLY_MAIL_ONLY=1
export HOLLY_MAIL_ONLY

# Perforce crap
if [ -f "${HOME}/.p4rc.ksh" ] ; then
    . "${HOME}/.p4rc.ksh"
fi

# PBS crap
#if [ -f "${HOME}/PBS/.pbsInit-bash" ] ; then
#    . "${HOME}/PBS/.pbsInit-bash"
#fi

MALLOC_CHECK_=2; export MALLOC_CHECK_
