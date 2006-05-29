#!/bin/tcsh -x
# .hollyrc.csh
###############################################################################
# Sets up the Holly-specific crapola
###############################################################################
# Matt Bisson

# This is available for convenience...
setenv USE_HOLLY 1

##############################################################################
# Set up my 'simulator's environment
##############################################################################

# Hollywood tools crap
#setenv HOLLY_ARCH       "arm-linux"
setenv HOLLY_ARCH        "i386-linux"
setenv HOLLY_DEBREL      "debug"
setenv HOLLY_RESULT_ROOT "${HOME}/tmp/toilet/${HOLLY_ARCH}"
setenv HOLLY_SYSTEM_ROOT "/opt/holly/sysroot/${HOLLY_ARCH}"
setenv HOLLY_TOOL_ROOT   "/opt/holly/toolroot"

# Hollywood source crap
setenv HOLLY_DEV_BRANCH  "1.0/Dev"
setenv HOLLY_DIR         "${HOME}/ws/mbisson-holly"
setenv HOLLY_BASE        "${HOLLY_DIR}/Source/Platform/Holly/${HOLLY_DEV_BRANCH}"

# Perforce crap
if ( -f "${HOME}/.p4rc.csh" ) then
    source "${HOME}/.p4rc.csh"
endif

# PBS crap
#if ( -f "${HOME}/PBS/.pbsInit-csh" ) then
#    source "${HOME}/PBS/.pbsInit-csh"
#endif

setenv MALLOC_CHECK_ 2
