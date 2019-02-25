#!/bin/zsh -x
# use_devprofile.zsh
################################################################################
# This file should be sourced into the current shell.  It accepts input as the
# $VMWARE_PROFILE environment variable, and configures things like source code
# and editor settings.
#
# This works by having a file in $HOME that matches ".zshrc.$VMWARE_PROFILE".
# This file should have the following settings:
#
# - VMWARE_SRCDIR: The full path to the directory that is the top of the VMware
#   source tree.  The script assumes the build trees and other variables needed
#   for the build environment live beneath this directory.
#
# - VMWARE_PROFILE_TITLE (optional): This is a nice display name that will be
#   shown on the window title if the given terminal emulator supports changing
#   it.
#
################################################################################
# Matt Bisson

# TODO: Where should this function really go??  I have to source it instead of
# just running a command, which is annoying.  Perhaps an alias...

if [ ${+VMWARE_PROFILE} -eq 0 ] ; then
    echo "$0: Must specify VMWARE_PROFILE prior to running." > /dev/stderr
    return 1
fi

# If we find a profile, load it.  Otherwise, fail, and keep the environment like
# it is.
if [ -f "${HOME}/.zshrc.${VMWARE_PROFILE}" ] ; then
    unset VMWARE_PROFILE_TITLE
    unset VMWARE_SRCDIR
    source "${HOME}/.zshrc.${VMWARE_PROFILE}"
else
    echo "$0: No loadable profile for '${VMWARE_PROFILE}'." > /dev/stderr
    return 2
fi

if [ ${+VMWARE_SRCDIR} -eq 0 ] ; then
    echo "$0: Must specify VMWARE_SRCDIR prior to running." > /dev/stderr
    return 1
fi

# Probably we can do without this variable, but it makes tweaking editors easier
# when not using a profile...
export VMWARE_CODE=1

# We assume that there's both a Git and Perforce source directory.
export VMWARE_SRCDIR_GIT="${VMWARE_SRCDIR}.gf"
export VMWARE_SRCDIR_P4="${VMWARE_SRCDIR}.p4"

# Set up Emacs.
export EMACS_DESKTOP_DIR="${VMWARE_SRCDIR}"
export EMACS_SERVER_FILE="${VMWARE_PROFILE}"

# Set up crappy crappy Perforce.
export P4CONFIG="${HOME}/.p4config.${VMWARE_PROFILE}"

# Prevent it from asking annoying questions or loading up a desktop when
# just making a git-commit message or something...
export EDITOR="emacsclient -s ${EMACS_SERVER_FILE} -a 'emacs --no-desktop'"
export VISUAL="emacsclient -s ${EMACS_SERVER_FILE} -a 'emacs --no-desktop'"

# Useful aliases.
alias cdsrc='cd ${VMWARE_SRCDIR}'
alias emacsc='"emacsclient" -s ${EMACS_SERVER_FILE}'
alias emacss='"emacs" --instance-id ${VMWARE_PROFILE} -f server-start'

# If we can, set the window title.  Note that the window title will not return
# to normal after an SSH session or something.
if [ ${+VMWARE_PROFILE_TITLE} -ne 0 ] ; then
    # Set the Konsole title.  If we have used SSH, these DBUS settings will not
    # pertain to the local terminal, and will do the wrong thing.  Skip this
    # step on SSH!
    if [ ${+SSH_CLIENT} -eq 0 ] && [ ${+KONSOLE_DBUS_SESSION} -ne 0 ] && \
       [ ${+KONSOLE_DBUS_SERVICE} -ne 0 ] ; then
        qdbus \
            $KONSOLE_DBUS_SERVICE $KONSOLE_DBUS_SESSION \
            org.kde.konsole.Session.setTitle 1 "${VMWARE_PROFILE_TITLE}" \
            > /dev/null
    else
        # If this is a remote session, we can also indicate that in the title.
        if [ ${+SSH_CLIENT} -ne 0 ] ; then
            vmware_profile_title="${VMWARE_PROFILE_TITLE} - ${USER} @ ${HOST}"
        else
            vmware_profile_title="${VMWARE_PROFILE_TITLE}"
        fi

        # Certain terminal types can set the title like this.  It may override
        # this with subsequent commands, depending on settings, though.
        case ${TERM} in
            xterm* | rxvt*)
                echo "\033]0;${vmware_profile_title}\007"
                ;;
        esac
        unset vmware_profile_title
    fi
fi

# Change the directory if we're not already there.
if [ -d "${VMWARE_SRCDIR}" ] ; then
     if [ `pwd` != "${VMWARE_SRCDIR}" ] ; then
         echo "Moving to source tree: ${VMWARE_SRCDIR}"
         cd "${VMWARE_SRCDIR}"
     fi
else
    echo "Source tree \"${VMWARE_SRCDIR}\" does not exist; profile set to" \
         "\"${VMWARE_PROFILE}\"..." > /dev/stderr
fi
