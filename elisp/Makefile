# Matt Bisson - 6/29/2019
#
# BSD-Make specifics to build Emacs Lisp packages.

################################################################################
# Utilities:
################################################################################

MACOS_EMACS = /Applications/Emacs.app/Contents/MacOS/Emacs
.if exists("$(MACOS_EMACS)")
    EMACS = "$(MACOS_EMACS)"
.else
    EMACS = emacs
.endif

RM = rm

# Load the common recipes.
.include "common.mk"
