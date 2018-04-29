#!/bin/sh
################################################################################
# This file sets up default window behavior for Mac OS.  Run this script once to
# set up the environment.
################################################################################
# Matt Bisson			11/12/2017

defaults delete org.gnu.Emacs

# Disable this behavior that doesn't seem to work in Emacs 25.3.
defaults write org.gnu.Emacs ApplePressAndHoldEnabled NO

defaults write org.gnu.Emacs Background gray20
#defaults write org.gnu.Emacs Background "#D8D0C8"
defaults write org.gnu.Emacs Font "Menlo 12"
defaults write org.gnu.Emacs Foreground gray81
#defaults write org.gnu.Emacs Foreground black
defaults write org.gnu.Emacs ToolBar 0

# This doesn't work -- I don't know why.  The invocation of (x-parse-geometry
# (x-get-resource "geometry" "Geometry")) as used in faces.el returns what we
# expect, but it has no effect.
defaults write org.gnu.Emacs Geometry "0 0 50 81"
