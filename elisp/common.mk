# Matt Bisson - 6/29/2019
#
# Builds Emacs Lisp files into .elc (compiled) outputs.  This is independent of
# the GNUmake vs. BSD Make differences.

################################################################################
# Make recipes:
################################################################################

ELISP_FILES = \
    clang-format.el \
    dos.el \
    emacs.el \
    gtags.el \
    markdown-mode.el \
    media-wiki.el \
    mud.el \
    output-org.el \
    p4.el \
    powershell.el \
    sql.el \
    undo-tree.el \
    visual-basic-mode.el \
    vmw-c-dev.el \
    xcscope.el \
    xgtags.el
ELISP_OUTPUTS = $(ELISP_FILES:.el=.elc)


# Meta-targets...
.PHONY: all clean

# Clear out any suffix we're not using to avoid accidents, then re-set.
.SUFFIXES:
.SUFFIXES: .el .elc

# By default, just build the emacs.el file.
emacs.elc: emacs.el

# WARNING: Not all the files actually compile (p4.el?) in batch mode...
all: $(ELISP_OUTPUTS)
	$(EMACS) --batch -f batch-byte-compile $(ELISP_FILES)

clean:
	$(RM) -f $(ELISP_OUTPUTS)

.el.elc:
	$(EMACS) --batch -f batch-byte-compile $<
