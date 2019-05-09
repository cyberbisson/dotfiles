# ===-- .gdbinit -------------------------------------- -*- gdb-script -*- --===
################################################################################
# This is my GDB initialization file.  It is very generic.  Any GDB settings
# specific to a particular development environment should be included at the end
# of this file so that it may override any settings here.
################################################################################
# Matt Bisson

# Just in case someone changes this on us, and we forget to prefix a number with
# "0d", make sure the input radix is 10 for the configuration file.
set input-radix 0d10

# ----------------------------------------
# General settings:
# ----------------------------------------

# Allows us to use Emacs keybindings at the prompt.
set editing on

# By default, choose to follow the parent process instead of the child.
set follow-fork-mode parent

# THIS BREAKS AUTO-COMPLETE FOR EMACS 22.  TODO: It should be enough to check
# for MI as the interpreter.
if 0
  set prompt [gdb]
end

# Step into assembly when debugging info is not present (instead of skipping
# over).
set step-mode on

# Of course unwind C++ exceptions before std::terminate.
set unwind-on-terminating-exception on

# Don't write to core files.  That's just weird.
set write off

# Really, I have no idea what this means.
set verbose off

# ----------------------------------------
# Shared library stuff:
# ----------------------------------------
set auto-solib-add       on
set complaints           1
#set libthread-db-search-path ""
set stop-on-solib-events 0d0

# ----------------------------------------
# History configurations:
# ----------------------------------------
set history expansion on
set history save      on
set history size      0d256

# ----------------------------------------
# Logging configuration:
# ----------------------------------------
set logging overwrite off
set logging redirect  off

# Only do this if the home directory is not "/".  This is particularly important
# for "auto-load safe-path", which would allow anything to be considered "safe"
# if we are "root" or have no home directory.  TODO:
if 1
  set auto-load safe-path ~/
  set history filename    ~/.gdb_history
  set logging file        ~/tmp/gdb.log
  set remotelogfile       ~/tmp/gdb-remote.log
end

# ------------------------------------------------------------------------------
# Output Control:
# ------------------------------------------------------------------------------

set disassemble-next-line  auto
set disassembly-flavor     intel
set exec-done-display      on
# Skip the paging except after a larger number of lines.
set height                 0d1024
set listsize               0d10
set max-user-call-depth    0d256
set opaque-type-resolution on
set output-radix           0d10
set overload-resolution    on
set pagination             on

# ----------------------------------------
# Backtrace display:
# ----------------------------------------
set backtrace limit      0d256
set backtrace past-entry on
set backtrace past-main  on

# ----------------------------------------
# Data printouts:
# ----------------------------------------
set print address               on
set print array                 on
set print array-indexes         on
set print asm-demangle          on
set print demangle              on
set print elements              0d128
set print entry-values          both
set print frame-arguments       scalar
set print inferior-events       on
set print max-symbolic-offset   0d0
set print null-stop             on
set print object                on
set print pascal_static-members on
set print pretty                on
set print raw frame-arguments   on
set print repeats               0d16
set print sevenbit-strings      on
set print static-members        on
set print symbol                on
set print symbol-filename       on
set print symbol-loading        full
set print thread-events         on
set print type methods          on
set print type typedefs         on
set print union                 on
set print vtbl                  on

# ------------------------------------------------------------------------------
# Remote Debugging:
# ------------------------------------------------------------------------------

set tcp auto-retry          on
set tcp connect-timeout     0d15
set trust-readonly-sections off
set watchdog                0

# ------------------------------------------------------------------------------
# Additional Configurations:
# ------------------------------------------------------------------------------
