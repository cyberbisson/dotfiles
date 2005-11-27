# .gdbinit
# shortcut for the target remote command. Type 'tarr' at the gdb
# command line to issue the 'target remote ...' command.

# Load up the HollyMail executable
def HollyMail
	file /home/mbisson/ws/mbisson/holly/device/MM/HollyMail
end

# Load up the TestHarness
def TestHarness
	file /home/mbisson/ws/mbisson/holly/device/Octane/Examples/TestHarness/TestHarness
end

# Connect to the device
def tarr
    target remote holly:7777
end

# Show me everything
def dumpenv
	show annotate
	show architecture
	show args
	show auto-solib-add
	show backtrace
	show breakpoint
	show can-use-hw-watchpoints
	show case-sensitive
	show charset
	show check
	show coerce-float-to-double
	show commands
	show complaints
	show confirm
	show convenience
	show copying
	show cp-abi
	show debug
	show debug-file-directory
	show debugvarobj
	show demangle-style
	show directories
	show disassembly-flavor
	show download-write-size
	show editing
	show endian
	show environment
	show exec-done-display
	show extension-language
	show follow-fork-mode
	show gnutarget
	show height
	show history
	show host-charset
	show inferior-tty
	show input-radix
	show language
	show listsize
	show logging
	show max-user-call-depth
	show opaque-type-resolution
	show os
	show osabi
	show output-radix
	show overload-resolution
	show pagination
	show paths
	show print
	show prompt
	show radix
	show remote
	show remoteaddresssize
	show remotebaud
	show remotebreak
	show remotecache
	show remotedevice
	show remotelogbase
	show remotelogfile
	show remotetimeout
	show remotewritesize
	show scheduler-locking
	show serial
	show solib-absolute-prefix
	show solib-search-path
	show step-mode
	show stop-on-solib-events
	show struct-convention
	show symbol-reloading
	show target-charset
	show trust-readonly-sections
	show tui
	show unwindonsignal
	show user
	show values
	show verbose
	show version
	show warranty
	show watchdog
	show width
	show write
end

# Signals
handle SIG32 nostop

# Misc. settings
set annotate                1
set architecture            auto
set breakpoint              pending auto
set can-use-hw-watchpoints  1
set case-sensitive          auto
set charset                 ISO-8859-1
set coerce-float-to-double  on
set complaints              0
set confirm                 off
set cp-abi                  auto
set demangle-style          auto
set disassembly-flavor      intel
set download-write-size     0d512
set editing                 on
set endian                  auto
set exec-done-display       off
set follow-fork-mode        parent
set height                  0
set host-charset            ISO-8859-1
set input-radix             0d10
set language                auto
set listsize                0d10
set max-user-call-depth     0d1024
set opaque-type-resolution  on
set output-radix            0d10
set overload-resolution     on
set pagination              on
set prompt                  (dbx) 
set struct-convention       default
set symbol-reloading        off
set target-charset          ISO-8859-1
set trust-readonly-sections off
set unwindonsignal          off
set verbose                 on
set watchdog                0
set width                   0
set write                   off

# Backtrace
set backtrace  limit      0
set backtrace  past-main  off

# Runtime checks
set check range auto
set check type  auto

# Debug file
set debug                arch       0
set debug                event      0
set debug                expression 0
set debug                frame      0
#set debug               infrun     0
set debug                lin-lwp    0
set debug                observer   0
set debug                overload   1
set debug                remote     0
set debug                serial     0
set debug                target     0
set debug-file-directory /opt/holly/toolroot/lib/debug
set debugvarobj          0

# History
set history expansion off
set history filename  /home/mbisson/ws/mbisson/.gdb_history
set history save      on
set history size      0d256

# Log crapola
set logging file      gdb.log
set logging overwrite off
set logging redirect  off

# Display crap
set print address               on
set print array                 on
set print array                 on
set print asm-demangle          on
set print demangle              on
set print elements              0d200
set print max-symbolic-offset   0
set print null-stop             off
set print object                on
set print pascal_static-members on
set print pretty                on
set print repeats               0d10
set print sevenbit-strings      off
set print static-members        on
set print symbol-filename       on
set print union                 on
set print vtbl                  on

# Remote debugging mumbo-jumbo
set remote            binary-download-packet                  auto
set remote            fetch-register-packet                   auto
set remote            hardware-breakpoint-limit               0d4294967295
set remote            hardware-breakpoint-packet              auto
set remote            hardware-watchpoint-limit               0d4294967295
set remote            memory-read-packet-size                 0
set remote            memory-write-packet-size                0
set remote            p-packet                                auto
set remote            P-packet                                auto
set remote            read-aux-vector-packet                  auto
set remote            read-watchpoint-packet                  auto
set remote            set-register-packet                     auto
set remote            software-breakpoint-packet              auto
set remote            symbol-lookup-packet                    auto
#set remote           system-call-allowed                     not allowed
set remote            verbose-resume-packet                   auto
set remote            write-watchpoint-packet                 auto
set remote            X-packet                                auto
set remoteaddresssize 0d32
set remotebaud        0d4294967295
set remotebreak       off
set remotecache       off
set remotelogbase     ascii
set remotetimeout     0d2

# Shared Library stuff
set auto-solib-add         on
#set solib-absolute-prefix /opt/holly/sysroot/arm-linux
#set solib-absolute-prefix /opt/holly/sysroot/i386-linux
set solib-search-path      /opt/holly/sysroot/i386-linux/lib:/opt/holly/sysroot/i386-linux/usr/lib
set step-mode              off
set stop-on-solib-events   0

# TUI
set tui active-border-mode bold-standout
set tui border-kind        acs
set tui border-mode        normal

# The HollyMail sandbox
dir /home/mbisson/ws/mbisson/holly:/home/mbisson/ws/mbisson/holly/Common/SDK:/home/mbisson/ws/mbisson/holly/Common/SDK/Holly:/home/mbisson/ws/mbisson/holly/device/MMCommon:/home/mbisson/ws/mbisson/holly/device/Octane/Examples/TestHarness:/home/mbisson/ws/mbisson/holly/device/Octane/:/home/mbisson/ws/mbisson/holly/device/Octane/FrameWork:/home/mbisson/ws/mbisson/holly/device/Octane/FrameWork/Util:/home/mbisson/ws/mbisson/holly/device/Octane/FrameWork/Util/Objecteering:/home/mbisson/ws/mbisson/holly/device/Octane/FrameWork/Util/Platform/Holly:/home/mbisson/ws/mbisson/holly/device/Octane/FrameWork/Environment:/home/mbisson/ws/mbisson/holly/device/Octane/FrameWork/Environment/Platform/Holly:/home/mbisson/ws/mbisson/holly/device/Octane/FrameWork/View:/home/mbisson/ws/mbisson/holly/device/Octane/FrameWork/View/Platform/Holly:/home/mbisson/ws/mbisson/holly/device/Octane/FrameWork/View/Util:/home/mbisson/ws/mbisson/holly/device/Octane/FrameWork/Model:/home/mbisson/ws/mbisson/holly/device/Octane/FrameWork/Controller:/home/mbisson/ws/mbisson/holly/device/Octane/Examples/TestHarness/Source:/home/mbisson/ws/mbisson/holly/device/Octane/Examples/TestHarness/Source/TableSubclasses:/opt/holly/sysroot/i386-linux/usr/include:/home/mbisson/ws/mbisson/holly/device/MM/Src:/home/mbisson/ws/mbisson/holly/device/MM/Src-OOP:/home/mbisson/ws/mbisson/holly/device/MM/Src-OOP/VMModel:/home/mbisson/ws/mbisson/holly/device/MM/Src-OOP/VMView:/home/mbisson/ws/mbisson/holly/device/MM/Src-OOP/VMController:/opt/holly/sysroot/i386-linux/usr/include/directfb
# The mainline...
#dir /home/mbisson/ws/mbisson/main:/home/mbisson/ws/mbisson/main/Common/SDK:/home/mbisson/ws/mbisson/main/Common/SDK/Holly:/home/mbisson/ws/mbisson/main/device/MMCommon:/home/mbisson/ws/mbisson/main/device/Octane/Examples/TestHarness:/home/mbisson/ws/mbisson/main/device/Octane/:/home/mbisson/ws/mbisson/main/device/Octane/FrameWork:/home/mbisson/ws/mbisson/main/device/Octane/FrameWork/Util:/home/mbisson/ws/mbisson/main/device/Octane/FrameWork/Util/Objecteering:/home/mbisson/ws/mbisson/main/device/Octane/FrameWork/Util/Platform/Holly:/home/mbisson/ws/mbisson/main/device/Octane/FrameWork/Environment:/home/mbisson/ws/mbisson/main/device/Octane/FrameWork/Environment/Platform/Holly:/home/mbisson/ws/mbisson/main/device/Octane/FrameWork/View:/home/mbisson/ws/mbisson/main/device/Octane/FrameWork/View/Platform/Holly:/home/mbisson/ws/mbisson/main/device/Octane/FrameWork/View/Util:/home/mbisson/ws/mbisson/main/device/Octane/FrameWork/Model:/home/mbisson/ws/mbisson/main/device/Octane/FrameWork/Controller:/home/mbisson/ws/mbisson/main/device/Octane/Examples/TestHarness/Source:/home/mbisson/ws/mbisson/main/device/Octane/Examples/TestHarness/Source/TableSubclasses:/opt/holly/sysroot/i386-linux/usr/include:/opt/holly/sysroot/i386-linux/usr/include/directfb
# Darsono's sandbox...
#dir /home/mbisson/ws/sandbox/MultiMail:/home/mbisson/ws/sandbox/MultiMail/Common/SDK:/home/mbisson/ws/sandbox/MultiMail/Common/SDK/Holly:/home/mbisson/ws/sandbox/MultiMail/device/MMCommon:/home/mbisson/ws/sandbox/MultiMail/device/Octane/Examples/TestHarness:/home/mbisson/ws/sandbox/MultiMail/device/Octane/:/home/mbisson/ws/sandbox/MultiMail/device/Octane/FrameWork:/home/mbisson/ws/sandbox/MultiMail/device/Octane/FrameWork/Util:/home/mbisson/ws/sandbox/MultiMail/device/Octane/FrameWork/Util/Objecteering:/home/mbisson/ws/sandbox/MultiMail/device/Octane/FrameWork/Util/Platform/Holly:/home/mbisson/ws/sandbox/MultiMail/device/Octane/FrameWork/Environment:/home/mbisson/ws/sandbox/MultiMail/device/Octane/FrameWork/Environment/Platform/Holly:/home/mbisson/ws/sandbox/MultiMail/device/Octane/FrameWork/View:/home/mbisson/ws/sandbox/MultiMail/device/Octane/FrameWork/View/Platform/Holly:/home/mbisson/ws/sandbox/MultiMail/device/Octane/FrameWork/View/Util:/home/mbisson/ws/sandbox/MultiMail/device/Octane/FrameWork/Model:/home/mbisson/ws/sandbox/MultiMail/device/Octane/FrameWork/Controller:/home/mbisson/ws/sandbox/MultiMail/device/Octane/Examples/TestHarness/Source:/home/mbisson/ws/sandbox/MultiMail/device/Octane/Examples/TestHarness/Source/TableSubclasses:/opt/holly/sysroot/i386-linux/usr/include:/opt/holly/sysroot/i386-linux/usr/include/directfb
