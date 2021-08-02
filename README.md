# Matt Bisson's Dotfiles

You have reached the home of Matt Bisson's dotfiles.  This repository has been carried forth from a very long time ago, and has been actively maintained and improved over the years.  This repo includes sub-environments from various projects and organizations where I've worked over the years.  Primarily, check out this repo for configurations of various shells on various OSes, and lots Emacs configuration.

## Basic Philosophy

Philosophically, I will say that the dotfiles I create are geared more toward "harmonizing" different environments than they are to making short-cuts.  For example, I will avoid creating aliases like `ll` (for `ls -l`) both because it isn't that interesting to me, and because I don't want features of my personal environment to slip into instructions for other people as to how to do X, Y, and Z.  This also implies that I will _not_ be making Windows behave like a *NIX shell, or vice-versa.  For Emacs, this sometimes means _reverting_ certain things in new versions so it behaves "like it always did."  For example, I still prefer that `Home` and `End` jump to the beginning and end of the buffer, instead of the beginning and end of the line.

I also do net set a big fancy prompt.  It honestly stresses me out to see the count of history items go upward with every command, and including the path tends to waste space (in my opinion), when I generally know where I am.  Furthermore, I do everything out of Emacs, so the mode-line shows me things like time, CPU usage, and battery.  If you are looking for Script-Fu in the shell prompt, these aren't the scripts you're looking for, unfortunately.

## Shell Support

One of the major chunks of code here is shell initialization support.  The scripts set up the following shell types with equal functionality (except where noted):

- Zsh (generally my primary shell of choice).
- Tcsh (my primary shell of choice for years, until I settled on Zsh).
- Bourne Again Shell (Bash).
- Korn Shell (Ksh).
- C Shell (Csh)
- Bourne Shell -- the "original" UNIX System V shell.  Since this shell does not support `alias`, I have scripted up some functions to do the same work as the above shells, but only where the alias was critically important.
- There is also a Windows CMD log-in script, but it's vastly different from these other *NIX shell scripts.

I have run these shells on _all_ the following OS types:

- UNIX (Solaris, HP-UX, SGI Irix, IBM AIX, Digital / Compaq UNIX, SCO UnixWare, SINIX, UNIX System V).
- Linux (Gentoo, RedHat, Fedora, CentOS, SUSE, Ubuntu, Mint)
- BSD (FreeBSD, OpenBSD)
- MacOS
- Windows (Cygwin and `CMD.EXE`)

Generally speaking, for all shell types, there is a system-wide script (e.g., `.profile`), an interactive shell startup (e.g., `.shrc`), and I have placed aliases in their own script (e.g., `.alias.sh`).  To make everything correct, create the following symlinks in your home directory from this repository (for Windows, prefer `mklink`).

 Source      | Sh          | Ksh          | Bash            | Zsh          | Csh          | Tcsh
-------------|-------------|--------------|-----------------|--------------|--------------|------------
 `alias.zsh` | -           | -            | -               | `.alias.zsh` | -            | -
 `zprofile`  | -           | -            | -               | `.zprofile`  | -            | -
 `zshrc`     | -           | -            | -               | `.zshrc`     | -            | -
 `alias.sh`  | `.alias.sh` | -            | -               | -            | -            | -
 `alias.ksh` | -           | `.alias.ksh` | `.alias.ksh`    | -            | -            | -
 `profile`   | `.profile`  | `.profile`   | `.bash_profile` | -            | -            | -
 `kshrc`     | `.shrc`     | `.kshrc`     | `.bashrc`       | -            | -            | -
 `alias.csh` | -           | -            | -               | -            | `.alias.csh` | `.alias.csh`
 `login`     | -           | -            | -               | -            | `.login`     | `.login`
 `tcshrc`    | -           | -            | -               | -            | `.cshrc`     | `.tcshrc`

## Emacs

This repository (under `elisp/`) contains my Emacs configuration.  This has been tested against every version of GNU Emacs since 19.34, and the XEmacs versions 19 and 20.  It works on the usual suspects: *NIX, Windows, and MacOS.  This configuration exists almost entirely within a single `emacs.el` file.  Check out the header of this file for details, but it contains a number of features for my typical usage patters, like Emacs as a daemon, `destkop-mode`, custom syntax highlighting for both light and dark terminals (including different clients on the same Emacs instance), and so on.  All Emacs files can be byte-compiled using the `make` command from that directory.  By default `emacs.el` compiles to `emacs.elc`, which can be linked into `~/.emacs.elc`.

There is a small collection of external Emacs modes for systems where I'm not the administrator (of limited usefulness).  To use them, they should be compiled (with the `make x.elc` command), and linked into `~/elisp/`.

## Miscellaneous

Most files (and some directories) in the top level directory are intended to be symlinked into the home directory, prefixed with a dot (e.g., `ln -s mpd "$HOME/.mpd"`).  There are "X Resources" configurations, which for *NIX are obviously meant to be linked, but for Windows and MacOS they are meant to run as a one-time configuration script with the appropriate settings.

The directory `envs/` contains various project-specific configurations.  They are probably not relevant any more...

## Bugs

_Warning:_ A few files assume that the dotfiles are kept in this repository, with a path of `$HOME/sb/dotfiles`.  This is mostly because I was avoiding some clutter in the top-level of my home directory.  Beware of `.Xdefaults` and `use_devprofile.*` expecting to look directly into the repository.
