<#
.SYNOPSIS
Set up Matt Bisson's working environment.

.DESCRIPTION

This script creates directories for storage, configures Emacs and Cygwin (if
found), as well as Windows and *NIX command environments.  Note that the script
is idempotent, so it can run again and again safely.  It may overwrite manual
settings, however, that were made after an earlier configuration.

.PARAMETER HomeDir
The target location where most configuration lands.  By default, this is
"%USERPROFILE%".

.PARAMETER DotfileRepo
Matt Bisson keeps his dot-files in a Git repository.  This parameter indicates
where the repository exists.  By default, this is "$HomeDir\sb\dotfiles".

.PARAMETER EmacsVersion
The version of GNU Emacs that will be used.

.PARAMETER EmacsDir
The full path to the Emacs instance, referenced by $EmacsVersion.

.NOTES

You need to run this (in an administrative PowerShell), or Windows complains
about there not being a signature in the script:

> Set-ExecutionPolicy RemoteSigned

You can, of course, revert it to "AllSigned" (or whatever Get-ExecutionPolicy
reports) when you're done.
#>

# TODO:
# - Error paths.
# - Refactor shortcut functions
# - Set up Cygwin (separate script) to have ZSH as my shell, and to create
#   /etc/password, et al.
# - Separate MS set-up script.

param (
    [string] $HomeDir = $env:USERPROFILE,
    [string] $DotfileRepo = "$HomeDir\sb\dotfiles",
    [string] $EmacsVersion = "29.4",
    [string] $EmacsDir = "C:\Program Files\Emacs\emacs-$EmacsVersion"
)


<#
.SYNOPSIS
Generate a script to run at the start of all CMD.EXE invocations.

.DESCRIPTION

This function generates a script with DOSKEY aliases, PROMPT settings, and so
on.  The script will be invoked by CMD.EXE as an "autorun", which the function
adds to the registry.

.PARAMETER ScriptName
Specify the path and name of the login script to generate.
#>
function Add-CmdLoginScript {
    param (
        [Parameter(Mandatory=$true)] [string] $ScriptName
    )
    @'
@ECHO OFF

REM WARNING !! This script is generated; modifications may be lost !!

REM Show my version thingy
VER

REM "(me@mymachine):directory> "
PROMPT $C%COMPUTERNAME%@%USERNAME%$F:$P$G 

DOSKEY /INSERT
DOSKEY emacs=runemacs.exe $*
DOSKEY emacsc=emacsclientw.exe -c -n $*
DOSKEY emacsd=runemacs.exe --force-high-dpi --daemon $*
'@ | Out-File -Encoding ASCII $ScriptName

    if (-not (Test-Path -Path "HKCU:\Software\Microsoft\Command Processor" `
                        -PathType Container)) {
        New-Item -Path "HKCU:\Software\Microsoft" `
                 -Name "Command Processor" -Force
    }

    Set-ItemProperty -Force -Path "HKCU:\Software\Microsoft\Command Processor" `
      -Name "Autorun" -Value "$ScriptName"
}


<#
.SYNOPSIS
Add a Windows shortcut for Emacs Client to the Desktop.

.DESCRIPTION

The function adds a shortcut for Emacs as part of a client/server.  The
Client/Server shortcut will start Emacs as a server (not daemon), if the client
fails to connect.

.PARAMETER EmacsVersion
The version of Emacs being used.  The is mostly for the shortcut name.

.Parameter EmacsDir
The location of the Emacs installation.
#>
function Add-EmacsClientShortcut {
    param (
        [Parameter(Mandatory=$true)] [string] $EmacsVersion,
        [Parameter(Mandatory=$true)] [string] $EmacsDir
    )

    Write-Output "Creating Emacs Client ($EmacsVersion) shortcut."

    $ShellObj = New-Object -comObject WScript.Shell
    $DesktopDir = $ShellObj.SpecialFolders("Desktop")

    $EmacsClientLnk = $ShellObj.CreateShortcut(
        "$DesktopDir\Emacs Client ($EmacsVersion).lnk")
    $EmacsClientLnk.TargetPath = "$EmacsDir\bin\emacsclientw.exe"
    # NOTE: Can't seem to get emacsclient to use a full path for "-a".
    $EmacsClientLnk.Arguments = `
      "-c -n -a ""runemacs -f server-start"""
    $EmacsClientLnk.WorkingDirectory = "%HOME%"
    $EmacsClientLnk.Description = `
      "Create a new client frame in a running Emacs instance."

    $EmacsClientLnk.Save()
}


<#
.SYNOPSIS
Add a Windows shortcut for Emacs to the Desktop.

.DESCRIPTION

The function adds a shortcut for stand-alone Emacs

.PARAMETER EmacsVersion
The version of Emacs being used.  The is mostly for the shortcut name.

.Parameter EmacsDir
The location of the Emacs installation.
#>
function Add-EmacsShortcut {
    param (
        [Parameter(Mandatory=$true)] [string] $EmacsVersion,
        [Parameter(Mandatory=$true)] [string] $EmacsDir
    )

    Write-Output "Creating Emacs ($EmacsVersion) shortcut."

    $ShellObj = New-Object -comObject WScript.Shell
    $DesktopDir = $ShellObj.SpecialFolders("Desktop")

    $EmacsClientLnk = $ShellObj.CreateShortcut(
        "$DesktopDir\Emacs $EmacsVersion (Stand-alone).lnk")
    $EmacsClientLnk.TargetPath = "$EmacsDir\bin\runemacs.exe"
    $EmacsClientLnk.WorkingDirectory = "%HOME%"
    $EmacsClientLnk.Description = "Create a new isolated Emacs instance."

    $EmacsClientLnk.Save()
}


<#
.SYNOPSIS
Create the initial set of directories that I expect in my "home" directory.

.PARAMETER HomeDir
The location where the function creates new directories.
#>
function Add-InitialDirStructure {
    param (
        [Parameter(Mandatory=$true)] [string] $HomeDir
    )

    New-Item -Force -ItemType Directory -Path "$HomeDir" -Name "bin"
    New-Item -Force -ItemType Directory -Path "$HomeDir" -Name "doc"
    New-Item -Force -ItemType Directory -Path "$HomeDir" -Name "elisp"
    New-Item -Force -ItemType Directory -Path "$HomeDir" -Name "save"
    New-Item -Force -ItemType Directory -Path "$HomeDir" -Name "sb"
    New-Item -Force -ItemType Directory -Path "$HomeDir" -Name "src"
    New-Item -Force -ItemType Directory -Path "$HomeDir" -Name "tmp"
}


<#
.SYNOPSIS
Create symbolic links to version-controlled profile/environment scripts.

.DESCRIPTION

I keep my profile scripts in a Git repository.  Rather than copy them all over,
simply create links.  Of course, for some bizarre reason, Windows requires
administrative rights to make symbolic links...

.PARAMETER HomeDir
The directory where the links will be created.

.PARAMETER Repo
The location of my dotfile Git repository.

.PARAMETER EmacsSiteLispDir
Specifies where the Cygwin Emacs site-lisp directory is.  Presumably, there is
some package installed as part of Cygwin that has a more up-to-date version than
what's in my dotfile repository, so bring that into Windows Emacs.  TODO: This
doesn't need to be hard-coded.
#>
function Add-ProfileSymlinks {
    param (
        [Parameter(Mandatory=$true)] [string] $HomeDir,
        [Parameter(Mandatory=$true)] [string] $Repo,
        [string] $EmacsSiteLispDir = "C:\cygwin64\usr\share\emacs\site-lisp"
    )

    Push-Location -Path $HomeDir

    # Make symbolic links for all the *NIX dotfiles.
    New-Item -Force -ItemType SymbolicLink `
      -Name .alias.zsh -Target $Repo\alias.zsh
    New-Item -Force -ItemType SymbolicLink `
      -Name .emacs.el  -Target $Repo\elisp\emacs.el
    New-Item -Force -ItemType SymbolicLink `
      -Name .minttyrc  -Target $Repo\minttyrc
    New-Item -Force -ItemType SymbolicLink `
      -Name .zprofile  -Target $Repo\zprofile
    New-Item -Force -ItemType SymbolicLink `
      -Name .zshrc     -Target $Repo\zshrc

    # Now, bring over the Emacs packages from the Git repo.
    New-Item -Force -ItemType SymbolicLink `
      -Name .emacs.el              -Target $Repo\elisp\emacs.el
    New-Item -Force -ItemType SymbolicLink `
      -Name elisp\dos.el           -Target $Repo\elisp\dos.el
    New-Item -Force -ItemType SymbolicLink `
      -Name elisp\markdown-mode.el -Target $Repo\elisp\markdown-mode.el
    New-Item -Force -ItemType SymbolicLink `
      -Name elisp\powershell.el    -Target $Repo\elisp\powershell.el

    # If the packages are compiled, bring those in, otherwise warn.
    New-OptSymbolicLink `
      -Name .emacs.elc              -Target $Repo\elisp\emacs.elc
    New-OptSymbolicLink `
      -Name elisp\dos.elc           -Target $Repo\elisp\dos.elc
    New-OptSymbolicLink `
      -Name elisp\markdown-mode.elc -Target $Repo\elisp\markdown-mode.elc
    New-OptSymbolicLink `
      -Name elisp\powershell.elc    -Target $Repo\elisp\powershell.elc

    # If ClangFormat is installed in Cygwin Emacs, use it, otherwise, grab
    # what's in the "dotfiles" Git repo.
    if (Test-Path -Path "$EmacsSiteLispDir\clang-format.elc") {
        $ClangFormatDir = $EmacsSiteLispDir
    } else {
        $ClangFormatDir = "$Repo\elisp"
    }

    New-Item -Force -ItemType SymbolicLink `
      -Name elisp\clang-format.el  -Target $ClangFormatDir\clang-format.el
    New-OptSymbolicLink `
      -Name elisp\clang-format.elc -Target $ClangFormatDir\clang-format.elc

    Pop-Location
}


<#
.SYNOPSIS
Create a symbolic link, only if the link target exists.
#>
function New-OptSymbolicLink {
    param (
        [Parameter(Mandatory=$true)] [string] $Name,
        [Parameter(Mandatory=$true)] [string] $Target
    )

    if (Test-Path -Path $Target) {
        New-Item -Force -ItemType SymbolicLink -Name $Name -Target $Target
    } else {
        Write-Warning "Skipping ""$Target"", which doesn't exist."
    }
}


<#
.SYNOPSIS
Create an .Xdefaults configuration for Emacs in the registry.

.DESCRIPTION

Rather than simply merging in a registry file, this function determines
dynamically if fonts and screen sizes need to be adjusted, and allows for
dynamic configuration of the fg/bg color scheme.  This logic is otherwise
duplicated in the .emacs file, because the .reg solution would often get things
wrong.

.PARAMETER HiResDisplay
Indicates if the function should assume the display is "high" resolution, or
"normal."

.PARAMETER LightBg
Indicates if Emacs should have a light background (with a dark foreground), or
the reverse.
#>
function Set-EmacsXdefaults {
    param (
        [Parameter(Mandatory=$true)] [bool] $HiResDisplay,
        [Parameter(Mandatory=$true)] [bool] $LightBg
    )

    if ($HiResDisplay) {
        $LineCount = 50
        $FontHeight = 22
    } else {
        $LineCount = 70
        $FontHeight = 11
    }

    if ($LightBg) {
        $EmacsBgColor = "#D8D0C8"
        $EmacsFgColor = "#000000"
    } else {
        $EmacsBgColor = "gray20"
        $EmacsfgColor = "gray81"
    }

    $EmacsKeyPath = "HKCU:\SOFTWARE\GNU"
    $EmacsKeyName = "Emacs"
    $EmacsRegPath = "$EmacsKeyPath\$EmacsKeyName"

    if (-not (Test-Path -Path "$EmacsRegPath" -PathType Container)) {
        New-Item -Path "$EmacsKeyPath" -Name $EmacsKeyName -Force
    }

    Set-ItemProperty -Force -Path $EmacsRegPath `
      -Name "Emacs.toolBar" -Value "false"
    Set-ItemProperty -Force -Path $EmacsRegPath `
      -Name "Emacs.Geometry" -Value "81x$LineCount"
    Set-ItemProperty -Force -Path $EmacsRegPath `
      -Name "Emacs.Font" -Value "Consolas 11"
    Set-ItemProperty -Force -Path $EmacsRegPath `
      -Name "Emacs.Background" -Value $EmacsBgColor
    Set-ItemProperty -Force -Path $EmacsRegPath `
      -Name "Emacs.Foreground" -Value $EmacsFgColor
}


<#
.SYNOPSIS
Write a hello message, and give a chance for the user to cancel the set-up.
#>
function Write-InstalledPkgMessage {
    Write-Output `
      "======================================================================" `
      "Hello!  You are about to set-up your workstation with Matt Bisson's" `
      "configuration.  This script is idempotent (but it will overwrite any " `
      "locally made changes).  Ideally, you have installed the following:" `
      " - Cygwin" `
      " - GNU Emacs" `
      " - GNU Make" `
      " - Liberation Mono (Font)" `
      " - ASpell" `
      " - Clang Format" `
      " - Git" `
      " - ZSH" `
      ""

    Write-Output `
      "If you have my repo in ""$DotfileRepo"" (and admin privileges in this " `
      "shell), the script links in a ZSH/Cygwin environment." `
      "======================================================================"

    # Allow the user to ^C, if they like...
    Pause
}

################################################################################
# The Script Begins Here!
################################################################################

#
# Hello!
#
Write-InstalledPkgMessage

#
# Make directories
#
Write-Output "Creating initial directories in ""$HomeDir""."
Add-InitialDirStructure -HomeDir $HomeDir | out-null

#
# Make symlinks to the profile scripts
#
$IsAdmin = ([Security.Principal.WindowsPrincipal] `
            [Security.Principal.WindowsIdentity]::GetCurrent() `
           ).IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator)
if ($IsAdmin) {
    if (Test-Path -Path $DotfileRepo -PathType Container) {
        Write-Output "Creating links from ""$DotfileRepo"" to ""$HomeDir""."
        Add-ProfileSymlinks -HomeDir $HomeDir -Repo $DotfileRepo | out-null
    } else {
        Write-Warning "Dotfile repo ""$DotfileRepo"" does not exist."
    }
} else {
    Write-Warning "Not admin: skipping symbolic link creation."
}

#
# Initialize user Cygwin
#
if (Test-Path -Path "HKLM:\SOFTWARE\Cygwin\setup" -PathType Container) {
    $CygwinRoot = (Get-ItemProperty `
      -Path "HKLM:\SOFTWARE\Cygwin\setup" -Name "rootdir").rootdir
    Write-Output "Found Cygwin at ""$CygwinRoot""."
    # --- TODO! ---
} else {
    Write-Warning "Skipping Cygwin setup: not installed."
}

#
# Generate the login script, and add it to the registry
#
Write-Output `
  "Generating a CMD.EXE start-up script in ""$HomeDir\login.gen.cmd""."
Add-CmdLoginScript -ScriptName "$HomeDir\login.gen.cmd"

#
# Set Emacs Xdefaults
#
# TODO: [0] for some, not for others...
$VidController = Get-WmiObject win32_videocontroller
$HiResDisplay = $True #($VidController.CurrentVerticalResolution[0] -gt 1200)
Set-EmacsXdefaults -HiResDisplay $HiResDisplay -LightBg $false

#
# Set Emacs shortcuts
#
if (Test-Path -Path $EmacsDir -PathType Container) {
    Write-Output "Found Emacs: ""$EmacsDir"""

    [System.Environment]::SetEnvironmentVariable(
        'EDITOR', '"$EmacsDir\bin\emacs.exe"', 'User')

    Add-EmacsClientShortcut $EmacsVersion $EmacsDir
    Add-EmacsShortcut $EmacsVersion $EmacsDir
} else {
    Write-Warning "Emacs not found; skipping set-up."
}
