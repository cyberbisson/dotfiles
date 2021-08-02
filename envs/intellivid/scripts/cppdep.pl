#!/usr/bin/perl -w

use English '-no_match_vars';
use strict;

my @include_dirs;
my @deplist;
my $indent_level = 0;

my $freaking_intellivid        = 1;
my $freaking_intellivid_system = 1;

sub normalize_path ($)
{
    return join('/', split(/\/\/+/, $ARG[0]));
}

sub duplicated_include ($@)
{
    my $infile = shift (@ARG);

    foreach my $found (@ARG)
    {
        if ($infile eq $found)
        {
            return 1;
        }
    }

    return 0;
}

sub get_include_filename ($$)
{
    my ($infile, $check_local) = @ARG;

    # #include <> vs #include ""
    if ($check_local == 1)
    {
        if (-f $infile)
        {
            return normalize_path ($infile);
        }
    }

    # Absolute path = straight passthrough
    if ($infile =~ m/^\//)
    {
        return normalize_path ($infile);
    }

    foreach my $path (@include_dirs)
    {
        if (-f "$path/$infile")
        {
            return normalize_path ("$path/$infile");
        }
    }

    return $infile;
}

sub print_indent ()
{
    for (my $i = 0; $i < $indent_level; $i++)
    {
        print ("  ");
    }
}

sub find_dependencies ($)
{
    my ($infile) = @ARG;
    my $lineno = 0;
    my $incomment = 0;

    open my $FH, $infile or die $!;

    $indent_level++;

    while (<$FH>)
    {
        # Yes, there are flaws in the logic, but I'm not writing a scanner
        # because it'll be close enough
        $lineno++;
        if ($ARG =~ m/\/\*/)
        {
            $incomment = 1;
        }
        if ($incomment)
        {
            if ($ARG =~ m/\*\//)
            {
                $incomment = 0;
            }
            next;
        }

        if ($ARG =~ m/^\s*#\s*(.+)$/)
        {
            if ($1 =~ m/^include\s+([\"\<])(\S+)([\"\>])/)
            {
                my $examine_next = get_include_filename ($2, ($1 eq "\""));
                print_indent ();
                print "- " . $examine_next;

                if (duplicated_include ($examine_next, @deplist))
                {
                    print " [AGAIN]\n";
                }
                else
                {
                    print "\n";
                    if (-f $examine_next)
                    {
                        push (@deplist, $examine_next);
                        &find_dependencies ($examine_next);
                    }
                }
            }
        }
    }

    $indent_level--;

    close $FH;
}

{
    my $wantinput = 0;
    my @input_files;

    if ($freaking_intellivid == 1)
    {
        push (@include_dirs, "/buildtools/c2/SDL-1.2.8/include/SDL");
        push (@include_dirs, "/buildtools/c2/curl-7.15.1/include/curl/");
        push (@include_dirs, "/buildtools/c2/ffmpeg-0.4.8/include/ffmpeg");
        push (@include_dirs, "/buildtools/c2/fftpack-netlib20050505/include");
        push (@include_dirs, "/buildtools/c2/freetype-2.1.9/include");
        push (@include_dirs, "/buildtools/c2/freetype-2.1.9/include/freetype2");
        push (@include_dirs, "/buildtools/c2/glew-1.3.3_p2/include");
        push (@include_dirs, "/buildtools/c2/iniparser-2.17/include");
        push (@include_dirs, "/buildtools/c2/openssl-0.9.8b/include");
        push (@include_dirs, "/buildtools/c2/postgresql-8.1.9/include");
        push (@include_dirs, "/buildtools/c2/qt-x11-commercial-src-4.3.3_p1/include");
        push (@include_dirs, "/buildtools/c2/qt-x11-commercial-src-4.3.3_p1/include/ActiveQt");
        push (@include_dirs, "/buildtools/c2/qt-x11-commercial-src-4.3.3_p1/include/Qt3Support");
        push (@include_dirs, "/buildtools/c2/qt-x11-commercial-src-4.3.3_p1/include/QtCore");
        push (@include_dirs, "/buildtools/c2/qt-x11-commercial-src-4.3.3_p1/include/QtGui");
        push (@include_dirs, "/buildtools/c2/qt-x11-commercial-src-4.3.3_p1/include/QtOpenGL");
        push (@include_dirs, "/buildtools/c2/xorg-2004-03-18/include");
        push (@include_dirs, "/buildtools/c3/boost-1.33.1_p1/include");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/apps/ui");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/core");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/core/viz");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/csu");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/db");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/db/user");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/idxs");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/imu");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/net");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/pos");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/sched");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/smu");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/sys");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/track");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/ui");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/ui/param");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/ui/qtsolutions");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/ui/qtsolutions/obj_Linux-2.6_instrumented");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/ui/wgt");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/ui/wgt/obj_Linux-2.6_instrumented");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/ui/xpm");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/uihw");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/util");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/vcache");
        push (@include_dirs, "/home/mbisson/porter/3.1.0/modules/vsm");
        push (@include_dirs, "/usr/include/GL");
    }

    # IntelliVid / Linux specific include directories as of 6/25/2008
    if ($freaking_intellivid_system == 1)
    {
        push (@include_dirs, "/buildtools/c2/gcc-4.2.3/include/c++/4.2.3");
        push (@include_dirs, "/buildtools/c2/gcc-4.2.3/include/c++/4.2.3/i686-pc-linux-gnu");
        push (@include_dirs, "/buildtools/c2/gcc-4.2.3/include/c++/4.2.3/backward");
        push (@include_dirs, "/buildtools/c2/gcc-4.2.3/lib/gcc/i686-pc-linux-gnu/4.2.3/include");
        push (@include_dirs, "/usr/local/include");
        push (@include_dirs, "/buildtools/c2/gcc-4.2.3/include");
        push (@include_dirs, "/usr/include");
    }

    foreach my $cmdarg (@ARGV)
    {
        if ($wantinput == 1)
        {
            push (@include_dirs, $cmdarg);
            $wantinput = 0;
            next;
        }
        elsif ($cmdarg =~ m/-(\w)(.*)/)
        {
            if ($1 eq "I")
            {
                if ($2 ne "")
                {
                    push (@include_dirs, $2);
                }
                else
                {
                    $wantinput = 1;
                    next;
                }
            }
            else
            {
                die "$0: Unknown parameter given - \"$1\".\n"
            }
        }
        elsif ($cmdarg =~ m/--(.+)/)
        {
            die "$0: Unknown parameter given - \"$1\".\n"
        }
        else
        {
            push (@input_files, $cmdarg);
        }
    }

    if ($wantinput)
    {
        die "$0: Gave -I with no parameter.\n";
    }
    elsif ($#input_files < 0)
    {
        die "$0: No input files given.\n";
    }

    foreach my $input_file (@input_files)
    {
        print "Scanning $input_file\n";
        find_dependencies ($input_file);
    }
}
