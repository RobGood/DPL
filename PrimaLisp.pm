#!/usr/bin/perl -w
# -*- coding: UTF-8 -*-
# $Id: PrimaLisp.pm,v 1.460 2013/04/18 17:07:56 rcgood Exp $  #%^)
#
# This implements a PrimaLisp interpreter and some basic builtins.
# Copyright (C) 2010-2013 Rob Good
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


#X# doc_ClassDesc(<|
# # $Id: PrimaLisp.pm,v 1.460 2013/04/18 17:07:56 rcgood Exp $
# # |>, <|
# # This class implements a Descriptor/PrimaLisp interpreter runtime environment.
#X# |>)

BEGIN {
    push @INC, '.', '/usr/local/lib/perl';
    $ENV{PATH} = "$ENV{PATH}:/usr/local/bin:/usr/bin:/bin";
}

#X# doc_PerlPackage(<|
package PrimaLisp;
$Version = '0.8.46';
$VDate   = '2013-04-18';
#X# |>)

$Version = sprintf '%s.%s', $Version, (split /\./, (split /\s+/, '$Revision: 1.460 $')[1])[1];


#X# doc_PerlUses(0,
use FileHandle;
use JSON -convert_blessed_universally;
use Encode;
use Time::HiRes qw(time);
use File::Path;
use Digest::MD5;
use IPC::SysV qw(ftok IPC_PRIVATE S_IRWXU IPC_CREAT);
use IPC::Semaphore;
use HTML::Entities;

no warnings 'recursion';
#X# )

#---
# use Profiler;
# my $pf = new Profiler('PrimaLisp', 'dpl');
#---
  my $pf;
#---

END { $pf->report if defined $pf }


#----------------------------------------------------------------------------------------

# Make sure we don't blow up trying to use threads on a perl that isn't configured for it.
# my ($GetThreadIdFn, $NoThreadsConfigMesg);
    
$NoThreadsConfigMesg = '';
$GetThreadIdFn = eval 'sub { use threads; threads->self()->tid }';
# eval { if($GetThreadIdFn) { &$GetThreadIdFn } };
if($@) { $GetThreadIdFn = ''; $NoThreadsConfigMesg = $@ }
          

$logFH = STDERR;

#X# doc_Method(_report, <|
# # _report($msg);
# # |>, <|Record message in the diagnostic log file.
#X# |>)
sub
_report {
    my ($msg) = @_;
	# if(ref $msg) { printf STDERR " >> _report(%s)\n", join(',', @$msg) }
	# else         { printf STDERR " >> _report(%s)\n", $msg            }

    if(ref $msg eq 'ARRAY') {
        # map { printf STDERR " -- msg: %s\n", utf8::is_utf8($_)? encode_utf8($_): $_ } @$msg;

        my $fmt = shift @$msg;
        $msg = sprintf $fmt, map { decode_utf8 $_ } @$msg;
    }

    chomp $msg;

    my $typeStr = '--';
    if($msg =~ s/^ (..) //) { $typeStr = $1 }

    my $tid;
    if($GetThreadIdFn) { $tid = &$GetThreadIdFn }
    $tid = defined $tid && $tid? sprintf("#%d", $tid): '';

	$logFH->autoflush(1);

    my $now = time;
    printf $logFH " %s [%.6f %s]%s %s\n",
        $typeStr, $now, scalar(localtime($now)), $tid, encode_utf8($msg);

    return;
}


#X# doc_Method(_throw, <|
# # _throw($exception);
# # |>, <|Throw an exception that can be caught by (catch).
#X# |>)
sub
_throw {
    my ($e) = @_;
      print STDERR " >> _throw($e)  Deprecated call!\n";

    my $cs = $o->eval('(callstack)', {});
      _report(['cs: %s', join(", ", @$cs)]);

    die { exception => defined $e? $e: $@ };

    return;
}

#X# doc_Method(throw, <|
# # throw($exception);
# # |>, <|Throw an exception that can be caught by (catch).
#X# |>)
sub
throw {
    my ($o, $e) = @_;
    # print STDERR " >> throw($e)\n";

    my $bi = $o->{builtinsMap};

    my $cs = $o->{'callstack-at-throw'};
    if(!defined $cs) {
        $cs = $bi->{callstack}->($o, []);
        $o->{'callstack-at-throw'} = $cs;
        _report(['cs: %s', join(" | ", @$cs)]);
    }

    my $cast = {
		callstack => $cs,
		exception => defined $e? $e: $@
	};

    if($o->{debug}) {
        _report(sprintf(" ** ********* Caught this exception for you: %s", $cast->{exception}));
        $cast->{exception} = $o->evalAll('(= _err $__err) (repl) $_err', { __err => $cast->{exception} });
    }

    ##########
    #        #
    #   The  #
        die  #
    #   is   #
      $ cast ;
    #        #
    ##########
}


#X# doc_Method(_return, <|
# # _return($value);
# # |>, <|Throw an internal exception that implements early out (return).
#X# |>)
sub
_return {
    my ($value) = @_;
    # print STDERR " >> _return($value)\n";

	if(@_ > 1) { die { values => \@_ }}
    die { value => $value };

    return;
}

#----------------------------------------------------------------------------------------

my $_sig_o;

sub
_sig {
    die unless defined $_sig_o;

    my $msg = $_sig_o->eval("(repl)", { });
    
    $_sig_o->throw($msg) if defined $msg && $msg;
}

$SIG{INT} = \&_sig;
# $SIG{HUP} = \&_sig;
$SIG{ALRM} = \&_sig;

#----------------------------------------------------------------------------------------

#X# doc_Method(_urlencode, <|
# # _urlencode($str);
# # |>, <|
#X# |>)
sub
_urlencode {
    my ($str) = @_;
    # print STDERR " >> _urlencode($str)\n";

    # if(!utf8::is_utf8($str)) { $str = encode_utf8 $str }

    $str =~ s/([^\w._~-])/sprintf("%%%02X", ord $1)/eg;
    return $str;
}


#X# doc_Method(_urldecode, <|
# # _urldecode($str);
# # |>, <|
#X# |>)
sub
_urldecode {
    my ($str) = @_;
    # print STDERR " >> _urldecode($str)\n";

    $str =~ s/\+/ /g;
    $str =~ s/%([0-9A-Fa-f][0-9A-fa-f])/chr(hex("$1"))/eg;

    return decode_utf8 $str;
}


#X# doc_Method(_xmlencode, <|
# # _xmlencode($text);
# # |>, <|
#X# |>)
sub
_xmlencode {
    my ($text) = @_;
    # print STDERR " >> _xmlencode($text)\n";

    encode_entities($text);
    return $text;
}


#X# doc_Method(_xmldecode, <|
# # _xmldecode($text);
# # |>, <|
#X# |>)
sub
_xmldecode {
    my ($text) = @_;
    # print STDERR " >> _xmldecode($text)\n";

    decode_entities($text);
	return $text;
}



#----------------------------------------------------------------------------------------
# Directory discovery.

#X# doc_Method(getCGIDir, <|
# # getCGIDir();
# # |>, <|
#X# |>)
sub
getCGIDir {
    # print STDERR " >> getCGIDir()\n";

    my $dir = $ENV{DPL_CGI_DIR}; $dir = '' unless defined $dir;
       $dir = '/srv/www/cgi-bin' unless -d $dir;
       $dir = '/var/www/cgi-bin' unless -d $dir;
       $dir = '/usr/lib/cgi-bin' unless -d $dir;

    return $dir;
}

#X# doc_Method(getWWWDir, <|
# # getWWWDir();
# # |>, <|
#X# |>)
sub
getWWWDir {
    # print STDERR " >> getWWWDir()\n";

    my $dir = $ENV{DPL_WWW_DIR}; $dir = '' unless defined $dir;
       $dir = '/srv/www/htdocs' unless -d $dir;
       $dir = '/var/www/htdocs' unless -d $dir;
       $dir = '/var/www/html'   unless -d $dir;

    return $dir;
}

#X# doc_Method(getBinDir, <|
# # getBinDir();
# # |>, <|
#X# |>)
sub
getBinDir {
    # print STDERR " >> getBinDir()\n";

    my $dir = $ENV{DPL_BIN_DIR}; $dir = '' unless defined $dir;
       $dir = '/usr/local/bin' unless -d $dir;
       $dir = "$ENV{HOME}/bin" unless -d $dir;

    return $dir;
}

#X# doc_Method(getPerlDir, <|
# # getPerlDir();
# # |>, <|
#X# |>)
sub
getPerlDir {
    # print STDERR " >> getPerlDir()\n";

    my $dir = $ENV{DPL_PERL_DIR}; $dir = '' unless defined $dir;
       $dir = '/usr/local/lib/perl' unless -d $dir;
       $dir = "$ENV{HOME}/lib/perl" unless -d $dir;
       $dir = "$ENV{HOME}/perl"     unless -d $dir;

    return $dir;
}


#---------------------------------------------------------------------------------------

#X# doc_Method(shuffleList, <|
# # $o->shuffleList($list);
# # |>, <|
#X# |>)
sub
shuffleList {
	my ($o, $list) = @_;
	# $lgr->Enter("shuffleList($list)");

	return [ map { $_->[1] } sort { $a->[0] <=> $b->[0] } map { [ rand, $_ ] } @$list ];
}


#----------------------------------------------------------------------------------------

$nInner = 0;

my $Default_builtinsMap = {

    'def-fn/usage' => '(def-fn -name- (-params-) -body-) : Define a function -name- with -params- and -body-.',
    'def-fn/ismacro' => 1,
    'def-fn' => '(fn (name params &body) // (report " >> (def-fn $name $params ...)")
          (
             var-set       $name  (sprintf "(fn %s %s)" $params (join " " $body))
          // var-set (eval $name) (sprintf "(fn %s %s)" $params (join " " $body))

              // Extract attributes for usage messages.
              // This is still within the (var-set) call!

              ((fn (body n attrs)
                (if (list-len $body)
                 (while (eq "\'" (substr (first $body) 0 1))
                  (++_ n)
                  (hash-set $attrs / "usage$n" (eval (shift $body)))))
                $attrs)  $body 0 (hash))
              ))',


    'def-macro/usage' => '(def-macro -name- (-params-) -exprs- ...) : Define macro -name- with parameters -params- with body -exprs-.',
    'def-macro/ismacro' => 1,
    'def-macro' => '(fn (name params &exprs)
        (var-set name (eval $name))
        (push params (eval (sprintf "(def-fn %s %s %s)" $name $params (join " " $exprs))))
        (eval (sprintf "(var-set %s $%s {ismacro 1})" $name $name))
        (pop params))',


    'parse/usage' => '(parse -expr-) : Convert -expr- from string (S expression) form to list form.',
    'parse/ismacro' => 1,
    'parse' => '(fn (expr - out)
        (oncomma ignore)
        (= out (if (ne "(" /*)*/ (substr $expr 0 1))
          (return $expr)
          (map-list (fn (e) (eval "(parse $e)")) (eval "(expr-to-list $expr)"))))
        (oncomma ())
        $out)',


    # TODO make this a native language builtin.
	'foreach/usage' => '(foreach -var- -in-list- -body-) : Evaluate -body- with -var- bound to each element in -in-list-.',
	'foreach/ismacro' => 1,
	'foreach' => '(fn (.var .in-list &.exprs)
			((fn (.body-fn) (map-list (fn (.i) ($.body-fn $.i)) (eval $.in-list)))
		(sprintf "(fn (%s) %s)" (eval $.var) (join " " $.exprs))))',



    'def-hash-fn/usage1' => '(def-hash-fn -desc- -path- -name-) : Define function -name- from expression in -desc- at -path- / -name-.',
    'def-hash-fn/usage2' => '(def-hash-fn -desc- -api-) : Define functions specified by -api- from -desc-.',
    'def-hash-fn' => '(fn (self path fn -- api)
        (if (hash? (var-set api $path))
          (foreach path (hash-keys $api /)
		    (foreach name (hash-get $api / $path)
              // (report " -> %s - %s (%s)" $path $name)
              (def-hash-fn $self $path $name)))
		  (var-set $fn (utf8-decode (hash-eval $self $path $fn)) {path $path desc-name $self/name})))',

    'def-desc-fn/usage' => '(def-desc-fn -desc- -path- -name-) : Define function -name- from expression in -desc- at -path- / -name- (using /refer-to inheritance).',
    'def-desc-fn' => '(fn (self path fn) (var-set $fn (desc-eval $self $path $fn) {path $path}))',

    'report-vars/usage' => '(report-vars -var-names- ...) : Report the values given.',
    'report-vars' => '(fn (&names) (report %s (desc-to-string $[@$names])))',

    # '/usage' => '() : .',
    # '' => sub {
    # },

    #------------------------------------------------------------------------------------------

    'update-DPL-dist/usage' => '(update-DPL-dist -source-) : Update this installation from designated -source-.',
    'update-DPL-dist' => sub {
        my ($o, $args) = @_;
        my ($source) = @$args;

        if(defined $options) {
            if(ref $options eq '') { $source = $options           }
            else                   { $source = $options->{source} }
        }

		#-----
		# (update-DPL-dist) modifies some code here (post-installation) to make the
		# default (update-DPL-dist) source host the host providing the dist file. 
		# /8467ecb92e76efbb21eed29b9c9ec9d0

		## if(!defined $source || $source eq '') { $o->throw("No source specified and no default source available.") }

        $source = 'srvdevlnx01'            unless defined $source;
        # $source = 'update.dpl.primal.com'  unless defined $source;
        # $source = 'update.dpl.org'         unless defined $source;
		#-----


        my $dryrun = 0;

        my $hostname = `/bin/hostname`; chomp $hostname;
        if($source eq $hostname) {
            $dryrun = 1;
            _report("Can't call (update-DPL-dist source) from the source host! Doing dryrun for testing.");
         }

        my $perlDir    = getPerlDir;
        my $binDir     = getBinDir;
        my $cgiDir     = getCGIDir;

        my @destDirs = (
            $perlDir,
            $binDir,
            $cgiDir,
            "$cgiDir/descriptor-dir",
        );

        map { if(! -w $_) { $o->throw("Can't write to dir $_") } } @destDirs;

        my @cpCmds = ( );
        my @cmCmds = ( );
        my @mvCmds = ( );

        sub _cp { my ($src, $dstDir) = @_; "cp -pvf $src $dstDir/$src.$$" };
        sub _cm { my ($src, $dstDir) = @_; "(chmod +w $dstDir/$src || true)" };
        sub _mv { my ($src, $dstDir) = @_; "(chmod +w $dstDir/$src || true ;  mv -vf $dstDir/$src.$$ $dstDir/$src)" };

        map {
            push @cpCmds, _cp(@$_);
            push @cmCmds, _cm(@$_);
            push @mvCmds, _mv(@$_);
        } (
            [ 'PrimaLisp.pm', $perlDir ],
            [ 'dpl',          $binDir  ],
            [ 'dpl-depends',  $binDir  ],
            [ 'dpld',         $binDir  ],
            [ 'dplx',         $binDir  ],
            [ 'dplx+',         $binDir  ],
            [ 'dple',         $cgiDir  ],
            [ 'dple+',         $cgiDir  ],
            [ 'dplx',         $cgiDir  ],
            [ 'dplx+',         $cgiDir  ],
        );

        my $now = time;
        my $ts = ".$now.$$";

        my $instDir = "DPL-dists/$ts";
        $instDir = "$ENV{HOME}/$instDir"  if exists $ENV{HOME};

        my @cmds = (
            "cd && mkdir -p $instDir/.upl && cd $instDir/.upl",

            "wget -q -O .dist.tar 'http://$source/api/dplx/get-DPL-dist'",
            "(tar xvzpf .dist.tar -C .. 1>&2 || true)",

            'dir=`ls ..`',
            'mv .dist.tar $dir.tar.gz',

            'chmod +w ../$dir/PrimaLisp.pm',
            '(md5sum $dir.tar.gz >> ../$dir/PrimaLisp.pm)',
            'chmod -w ../$dir/PrimaLisp.pm',

            'cd ../$dir',

            join(' && ', @cpCmds),
            join(' && ', @cmCmds),
            join(' && ', @mvCmds),

            # Still need these in @..Cmds...
            "cp -pvf  `ls -d descriptor-dir/* | grep -v '/RCS\$'`         $cgiDir/descriptor-dir",
            "cp -pvfr descriptor-dir/.import   $cgiDir/descriptor-dir",

            "cd .. && mv \$dir ../\$dir.$ts && cd .. && rm -rf $instDir",
        );

        my $cmd = join(' && ', map { s/^\s+//; s/\s+/ /g; s/\s+$//; $_ } @cmds);

        if($dryrun) {
            # _report " %> $cmd";
            return { dryrun => 1, cmd => $cmd };
        }

        # _report([' %%> %s', $cmd]);
        my $out = `($cmd) 2>&1`;
        if($?) { $o->throw("Can't do update ($?): $out") }



	if(1) {
		# /8467ecb92e76efbb21eed29b9c9ec9d0
		my $usifh = new FileHandle("< $perlDir/PrimaLisp.pm");
		my $usofh = new FileHandle("> $perlDir/PrimaLisp.pm.$$");

		my $inject = "        \$source = '$source' if !defined \$source;";

		while(<$usifh>) {
			if(defined $inject && m/8467ecb92e76efbb21eed29b9c9ec9d0$/) {
				print $usofh "$inject\n";
				undef $inject;
			}

			print $usofh $_;
		}

		close $usifh;
		close $usofh;
		
        chmod 0444, "$perlDir/PrimaLisp.pm.$$";
		rename "$perlDir/PrimaLisp.pm.$$", "$perlDir/PrimaLisp.pm";
		# /8467ecb92e76efbb21eed29b9c9ec9d0
	}

        my $version = from_json `dpl -version < /dev/null`;

        return { 'source' => $source, 'hostname' => $hostname, 'log' => $out, 'version' => $version };
    },

    'get-DPL-dist/usage0' => '(get-DPL-dist) : Return a tar.gz file containing DPL code.',
    'get-DPL-dist/usage1' => '(get-DPL-dist -packer-) : Return a dist file containing DPL code. -packer- can be tar.gz or zip',
    'get-DPL-dist' => sub {
        my ($o, $args) = @_;

        my ($thisHost, $remHost);

        $thisHost = `hostname`; chomp $thisHost;
        $thisHost =~ s/\.indimensions\.com$//;

        if(exists $ENV{REMOTE_ADDR}) {
            $remHost = $ENV{REMOTE_ADDR};
            my $resolvedHost = `nslookup $remHost`;

            if($resolvedHost =~ /name = (\S+)/s) {
                $remHost = $1;
                $remHost =~ s/\.$//;
                $remHost =~ s/\.indimensions\.com$//;
            }
        }



        my $name = "DPL-$Version";
        my $dir0 = "/tmp/.get-dpl.$$";
        my $dir  = "$dir0/$name";

        my $packer = $args->[0];
        $packer = 'tar.gz' unless defined $packer;

        my $packerCmd;
           if($packer eq 'tar.gz') { $packerCmd = "(tar cf - $name | gzip)"     }
        elsif($packer eq 'zip'   ) { $packerCmd = "zip -qr - $name" } 
        else { $o->throw("Unsupported packer '$packer'") }
        

        my $binDir     = getBinDir;
        my $cgiDir     = getCGIDir;

        my $descDirSrc = "$cgiDir/descriptor-dir";
        my $descDirDst = 'descriptor-dir';

        my $descDirSrcII = "$descDirSrc/.ii/lib-dpl";
        my $descDirDstII = "$descDirDst/.ii/lib-dpl";


        # Collect files into distribution directory.
        my @cmds = (

         "umask 002",

         # "mkdir -p $dir/$descDirDst/RCS ",
         "mkdir -p $dir/$descDirDst/.import ",
         "mkdir -p $dir/$descDirDstII/.var-gps/incl ",
         "mkdir -p $dir/$descDirDstII/.import ",

         "cp -p $INC{'PrimaLisp.pm'} $dir ",

         "cp -p $binDir/dpl          $dir ",
         "cp -p $binDir/dpl-depends  $dir ",
         "cp -p $binDir/dpld         $dir ",
         "cp -p $cgiDir/dple         $dir ",
         "cp -p $cgiDir/dplx         $dir ",

         "cp -p $descDirSrcII/.import/Core.dpli                            $dir/$descDirDstII/.import ",
         "cp -p $descDirSrcII/.import/Core_sockets.dpli                    $dir/$descDirDstII/.import ",
         "cp -p $descDirSrcII/.import/Authentication_DPL.dpli              $dir/$descDirDstII/.import ",
         "cp -p $descDirSrcII/.import/Authentication_ActiveDirectory.dpli  $dir/$descDirDstII/.import ",

         "if [ -f $descDirSrc/.COPYING ]; then cp -p $descDirSrc/.COPYING $dir/COPYING; fi ",

         "cp $descDirSrcII/.var-gps/incl/dpl.css  $dir/$descDirDstII/.var-gps/incl ",


         # Copy in descriptors.
         "umask 0333 ",

         "cp $descDirSrcII/ChangeLog               $dir/$descDirDstII ",
         "cp $descDirSrcII/Credits                 $dir/$descDirDstII ",
         "cp $descDirSrc/system-version          $dir/$descDirDstII ",

         "cp $descDirSrcII/BaseTests               $dir/$descDirDstII ",
         "cp $descDirSrcII/Benchmarks              $dir/$descDirDstII ",
         "cp $descDirSrcII/DPL-demo-template       $dir/$descDirDstII ",
         "cp $descDirSrcII/HTML-macros             $dir/$descDirDstII ",
         "cp $descDirSrcII/HTML.test               $dir/$descDirDstII ",
         "cp $descDirSrcII/README                  $dir/$descDirDstII ",
         "cp $descDirSrcII/README.info.null        $dir/$descDirDstII ",
         "cp $descDirSrcII/Tests                   $dir/$descDirDstII ",
         "cp $descDirSrcII/desc-function-index     $dir/$descDirDstII ",
         "cp $descDirSrcII/dbi-tests               $dir/$descDirDstII ",

         "cp $descDirSrcII/dpl.man.docs  $dir/$descDirDstII ",
         "cp $descDirSrcII/dpld.man.docs $dir/$descDirDstII ",
         "cp $descDirSrcII/dple.man.docs $dir/$descDirDstII ",
         "cp $descDirSrcII/dplx.man.docs $dir/$descDirDstII ",
         "cp $descDirSrcII/eval.man.docs $dir/$descDirDstII ",
         "cp $descDirSrcII/man.man.docs  $dir/$descDirDstII ",

         "cp $descDirSrcII/dpl-getting-started     $dir/$descDirDstII ",
         "cp $descDirSrcII/dple                    $dir/$descDirDstII ",
         "cp $descDirSrcII/dplx                    $dir/$descDirDstII ",
         "cp $descDirSrcII/dplx-docs               $dir/$descDirDstII ",
         "cp $descDirSrcII/echo                    $dir/$descDirDstII ",
         "cp $descDirSrcII/eval                    $dir/$descDirDstII ",
         "cp $descDirSrcII/eval-docs               $dir/$descDirDstII ",
         "cp $descDirSrcII/eval-docs-examples      $dir/$descDirDstII ",
         "cp $descDirSrcII/eval-history            $dir/$descDirDstII ",
         "cp $descDirSrcII/get-DPL-dist            $dir/$descDirDstII ",
         "cp $descDirSrcII/man                     $dir/$descDirDstII ",
         "cp $descDirSrcII/man.docs                $dir/$descDirDstII ",
         "cp $descDirSrcII/pretty-print            $dir/$descDirDstII ",
         "cp $descDirSrcII/user                    $dir/$descDirDstII ",
         "cp $descDirSrcII/wk-log                  $dir/$descDirDstII ",

         "cp $descDirSrcII/hw          $dir/$descDirDstII ",
         "cp $descDirSrcII/upload      $dir/$descDirDstII ",

        );

        my $cmd = join(' && ', @cmds);
        system $cmd;
        if($?) { $o->throw(sprintf("%s (%d)", $!, ($?>>8))) }


        my $filename = sprintf "DPL-%s.%s", $Version, $packer;

        my $msg = sprintf "get-DPL-dist: $filename download from %s", $thisHost;
        $msg .= " by $remHost" if defined $remHost;

        if(exists $ENV{REMOTE_ADDR}) { $msg .= " ($ENV{HTTP_USER_AGENT})" }


        my $now = time;
        my $tsMsg = sprintf '[%s %.4f] %s', scalar(gmtime($now)),  $now, $msg;


        # Add a Watermark.
        chmod 0644, "$dir/PrimaLisp.pm";
        my $wmfh = new FileHandle(">> $dir/PrimaLisp.pm");
        die " !! Can't append: $!"
            unless defined $wmfh;

        print $wmfh "/* $tsMsg */\n";
        close $wmfh;


        chmod 0444, "$dir/PrimaLisp.pm";



        # Package the distribution file and cleanup.
        my @cmds2 = (
         "cd $dir/..   ",
         $packerCmd,
         "rm -rf $dir0 ",
        );

        my $cmd2 = join(' && ', @cmds2);
        my $val = `$cmd2`;
        if($?) { $o->throw(sprintf("%s (%d)", $!, ($?>>8))) }


        if(exists $ENV{REMOTE_ADDR}) {
            $msg =~ s/ download by /sprintf('%d bytes download by ', length($val))/e;

            $o->eval("(do (= content-type application/octet-stream) (= content-disposition 'filename=$filename'))");
            $o->eval("(desc-read log dl.log)");
            my $log = $o->eval("\$log");

            unless(exists $log->{downloads}) { $log->{downloads} = [ ] }


            my $ctx = new Digest::MD5;
            $ctx->add($val);
            my $digest = $ctx->hexdigest;

            unshift @{$log->{downloads}}, "$tsMsg, md5: $digest";

            $o->eval("(desc-write \$_log dl.log {no-checkpoint 1})", { _log => $log });
            _report " ** $msg";
        }

        return $val;
    },


    #------------------------------------------------------------------------------------------

    'help/usage1' => "(help) : Returns all available usage messages.", 
    'help/usage2' => "(help -fn-name-) : Returns usage message for function -fn-name-.", 
    'help/usage3' => "(help -pattern-) : Returns usage message for functions starting with -pattern-.", 
    'help' => sub {
        my ($o, $args) = @_;
        # _report(" >> help($o, $args)");
    
        my $msg;

        my $fn = '';

        my $bi = $o->{builtinsMap};
        my $vr = $o->{varMapNS};


        my @msg = ( );

        if(@$args) {
          while(@$args) {
            # Get the function name (or prefix if no exact match).
            $fn = shift @$args;
			last unless defined $fn;

            # The function's usage message is an attribute of the variable holding the function.
            my $attr = "$fn/usage";
			# _report $attr;

            my @attrs;

            if(1 && ( (exists $bi->{$fn} && exists $bi->{$attr}) || (exists $vr->{$fn} && exists $vr->{$attr}) )) {
                @attrs = ( $attr, "${attr}1", "${attr}2", "${attr}3" );
            }
            else {
                # Get it ready for use in regexp.
                $attr =~ s/\+/\\\+/gs;
                $attr =~ s/\*/\\\*/gs;
                $attr =~ s/\|/\\\|/gs;
                $attr =~ s/\/usage/\.*\/usage\.\*/;

                @attrs = grep { /^$attr/ } (keys %$bi, keys %$vr);
                  map { _report "attrs: $_" } @attrs;
            }

			my $msgFound = 0;

            map {
				my $msg;
                # _report "fn: $fn, _: $_";
                   if(exists $vr->{$_}) { $msg = decode_utf8 $vr->{$_}->[0] }
                elsif(exists $bi->{$_}) { $msg = decode_utf8 $bi->{$_}->[0] }

                # _report $msg;
				if(defined $msg && !ref $msg) {
					push @msg, $msg;
					$msgFound = 1;
				}
            } @attrs;

			_report "No help for function $fn..." unless $msgFound;
          }
        }

        $fn = '' unless defined $fn;

        # No args or no exact match. Lookup all the usage attributes and return them sorted in a list.
        
        if(!@msg) {
            push @msg, map { $bi->{$_} } grep { m~^$fn.*/usage.*$~ } keys %$bi;
            push @msg, map { $vr->{$_} } grep { m~^$fn.*/usage.*$~ } keys %$vr;
        }

        # unshift @msg, " ## List of Functions : Usages"
        #   if @msg > 1;

        my $longest = 0;
        map { if($longest < length $_) { $longest = length $_ } } map { my @s = split(' : ', $_) ; shift @s } @msg;

        my $fmt = "%-${longest}s : %s";
        return [ sort
            map {
                if(ref $_ eq 'ARRAY' && @$_ == 1) { ($_) = @$_ } 

                _report " !! No ':' in '$_'" unless /:/;
                if(ref $_ eq 'ARRAY') { map { _report "xx: $_" } @$_ }

                my @s = split(' : ', $_);
                sprintf $fmt, @s;
            }
                map { defined $_? $_:'<undef> : <undef>' }
                    @msg ];
    },

    #------------------------------------------------------------------------------------------

    # Write to diagnostic report. (i.e. log file but (log) was taken...)
    'report/usage' => "(report -msg-) : Timestamp and write -msg- to log file.",
    'report' => sub {
        my ($o, $args) = @_;
        # print $logFH  map { encode_utf8($_) } @$args;
        _report $args;
        return;
    },

    'callstack/usage' => "(callstack) : Return current call stack.",
    'callstack' => sub {
		my ($o, $args) = @_;

		return [ map { $_ } @{$o->{varMapNS}->{'.'}} ];
	},

    'print-banner/usage' => "(print-banner -name-) : Print a banner message.",
    'print-banner' => sub {
        my ($o, $args) = @_;
		my ($name) = @$args;
        # print STDERR " >> print-banner($o, $args)\n";

		$name = '' unless defined $name;
		$name = " $name" if $name;

		my $year = `date +%Y`; chomp $year;
		$year = "2010-$year" unless $year eq '2010';

		my $ofh = $o->{ofh};

		my $cprtNotice = $PrimaLisp::cprtNotice;

		printf $ofh " ** Descriptor/PrimaLisp%s (v%s %s)\n", $name, $Version, $VDate;
		printf $ofh " ** ©%s %s\n", $year, $cprtNotice if defined $cprtNotice;

		return;
    },

    'version/usage' => "(version) : Returns a Map containing this interpreter's version and date.",
    'version' => sub {
        my ($o, $args) = @_;
        # print STDERR " >> version($o, $args)\n";
        return { version => $Version, date => $VDate, rte => sprintf('Perl-%vd', $^V), Perl => sprintf('%vd', $^V) };
    },

    'statistics/usage' => "(statistics) : Returns a hash of useful runtime statistics.",
    'statistics' => sub {
        my ($o, $args) = @_;
        # print STDERR " >> statistics($o, $args)\n";

        my $fh;

        my $bogomips = $o->{bogomips};

        if(!defined $bogomips) {
            $fh = new FileHandle("< /proc/cpuinfo");
            if(defined $fh) {
                while(<$fh>) {
                    chomp;
                    next unless /^bogomips\s+:\s+([\d\.]+)/;
                    $bogomips = $1;
                    last;
                }
                close $fh;
            }
    
            $o->{bogomips} = $bogomips;
        }

        my @t = times;

        my $cpuS = 0;
        map { $cpuS += $_ } @t;

        my $evalRate =  eval { sprintf '%.0f', $o->{n_evals} / $cpuS };
        $evalRate = 0 unless defined $evalRate;

        my %stats = (
            Version => $Version,
            VDate   => $VDate,

            'cpu-s-u' => shift(@t),
            'cpu-s-s' => shift(@t),
            'cpu-s-cu' => shift(@t),
            'cpu-s-cs' => shift(@t),
            'cpu-s-ttl' => $cpuS,

            'n-evals' => $o->{n_evals},
            'n-evals-per-cpu-s' => $evalRate,

			'lock-hold-time-s' => $o->{lockHoldTimes},
        );

        if(defined $bogomips) {
            $stats{bogomips} = $bogomips;
            $stats{'n-inst-per-eval'} = sprintf '%.0f', eval { 1e6 * $bogomips / $evalRate };
            $stats{'n-inst-per-eval'} = 0 if $@;
        }
            
        return \%stats;
    },

    'repl/usage' => "(repl) : Run a Read/Eval/Print Loop until EOF. Returns last value evaluated.",
    'repl' => sub {
        my ($o, $args) = @_;
        # print STDERR " >> repl($o, $args)\n";

        my $prompt   = $o->{prompt};
        my $repSeq   = $o->{repSeq};

        if($repSeq-1 > 0) { $o->{prompt} = sprintf "[%d]%s", $repSeq-1, $prompt }

        $o->{repSeq} = 1;

        my $value = $o->repl(@$args);
        $o->{repSeq} = $repSeq;
        $o->{prompt} = $prompt;

        return $value;
    },


    #------------------------------------------------------------------------------------------

    # Drop interpreter builtins
#    'drop-builtins/usage' => "(drop-builtins -name- ...) : Delete interpreter references to named builtin functions. Now called (bi-drop).",
#    'drop-builtins/deprecated' => 1,
#    # 'drop-builtins' => sub { },

    'bi-drop/usage' => "(bi-drop -name- ...) : Delete interpreter references to named builtin functions.",
    'bi-drop' => sub {
        my ($o, $args) = @_;
        # print STDERR " >> drop-builtins($o, $args)\n";

		if(ref $args->[0] eq 'HASH') {
			my $zapall = $args->[0]->{'zap-all'};

			if(defined $zapall && $zapall) {
				$args = { map { $_ => 1 }  grep { !m/\//  } keys %{$o->{builtinsMap}} };
				$args = [ sort keys %$args ];
				
				_report([' !! Zapping all builtins: %s', join(',', @$args)]);
			}
		}

        for my $name (@$args) {
            if(!exists $o->{builtinsMap}->{$name}) { 
                _report "Attempted dropping of a non-existant builtin '$name'\n";
            }
            else {
                # _report "Dropping builtin function '$name'.";
                my $bi = $o->{builtinsMap};

                # Zap.
                delete $bi->{$name};
				# _report(['Zapped %s', $name]);

                # Don't forget to zap attributes too.
				my $name0 = $name;
				$name0 =~ s/([*+?&])/\\$1/g;
                map { delete $bi->{$_} } grep { m~^$name0/~ } keys %$bi;

                # Leave a message in its place for logging any future calls to dropped builtin.
                $bi->{$name} = { state => 'dropped', msg => "Dropped builtin function '$name' was called." };
            }
        }

        return;
    },

    # Load interpreter extensions as new builtins.
    'bi-import/usage1' => "(bi-import -name- -expr- ...) : Import builtins from -name-, eval -expr- with those definitions. Return with no effect outside this call.",
    'bi-import/usage2' => "(bi-import -name-!) : Import builtins from -name-, eval -expr- with those definitions. Persist builtins outside this call.",
    'bi-import/ismacro' => 1,
    'bi-import' => sub {
        my ($o, $args) = @_;

        $o->throw("(bi-import): No Descriptor directory defined.")
            unless defined $o->{descDir} && -d $o->{descDir};

        my $importName = shift @$args;
        my $exprs = $args;

        $importName = $o->eval($importName, { });

        # _report([' >> (%s %s)', 'bi-import', $importName]);

        my $persistChanges = ($importName =~ s/!$//);

        
        # Don't fall for it.
        $o->throw("(bi-import): Invalid import name '$importName'")
            unless $importName =~ /^[-.\w]+$/;

        # Locate the Perl file to load.
        # This file should end with/return a hash containing the right stuff.

        #  look in the main desc dir.
        my $loadFile = sprintf '%s/.import/%s.dpli',   $o->{descDir}, $importName;
        my $fh = new FileHandle("< $loadFile");

        if(!defined $fh) {
            # look in inner lib* desc dirs.
            my @hits = glob sprintf('%s/.ii/lib[-.]*/.import/%s.dpli', $o->{descDir}, $importName);
            $loadFile = shift @hits;
            if(@hits) {
                _report(['Extra found names being ignored: %s', join(',', @hits)]);
            }
            $fh = new FileHandle("< $loadFile");
        }

        my $pl = $o;
        while(!defined $fh && defined $pl->{outerInterp}) {
            # look in outer desc dir.
            $loadFile = sprintf '%s/.import/%s.dpli', $pl->{outerInterp}->{descDir}, $importName;

            $fh = new FileHandle("< $loadFile");
            if(!defined $fh) {
                # look in outer's inner lib* desc dirs.
                my @hits = glob sprintf('%s/.ii/lib[-.]*/.import/%s.dpli', $pl->{outerInterp}->{descDir}, $importName);
                $loadFile = shift @hits;
                if(@hits) {
                    _report(['Extra found names being ignored: %s', join(',', @hits)]);
                }
                $fh = new FileHandle("< $loadFile");
            }
            $pl = $pl->{outerInterp};
        }

		# _report([' !! Error: %s', $!]) unless defined $fh;
        $o->throw("(bi-import): 1. Can't import '$importName': $!")
            unless defined $fh;

        # Load the Perl code implementing new builtins.
        my $code;
        { local $/ = undef; $code = <$fh> }
        close $fh;

        return unless defined $code;


        # Eval the expressions now.
        my $preamble = <<FINI;
            # (bi-import) Perl side preamble.
            package PrimaLisp::$importName;
            no warnings qw(redefine);
            sub _report {PrimaLisp::_report \@_}
            sub _return {PrimaLisp::_return \@_}
            sub _throw  {PrimaLisp::_throw \@_}
            sub throw   {PrimaLisp::throw \@_}
            sub eval    {PrimaLisp::eval \@_}

            sub _urlencode  {PrimaLisp::_urlencode \@_}
            sub _urldecode  {PrimaLisp::_urldecode \@_}
            sub _xmlencode  {PrimaLisp::_xmlencode \@_}
            sub _xmldecode  {PrimaLisp::_xmldecode \@_}

            \$logFH = \$PrimaLisp::logFH;
FINI

        $code = "$preamble\n\n$code";

        my $importMap = eval $code;
        $o->throw("(bi-import): 2. Can't import '$importName': $@")
            if $@;

        $o->throw("(bi-import): Unexpected return from import code: $importMap")
            unless ref $importMap eq 'HASH';


        # Save previous definitions.
        my $stash = { };

        my $bi    = $o->{builtinsMap};

        # Save and overwrite current builtin Map references.
        # map { $stash->{$_} = $bi->{$_}; $bi->{$_} = $importMap->{$_} } keys %$importMap;
		# 
		# But don't overwite dropped builtins!
          map {
			if(exists $bi->{$_} && ref $bi->{$_} eq 'HASH' && $bi->{$_}->{state} eq 'dropped') {
				my $msg = " !! An attempt to re-import a droppped builtins has been thwarted.";
				_report(['%s', $msg]);
				$o->throw($msg);
			}

			$stash->{$_} = $bi->{$_};
			$bi->{$_} = $importMap->{$_};
		} keys %$importMap;

        # Eval rest of args.
        my $val;
        my $env = { };
        map { $val = $o->eval($_, $env) } @$exprs;

        # Restore previous definitions.
        if(!$persistChanges) {
            map { $bi->{$_} = $stash->{$_} } keys %$stash;
        }


        return $val;
    },

    #------------------------------------------------------------------------------------------

    'sort-alpha/usage' => "(sort-alpha -list-) : Return list of items from -list- sorted alphabetically.",
    'sort-alpha' => sub {
        my ($o, $args) = @_;
        return [ sort @{$args->[0]} ];
        # return [ sort { $a cmp $b } @{$args->[0]} ];
    },
 
    'sort-alpha-reverse/usage' => "(sort-alpha-reverse -list-) : Return list of items from -list- sorted alphabetically.",
    'sort-alpha-reverse' => sub {
        my ($o, $args) = @_;
        return [ sort { $b cmp $a } @{$args->[0]} ];
    },
 
    'sort-num/usage' => "(sort-num -list-) : Return list of items from -list- sorted numerically.",
    'sort-num' => sub {
        my ($o, $args) = @_;
        return [ sort { $a <=> $b } @{$args->[0]} ];
    },

    'sort-num-reverse/usage' => "(sort-num-reverse -list-) : Return list of items from -list- sorted numerically.",
    'sort-num-reverse' => sub {
        my ($o, $args) = @_;
        return [ sort { $b <=> $a } @{$args->[0]} ];
    },

    'sort-pred/usage' => "(sort-pred -pred- -list-) : Return list of items from -list- sorted according to (-pred- -a- -b-).",
    'sort-pred/ismacro' => 1,
    'sort-pred' => sub {
        my ($o, $args) = @_;
		# _report(" >> (sort-pred $args->[0] $args->[1])");

		my $list = $o->eval($args->[1], {});
		# _report(['list: %s %d', $list, scalar(@$list)]);

		my $fn = $args->[0];

        return [ sort { scalar $o->eval("(_fn \$_a \$_b)", { _fn => $fn, _a => $a, _b => $b }) } @$list ];
    },

    'sort-tt/usage' => "(sort-tt -list-) : Custom sort for Trendy Terms.",
    'sort-tt' => sub {
        my ($o, $args) = @_;
        return [ sort {
            my $ret = $a->[0] <=> $b->[0]; return $ret if $ret;
               $ret = $a->[1] cmp $b->[1]; $ret
         } @{$args->[0]} ];
    },

    'sort-rlog/usage' => "(sort-rlog -list-) : Custom sort for rlog.",
    'sort-rlog' => sub {
        my ($o, $args) = @_;
        return [ sort { $b->{'date'} cmp $a->{'date'} } @{$args->[0]} ];
    },

    'sort-records/usage' => "(sort-records -spec- -list-) : Custom sort for rlog.",
    'sort-records' => sub {
        my ($o, $args) = @_;
        my $spec = $args->[0];

        if($spec ne '<<') { $o->throw("Oops, false configurability exposed...") }

        return [ map { $_->[1]  } sort { my $r = $a->[0] cmp $b->[0]; return $r if $r; $a->[1] cmp $b->[1] } @{$args->[1]} ];
    },

    'shuffle-list/usage' => "(shuffle-list -list-) : Return a randomly shuffled form of -list-.",
    'shuffle-list' => sub {
        my ($o, $args) = @_;
		my ($list) = @$args;
        # print STDERR " >> map-list($o, $a)\n";

        # return [ map { $_->[1] } sort { $a->[0] <=> $b->[0] } map { [ rand, $_ ] } @{$args->[0]} ];
        return $o->shuffleList($list);
    },


    #------------------------------------------------------------------------------------------

    'throw/usage' => "(throw -expr-) : Throw -expr- as an exception.",
    'throw' => sub {
        my ($o, $args) = @_;
        $o->throw($args->[0]);

		# NOTREACHED #
    },


    'return/usage' => "(return -expr-) : Return -expr- from within a function.",
    'return' => sub {
        my ($o, $args) = @_;
        _return @$args;
    },

    #------------------------------------------------------------------------------------------

    'desc-to-string/usage' => "(desc-to-string   -desc-   [options]) : Return serialized form of -desc-.",
    'desc-to-string' => sub {
        my ($o, $args) = @_;
        # print STDERR " >> desc-to-string($o, $args)\n";

        my ($val, $options) = @$args;

        sub _preserialize {
            my ($o, $val) = @_;
            return if ref $val eq '';
               if(ref $val eq 'ARRAY' ) { map { $o->_preserialize($_) } @$val; }
            elsif(ref $val eq 'HASH'  ) { map { $o->_preserialize($val->{$_}) } keys %$val; }
            elsif(JSON::is_bool $val  ) {  }
            elsif(ref $val eq 'SCALAR') { $_[1] = sprintf '(sym-from-string \'%s\')', $o->eval('(sym-to-string $_val)', { _val => $val }); }

            else { $o->throw("_preserialize: Shouldn't get here.") }
        };

        $o->_preserialize($val);

        my $j = JSON->new->allow_blessed->allow_nonref->convert_blessed;

		if($options->{ascii}) {
			# Safer, but JSONview mis-renders Unicode strings.
            $j = $j->ascii;
		}

		if($options->{compact}) {
            ;
		}
		else {
			# JSONview likes this, but there may be encoding issues later, or not...
            $j = $j->pretty->canonical;
		}

        my $str = eval { $j->encode($val) };

        my $e = $@;
        if($e) {
			# _report("2. val: $val");
            _throw("$e\n$val");
        }

        return $str;
    },

    'desc-from-string/usage' => "(desc-from-string -string- [options]) : Return the descriptor deserialized from -string-.",
    'desc-from-string' => sub {
        my ($o, $args) = @_;
        # print STDERR " >> desc-from-string($o, $args)\n";
        return unless defined $args->[0] && $args->[0];

        $o->{_json_} = $args->[0];

        my $desc = eval { from_json $args->[0], { allow_nonref => 1, utf8 => 1 } };
        my $e = $@;
        if($e) {
            # _report "Exception in (desc-from-string): JSON: $e\n$args->[0]";
            _throw($e);
        }

        return unless defined $desc;

        sub _postdeserialize {
            my ($o, $val) = @_;
            if(ref $val eq '') {
                unless(defined $val) { _throw("undefined value here from '$o->{_json_}'") }

                if($val =~ /^\(sym-from-string '.*'\)$/) {
                    $_[1] = $o->eval($val, { });
                }
            }
			elsif($val eq 'true' ) { $_[1] = 1 }
			elsif($val eq 'false') { $_[1] = 0 }
			elsif($val eq 'null' ) { $_[1] = undef }
            elsif(ref $val eq 'ARRAY' ) { map { defined $_         && $o->_postdeserialize($_)         } @$val      }
            elsif(ref $val eq 'HASH'  ) { map { defined $val->{$_} && $o->_postdeserialize($val->{$_}) } keys %$val }

            else { $o->throw("_postdeserialize: Shouldn't get here. val: $val") }
        };

        unless(defined $desc) { $o->throw("undefined desc here") }
        $o->_postdeserialize($desc);

        return $desc;
    },


    #------------------------------------------------------------------------------------------

    'init-interpreter/usage' => '(init-interpreter) : Define basics in a new namespace.',
    'init-interpreter' => sub {
        my ($o, $args) = @_;
        return $o->initInterpreter;
    },

    'debug/usage' => '(debug -arg-) : Set/clear debug flag.',
    'debug' => sub {
        my ($o, $args) = @_;
        if(!@$args) { return $o->{debug} }
        $o->{debug} = $args->[0];
    },

    'oncomma/usage' => '(oncomma -arg-) : Set/get behaviour on seeing a comma.',
    'oncomma' => sub {
        my ($o, $args) = @_;
        if(!@$args) { return $o->{oncomma} }
        $o->{oncomma} = $args->[0];
    },

    #------------------------------------------------------------------------------------------

    'while/usage'        => "(while -cond- -body- ...) : Eval -cond-, if true eval -body- expresions, repeat. Otherwise return undefined.",
    'catch/usage'        => "(catch -body- -handler-) : Eval -body-, return its value if no exception. Otherwise var _err is set to the exception, -handler- is eval'd, and its value returned.",
    'fn/usage'           => "(fn -params- -body-) : Function literal. Returns itself when eval'd.",
    'lambda/usage'       => "(lambda -params- -body-) : Function literal. Returns itself when eval'd.",

    'if/usage1'           => "(if -cond- -body-)                          : Eval -cond-, if true eval -body-.",
    'if/usage2'           => "(if -cond- -bodyT- -bodyF-)                 : Eval -cond-, if true eval -bodyT-, otherwise eval -bodyF-.",
    'if/usage3'           => "(if -cond1- -body1- -cond2- -body2-)        : Eval -cond1-, if true eval -body1-, otherwise eval -cond2- ...",
    'if/usage4'           => "(if -cond1- -body1- -cond2- -body2- -body-) : Eval -cond1-, if true eval -body1-, otherwise ... eval -body-.",

    'expr-to-list/usage' => "(expr-to-list -expr-) : Return a list of the subexpressions in -expr-.",


    ## Place holders for (builtins). to be filled in with code factored from eval()...
    'while' => undef,
    'catch' => undef,
    'if'    => undef,
    'fn'    => undef,
    'lambda'    => undef,
    'expr-to-list' => undef,
    ## Place holders for (builtins). to be filled in with code factored from eval()...

    #------------------------------------------------------------------------------------------
};

#------------------------------------------------------------------------------------------

sub
DPL_login {
    my ($o, $workspaceName) = @_;
    # printf STDERR " >> DPL_login(%s)\n", defined $workspaceName? $workspaceName: '<undef>';

    #  Read in modules.
    $o->evalAll('(desc-read _ dplx) (desc-read _ user)', {});

    # Get top level workspace names before authenticating...
    my $workspaceNameList = [ '/' ];
    push @$workspaceNameList, map { "/$_" } @{$o->eval('(ii-list *)', {})};

    #  Login, adjust workspace.
    my $data = $o->evalAll('(= workspace-name $wn) (dpl-login)', {wn => $workspaceName});
    
    $workspaceName = $data->{'workspace-name'};
    $userid        = $data->{userid};
    my $auth       = $data->{auth};

    # printf STDERR " >> DPL_login() workspaceName: %s, userid: %s\n", (defined $workspaceName? $workspaceName: '<undef>'), (defined $userid? $userid: '<anon>');


    # /                 .
    # /NAME             .ii/NAME
    # ~                 .ii-user/MYUSERID
    # ~/NAME            .ii-user/MYUSERID/.ii/NAME
    # ~USERID           .ii-user/USERID
    # ~USERID/NAME      .ii-user/USERID/.ii/NAME

    #                       1    2         3  4
    if($workspaceName !~ m/^(\/|~([^\/]*))?(.*(\/.+)*)$/) {
        $o->throw("Invalid workspace name '$workspaceName'.");
    }

    my ($prefix, $altUserid, $wkPath) = ($1, $2, $3);

    if(defined $prefix) {
        if($prefix =~ /^~(.+)$/) {
            $o->throw("Invalid workspace name '$workspaceName', unsupported.");
        }

        if($prefix ne '/' && $prefix ne '~' ) {
            $o->throw("Invalid workspace name '$workspaceName', unexpected.");
        }
    }

   $prefix = '' unless defined $prefix;


    # Convert DPL workspace name to PrimaLisp inner interpreter name.
    $workspaceName = $wkPath;

    if($workspaceName =~ s/^\///) {
        $o->throw("Assertions fail... prefix: '$prefix'") unless $prefix eq '~';
        $prefix = "$prefix/";
    }


    #  Switch to inner interpreter for a named workspace other than '/'. 
    my $pl = $o;

    # /                 .
    # /NAME             .ii/NAME
    # ~                 .ii-user/MYUSERID
    # ~/NAME            .ii-user/MYUSERID/.ii/NAME
    # ~USERID           .ii-user/USERID
    # ~USERID/NAME      .ii-user/USERID/.ii/NAME

    if(defined $userid) {
        push @$workspaceNameList, '~';
    }

    my $opt = {'ii-user' => 1};

    if($prefix eq '/') {
        my $pl_tmp = $pl->evalAll('(ii-new \'\' $opt)', { opt => $opt});
        push @$workspaceNameList, map { "~/$_" } @{$pl_tmp->eval('(ii-list *)', {})};

        if($workspaceName eq '') {
            ;
        }
        else {
            $pl = $pl->evalAll('(ii-new $wn)', {wn => $workspaceName});
        }
    }
    elsif($prefix eq '~') {
        $pl = $pl->evalAll('(ii-new \'\' $opt)', { opt => $opt});
        push @$workspaceNameList, map { "~/$_" } @{$pl->eval('(ii-list *)', {})};
    }
    elsif($prefix eq '~/') {
        my $pl_tmp = $pl->evalAll('(ii-new \'\' $opt)', { opt => $opt});
        push @$workspaceNameList, map { "~/$_" } @{$pl_tmp->eval('(ii-list *)', {})};

        $pl = $pl->evalAll('(ii-new $wn $opt)', {wn => $workspaceName, opt => $opt});
    }

    if(defined $userid) {
        if($prefix ne '/') {
     #       push @$workspaceNameList, map { "~/$_" } @{$pl->eval('(ii-list *)', {})};
        }
    }

    # Convert PrimaLisp inner interpreter name to DPL workspace name.
    $workspaceName = "$prefix$workspaceName";
    $workspaceName = (defined $userid? '~': '/') if $workspaceName eq '';

    #  Set workspace name and get our userid.
    $userid = $o->evalAll('(= workspace-name $wn) (login)', {wn => $workspaceName});


    return ($pl, $auth, $userid, $workspaceName, $workspaceNameList);
}






#------------------------------------------------------------------------------------------
#X# doc_Method(hashLookup, <|
# # $o->hashLookup($desc, $path, $name, [ $value, [$clear, [$incr]]]);
# # |>, <|
# #  This method is used to look up and manipulate target values in a nested hash -desc-.
# #  -path- and -name- identify the target location within the hash for the operation.
# #  If no other arguments are given, then the value at the target is returned.
# #  If one other argument, -value-, is given, it is stored at the target, and that value returned.
# #  If two other arguments are given, the first must be undefined and the second a defined value. 
# #     In this case, the target is removed from the hash and its value returned.
# #  If three other arguments are given, the first two must be undefined and the third is a value
# #    by which the target value is incremented.
#X# |>)
sub
hashLookup {
    my ($o, $desc, $path, $name, $value, $clear, $incr) = @_;
    # my $name0 = $name; $name0 = '<undef>' unless defined $name0;
    # my $value0 = $value; $value0 = '<undef>' unless defined $value0;
    # print STDERR " >> hashLookup($desc, $path, $name0, $value0)\n";

    # TODO
    # handle simple hash lookup (hash-get $Hash $Name) -> (hash-get $Hash / $Name)
    #if(@_ == 3) { $name = $path; undef $path }
    $path = '/' unless defined $path;

    return unless defined $desc && (ref $desc eq 'HASH'  || (scalar $desc) =~ /=?HASH\(/ ) &&
                  defined $path && (ref $path eq 'ARRAY' || $path =~ m~^/~               ) &&
                  1;

	if(ref $path ne 'ARRAY') {
		$path = [ split '/', $path ];
		shift @$path;
	}
	else {
		$path = [ @$path ];
	}

    while(@$path && $desc) {
        my $name = shift @$path;
        if(!exists $desc->{$name}) {
            return unless defined $value;

            $desc->{$name} = { };
        }

        $desc = $desc->{$name};
    }

	my $clearedValue;

    if(defined $name) {
           if(defined $value) {                 $desc->{$name} = $value               }
        elsif(defined $clear) { $clearedValue = $desc->{$name}; delete $desc->{$name} }
        elsif(defined $incr ) {                 $desc->{$name} += $incr               }
    }

    return defined $name?
                (exists $desc->{$name}?
                    $desc->{$name} :
                    $clearedValue
                ) :
                 $desc;
}


#--------------------------------

#X# doc_Method(descFromString, <|
# # $o->descFromString($string);
# # |>, <|
#X# |>)
sub
descFromString {
    my ($o, $string) = @_;
    # print STDERR " >> descFromString($string)\n";

    return $o->{builtinsMap}->{'desc-from-string'}->($o, [ $string ]);
}

#X# doc_Method(descToString, <|
# # $o->descToString($desc);
# # |>, <|
#X# |>)
sub
descToString {
    my ($o, $desc) = @_;
    # print STDERR " >> descToString($desc)\n";

    return $o->{builtinsMap}->{'desc-to-string'}->($o, [ $desc ]);
}






#X# doc_Method(getDescKeys, <|
# # $o->getDescKeys($desc, $path);
# # |>, <|
#X# |>)
sub
getDescKeys {
    my ($o, $desc, $path) = @_;
    # print STDERR " >> getDescKeys($desc, $path)\n";

    my $map = $o->hashLookup($desc, $path);

    my $path0 = $path; $path0 =~ s~/$~~;    # /

    my $builtinsMap  = $o->{builtinsMap};

    $builtinsMap->{'var-push'}->($o, [ 'self', $desc ]);

    my $superMap;

    while(1) {
        my $referTo = $builtinsMap->{'hash-eval'}->($o, [ $desc, '/', 'refer-to' ]);
        last unless defined $referTo;

        # print STDERR " -- Looking up map $path0 in $referTo\n";

        $builtinsMap->{'desc-read'}->($o, [ 'self', $referTo ]);
        $desc = $o->{varMapNS}->{self}->[0];
        last unless defined $desc;

        $superMap = $o->hashLookup($desc, $path);

        map { if(!exists $map->{$_}) { $map->{$_} = $superMap->{$_} } } keys %$superMap;
    }


    $builtinsMap->{'var-pop'}->($o, [ 'self' ]);

    return [ keys %$map ];
}



#X# doc_Method(referToLookup_inner, <|
# # $o->referToLookup_inner($desc, $path, $name);
# # |>, <|
#X# |>)
sub
referToLookup_inner {
    my ($o, $desc, $path, $name) = @_;
    # _report "No path found for '$name', assuming '/'" unless defined $path;

    #  Why was this commented out?
    $path = '/' unless defined $path;
    #  Why was this commented out?

    # print STDERR " >> referToLookup_inner($desc, $path, $name)\n";

    my $path0 = $path; $path0 =~ s~/$~~;    # /

    my $builtinsMap  = $o->{builtinsMap};

    $builtinsMap->{'var-push'}->($o, [ 'self', $desc ]);

    my $value;

    while(!defined $value) {
        my $referTo = $builtinsMap->{'hash-eval'}->($o, [ $desc, '/', 'refer-to' ]);
        last unless defined $referTo;

        $referTo = [ $referTo ]  unless ref $referTo eq 'ARRAY';

        # Multiple inheritance.
        for my $parent (@$referTo) {

            # print STDERR " -- Looking up $path0/$name in $parent\n";

            $builtinsMap->{'desc-read'}->($o, [ 'self', $parent ]);
            $desc = $o->{varMapNS}->{self}->[0];
            if(!defined $desc) {
                _report "Descriptor '$parent' wasn't found.";
                last;
            }

            $value = $builtinsMap->{'hash-eval'}->($o, [ $desc, $path, $name ]);
            last if defined $value;
        }

        last unless defined $desc;
    }

    $builtinsMap->{'var-pop'}->($o, [ 'self' ]);

    return $value;
}



#X# doc_Method(referToLookup, <|
# # $o->referToLookup($name);
# # |>, <|
#X# |>)
sub
referToLookup {
    my ($o, $name, $value) = @_;
    # my $value0 = $value; $value0 = '<undef>' unless defined $value0;
    # print STDERR " >> referToLookup($name, $value0)\n";

    return $value if defined $value;

    my $desc = $o->{varMapNS}->{self}->[0];
    return unless defined $desc;

    # See definitions of (def-{hash,desc}-fn)
    my $path = $o->{varMapNS}->{"$name/path"}->[0];

    # return unless defined $path;
    # print STDERR " -- path: '$path'\n";

    return $o->referToLookup_inner($desc, $path, $name);
}

#--------------------------------

#X# doc_Method(getFullPathFromPath, <|
# # $o->getFullPathFromPath($path);
# # |>, <| This implement searching through various desc-dirs...
# #   Return full path relative to top interpreter.
#X# |>)
sub
getFullPathFromPath {
	my ($o, $path) = @_;
	# print STDERR " >> $Pname: getFullPathFromPath($path)\n";

   
    # Look in current descriptor dir
    my $fullPath = "$o->{descDir}/$path";

    my @s = stat $fullPath;
    if(@s) { return $fullPath }

    
    # Look in current inner descriptor dir
    my @fullPathList = glob "$o->{descDir}/.ii/lib-*/$path";
    if(@fullPathList) {
        $fullPath = shift @fullPathList;
        if(@fullPathList) { _report(['Leaving some paths behind: %s', join(':', @fullPathList)]) }

        @s = stat $fullPath;
        if(@s) { return $fullPath }
    }

    # Look in outer interpter...
    if(exists $o->{outerInterp}) { return $o->{outerInterp}->getFullPathFromPath($path) }

	return;
}




#--------------------------------


sub _interpolate_ {
    my ($o, $vars, $pre, $var, $post) = @_;
    my $val = $o->eval($var, $vars);
    return (defined $pre? $pre: '') . (defined $val? $val: '') . (defined $post? $post: '');
};


my $_eadepth = 0;


#X# doc_Method(evalAll, <|
# # $o->evalAll($expr, $vars);
# # |>, <|
#X# |>)
sub
evalAll {
    my ($o, $expr, $vars) = @_;
    # print STDERR sprintf(" >>%s evalAll(%s..., %s)\n", ('>' x $_eadepth++), substr(defined $expr? $expr: '()', 0, 300), $vars);
	# if(ref $expr eq 'ARRAY') { map { print STDERR " -- %s\n", $_ } @$expr }
	# if(exists $o->{varMapNS}->{'.'}) { printf STDERR " ** \n%s\n ** \n", join("\n",  @{$o->{varMapNS}->{'.'}}) }

	my $value;

	if(defined $expr) {

		$vars = { } unless defined $vars;

		if(ref $expr eq 'ARRAY') {
			map { $value = $o->eval($_, $vars) } @$expr;
		}
		else {
			# build a cache a list here for use above...
			while(defined $expr && $expr ne '') {
				($value, $expr) = $o->eval($expr, $vars);
			}
		}
	}

	# --$_eadepth;
	# print STDERR sprintf(" <<%s evalAll() returns\n", ('<' x $_eadepth));

    return $value;
}


my %closeDelim = ( '(' => ')', '[' => ']', '{' => '}' );
my %fnDelim = ( '(' => undef, '[' => 'list', '{' => 'hash' );


#X# doc_Method(eval, <|
# # $value = $o->eval($expr [, $vars [, evaling]] );
# # ($value, $after) = $o->eval($expr [, $vars [, evaling]] );
# # And $expr can be a string or list. (currently it can be only a string...)
# # |>, <|
#X# |>)
sub
eval {
    my ($o, $expr, $vars, $evaling) = @_;
    if(exists $o->{debug} && $o->{debug} >= 10) {
        my $expr0 = $expr; $expr0 = '<undef>'  unless defined $expr;
        my $vars0 = $vars; $vars0 = '<undef>'  unless defined $vars;
        _report " >> eval($expr0, $vars0)";
    }

    ++$o->{n_evals};

    $vars = { } unless defined $vars;
    $evaling = 1  unless defined $evaling;
    # map { _report " -- k: $_, v: $vars->{$_}\n" } sort keys %$vars;

    return unless defined $expr;

    $expr =~ s/^\s+//;
    return '' if $expr eq '';
    
    my ($fn, $inst, $match, $value, $after);

    my $builtinsMap  = $o->{builtinsMap};
    my $varMap       = $o->{varMapNS};
    my $ns           = $o->{namespace};


    $pf->click(__LINE__) if defined $pf;

	if(length($expr) > 25400) {
		_report([' !! Warning: Long expresssion (len %d, <|%s|>) might break eval here!', length($expr), substr($expr, 0, 100)]);
	}

    #--------------------------------------------------------------------------------------------------
    # If it's a function call, Collect function name, eval args, call function.
    # if($expr =~ /^(\()/) 
    if($expr =~ /^([[\({])/) {
        ($match, $after) = ($1, $');
        $pf->click(__LINE__) if defined $pf;

		my $openDelim = $match;

        my $matches = [ $match ];
        my $args    = [ ];

        my $oEvaling = $o->{evaling};
        undef $fn;

        my $ismacro = 0;

        while($after) {
            # It would be nice to match a comment preceeding a close paren.

			my $fnDelim = $fnDelim{$openDelim};

			if(defined $fnDelim && !defined $fn) {
				$fn = $fnDelim if defined $fnDelim;
				push @$args, $fn;
			}


            if($after =~ /^\s*([\)}\]])\s*/) { 
				my $match = $1;
				my $clDelim = $closeDelim{$openDelim};
  				if($match ne $clDelim) {
					_report(["Bracket delimter mismatch expression <|%s|>: saw %s, expected %s.", $expr, $match, $clDelim]);
					$o->throw("Bracket delimter mismatch: saw $match, expected $clDelim.");
				}
			}
			else {
                my $savedExpr = $after;

				# Do list interpolation.  For now just handle args of function/method calls.
				my $isListInterpolate = 0;

                my $oEvaling = $o->{evaling};

				# This is how macros can pass unevaluated args.
                $o->{evaling} = 0 if $ismacro;

				if($o->{evaling} && $after =~ s/^\s*@//) { $isListInterpolate = 1 }

				($value, $after) = $o->eval($after, $vars);

                $o->{evaling} = $oEvaling;


                if(!defined $fn) {
                    $fn = $value;  # First expression.
                    # _report(" -- 1. fn: $fn");
                    # $o->{debug} && _report(" -- 1. fn: $fn");


					# If $fn is a hash ref (DPL instance) we need the next arg (method name) to find out if this is a function or macro call.
					if(!defined $inst && ref $fn eq 'HASH' && exists $fn->{'.ismacro'}) {
						$inst = $fn;
						undef $fn;

						# _report("Going back and get the method name... inst: $inst");
						push @$args, $inst;
						next;
					}

					if(defined $inst) {

						$method = $fn;

						# _report("Checking for macroness and cleanup... method: $method");

						if(exists $inst->{'.ismacro'}->{$method}) {
							# _report("Found a macro!! $method");
							$ismacro = 1;
						}

						$fn = $inst;
						undef $inst;

						# _report(["method: %s", $method]); push @$args, $method;

						# _report("Now carrying on with arg processing.... (ismacro: $ismacro)");
						next;
					}



                    $o->throw("'fn' should be defined here and isn't <|$savedExpr|>")
                        unless defined $fn;

                    # We should hold off eval'ing the rest of the args here and wait until
                    # later when we know we're calling a function and need to eval the args.
                    # Doing so would allow proper macros to be implemented
                    # by passing uneval'd args to a function (?).  
                    # We do need to eat up the expression here and evaling it twice
                    # (once in copy mode here and once in eval mode later) would kinda suck for a function call.

					my $attr = "$fn/ismacro";
					# _report(['attr: %s (%s)', $attr, ref $o->{varMapNS}->{$attr}]);

					my $vars = $o->{varMapNS};
					my $bi   = $o->{builtinsMap};

                    $ismacro =
						 (exists($vars->{$attr}) && defined $vars->{$attr}->[0] && $vars->{$attr}->[0] eq '1' ) ||
						 (exists($bi  ->{$attr}) && defined $bi  ->{$attr}      && $bi  ->{$attr}      eq '1' ) ;


                    # _report([" -- fn: %s, ismacro: %s, value: %s", $fn, $ismacro, $value]);
                    if($fn =~ /^(fn|lambda|λ|if|while|catch|expr-to-list)$/) {
                        # Turn of evaling for remainder of this expression.
                        $o->{evaling} = 0;
                    }

                    if($fn eq 'fn' || $fn eq 'lambda' || $fn eq 'λ') {
                        $o->{lambda} = 1;
                        my $n = ++$o->{lambdaSeq};
                        # _report(['saw fn %d, expr %s', $n, $expr]);
                    }
                }


                0 && print STDERR " -- ", ($ismacro? 'eval': 'copy'), " collecting args: <|$value|> <|$savedExpr|>\n";
                0 && print STDERR " -- ", ($ismacro? 'eval': 'copy'), " collecting args: <|$value|>\n";

				# Now we're into the args if any.

				if(exists $o->{multi_value_return}) {
					push @$args, @{$o->{multi_value_return}};
					delete $o->{multi_value_return};
				}
				elsif($isListInterpolate) {
					if(defined $value) {
						   if(ref $value eq 'ARRAY') { push @$args, @$value if @$value }
						elsif(ref $value eq 'HASH' ) { push @$args, %$value if %$value }

						else { $o->throw("Unable to interpolate on value '$value'") }
					}
				}
				else {
					push @$args, $value;
				}

                next;
            }


			#  Now we have the args marshalled, we can call the function/macro.


            0 && printf STDERR " ## %s/%s function call: <|(%s)|>\n",
                    (!$o->{lambda}? 'eval': 'copy'),
                    ($o->{evaling}? 'eval': 'copy'),
                    join(' ', map {"$_"} @$args); 

            $after = $';

            if($o->{evaling} && !@$args) {
                $o->{evaling} = $oEvaling;
                return wantarray? (undef, $after): undef;
            }


            unless($o->{evaling} || (defined $fn && ($fn =~ /^(if|while|catch|expr-to-list)$/))) {
                $o->{evaling} = $oEvaling;
                $match = '(' . join(' ', map { defined $_? $_: '()' } @$args) . ')';
				# _report(['match: %s   %s:%d', $match, __FILE__, __LINE__]);
                return wantarray? ($match, $after): $match
            } 



            $o->{evaling} = $oEvaling;

            # Call the function now, return its result.

            # $o->{debug} && _report(" -- 2. fn: $fn");
            $fn = shift @$args;
            # $o->{debug} && _report(" -- 3. fn: $fn");

            if($o->{evaling}) {

                if($fn eq 'if') {
                    my $value;

                #   my $oEvaling = $o->{evaling};
                #   $o->{evaling} = 1;
					unshift @{$varMap->{'.'}}, "x $fn $ns";

					eval {
						while(@$args) {
							my $pred  = shift @$args;
							my $bodyT = shift @$args;

							last if !defined $bodyT;

							if($o->eval($pred, $vars)) {
								$value = $o->eval($bodyT, $vars);
								last;
							}
			
							if(@$args > 0) {
								if(@$args == 1) {
									my $bodyF = shift @$args;
									$value = $o->eval($bodyF, $vars);
									last;
								}
							}
						}
					};

					shift @{$varMap->{'.'}};

					die $@ if $@;


                #   $o->{evaling} = $oEvaling;

                    return wantarray? ($value, $after): $value;
                }
                elsif($fn eq 'while') {
                    my $pred = shift @$args;
                    my $body = $args;

                    my $oEvaling = $o->{evaling};
                    $o->{evaling} = 1;

					unshift @{$varMap->{'.'}}, "x $fn $ns";

                    my $value;
                    eval { 
						while($o->eval($pred, $vars)) {
							map { $value = $o->eval($_, $vars) } @$body;
						}
					};

					shift @{$varMap->{'.'}};

					die $@ if $@;

                    $o->{evaling} = $oEvaling;

                    return wantarray? ($value, $after): $value;
                }
                elsif($fn eq 'catch') {
                    my $body    = shift @$args;
                    my $handler = $args;

                    my $oEvaling = $o->{evaling};
                    $o->{evaling} = 1;

                    # $builtinsMap->{'var-clear'}->($o, [ '_err' ]);
                    delete $varMap->{_err};
                    delete $varMap->{_cs};
                    delete $o->{'callstack-at-throw'};

					unshift @{$varMap->{'.'}}, "x $fn $ns";

                    my $value = eval { $o->eval($body, $vars) };

					shift @{$varMap->{'.'}};

                    my $e = $@;

                    if($e) {
                        # _report "Exception in (catch): v: $e->{value}"     if defined $e->{value};

                        if(ref $e eq 'HASH') {
                            # It's a meta exception (one of ours.)
                            # map { _report " -> $_ : $e->{$_}" }  sort keys %$e;

                               if(exists $e->{value}    ) { _return $e->{value} }
                            elsif(exists $e->{values}) {
								my $V = $o->{multi_value_return} = $e->{values};
								my $value = pop @$V; push @$V, $value;	# get last value.

							    _return $value;
					    	}
                            elsif(exists $e->{exception}) { # $builtinsMap->{'var-set'}->($o, [ '_err', $e->{exception} ])
                            }
                            else { $o->throw("Unexpected meta-exception in (catch): $e; " . join(',', map { "$_=$e->{$_}" } sort keys %$e)) }
                        }

                        # Rethrow regular exception (something from perl) as a meta exception.
                        else { $o->throw($e) }

						  my $env = {_err => $e->{exception} };
						# my $env = { };

                        # $builtinsMap->{'var-set'}->($o, [ '_err', $e->{exception} ]);
			            $o->{varMapNS}->{_err}->[0] = $e->{exception};
			            $o->{varMapNS}->{_cs}->[0]  = $e->{callstack};

                        $value = $o->evalAll($handler, $env);

                        # $builtinsMap->{'var-clear'}->($o, [ '_err' ]);
                    }

                    $o->{evaling} = $oEvaling;

                    return wantarray? ($value, $after): $value;
                }
                elsif($fn eq 'expr-to-list') {
                    my $value;

					unshift @{$varMap->{'.'}}, "x $fn $ns";
#-----------------------
if(1) {
  eval {
                    if(defined $o->{_el_}) {
                        $value = $args;
                    }
                    else {
                        my $body = $args->[0]; $body =~ s/^\(//; $body =~ s/\)$//;

                        $o->{_el_} = 1; # NOT RE-ENTRANT ...
                        my $oEvaling = $o->{evaling};
                        $o->{evaling} = 1;
                        $value = $o->eval("(expr-to-list $body)", $vars);
                        $o->{evaling} = $oEvaling;
                        delete $o->{_el_};
                    }
  }
}
#-----------------------
else {
  eval {
                    my $body = $args->[0]; $body =~ s/^\(//; $body =~ s/\)$//;
                    _report [ 'args: %s', join(', ', @$args) ];
                    _report [ 'body: %s', $body ];

                    my $oEvaling = $o->{evaling};
                    $o->{evaling} = 1;
                    $value = $o->eval("(list $body)", $vars);
                    $o->{evaling} = $oEvaling;
  }
}
#-----------------------

					shift @{$varMap->{'.'}};

					die $@ if $@;

                    return wantarray? ($value, $after): $value;
                }
            }
            else {
                if($fn =~ /^(if|while|catch|expr-to-list)$/) {
                    my $value = sprintf "(%s %s)", $fn, join(' ', @$args);
            
                    return wantarray? ($value, $after): $value;
                }
            }

            $o->{evaling} = $oEvaling if $o->{lambda};

            $o->{lambda} = 0;

            my $fnRef;
            # _report("fn: $fn");

            if($fn =~ /^\(\s*(fn|lambda|λ)\s/) {
                # $fn is a lambda expression.
                $fnRef = $fn;
            }
            elsif(ref $fn && scalar($fn) =~ /=/) {
                # $fn is a interpreter builtin lanugage object method call.
                my $method = shift @$args;
                $o->throw("Invalid method name '$method'")
                    unless $method =~ /^\w+$/;

                my $code = sprintf('$fn->%s(@$args)', $method);
                # _report(" %> $code");
				unshift @{$varMap->{'.'}}, "  $method $fn";

                my $value = eval $code;

				shift @{$varMap->{'.'}};
                # _report(" -- value: $value");
                warn "Exception: $@, line: ", __LINE__  if $@;
                $o->throw($@) if $@;
                return wantarray? ($value, $after): $value;
            }
            elsif(ref $fn eq 'CODE') {
				$fnRef = $fn;
			}
            elsif(ref $fn eq 'HASH') {
                # $fn is a DPL object/instance method call.

                my $env        = { _obj => $fn };
                #  my $env        = $fn;  # might work better some day.
				# would be nice if instance vars could be accessed just like regular vars...
				#  for this, we really need a list of envs to look through before going to the var table.

                my $dpl_args0  = [ ];
                my $dpl_args11 = [ ];
                my $dpl_args12 = [ ];
                my $dpl_args2  = [ ];

				# _report("ismacro: $ismacro");

                my $n = 1;
                for my $arg (@$args) {
                    $env->{"_$n"} = $arg;
                    push @$dpl_args0, "_$n";
                    push @$dpl_args11, ($ismacro? $arg: "\$_$n");
                    if(!$ismacro) { push @$dpl_args12, "\$_$n" }
                    push @$dpl_args2, "_$n \$_$n";
                    ++$n;
                }

				### Hmm, how do we implement method calls to a macro??

                # map { _report("dpl_args0: $_") } @$dpl_args0;
                # map { _report("dpl_args11: $_") } @$dpl_args11;
                # map { _report("dpl_args12: $_") } @$dpl_args12;
                # map { _report("dpl_args2: $_") } @$dpl_args2;


#------------------
#                 my $code = sprintf '(var-push self $obj) (= val (%s)) (var-pop self) $val', join(' ', @$dpl_args11);
#                 # _report("ODispatch code: '$code'");
#                 # map { _report("env: $_ : $env->{$_}") } keys %$env;
# 
#                 if(exists $fn->{'.class'} && (my $ns = $fn->{'.class'}) ne '') {
#                     $code = sprintf '((fn (obj %s -- val) (namespace %s (hash obj $obj val $val %s) %s)) $_obj %s)',
#                                                                     join(' ', @$dpl_args0), $ns,join(' ', @$dpl_args2), $code, join(' ', @$dpl_args12);
#                 }
#                 else {
#                     $code = sprintf '((fn (obj %s -- val)                                           %s ) $_obj %s)',
#                                                                     join(' ', @$dpl_args0), $code, join(' ', @$dpl_args12);
#                 }
#------------------
                my $code = sprintf '(var-push Self $self) (var-push self $obj) (= val (%s)) (var-pop self)(var-pop Self) $val', join(' ', @$dpl_args11);
                # _report("ODispatch code: '$code'");
                # map { _report("env: $_ : $env->{$_}") } keys %$env;

                if(exists $fn->{'.class'} && (my $ns = $fn->{'.class'}) ne '') {
                    $code = sprintf '((fn (obj %s -- val) (namespace %s {self $self obj $obj val $val %s} %s)) $_obj %s)',
							                #   1                    2                                3   4          5
											#   1                       2    3                       4      5
                                                join(' ', @$dpl_args0), $ns, join(' ', @$dpl_args2), $code, join(' ', @$dpl_args12);
                }
                else {
                    $code = sprintf '((fn (obj %s -- val) %s ) $_obj %s)',
											#  1          2          3
											#  1                       2      3
                                               join(' ', @$dpl_args0), $code, join(' ', @$dpl_args12);
                }
#------------------

                # _report(" %> $code");
                $value = eval { $o->evalAll($code, $env) };
                # _report(" == value: $value");
                if($@) {
                    # _throw(sprintf("Exception(%s): %s", $@->{exception}, __LINE__));
                    $o->throw($@->{exception});
                }
                return wantarray? ($value, $after): $value;
            }
            else {
                # $fn is the variable name refering to the function.
				# $fnRef is a lambda expression or CODE ref, etc. implementing the function.
				#------------------------------------------------------------------------------------
                $fnRef = $vars->{$fn}        if   defined $vars;  # caller provided
                $fnRef = $varMap->{$fn}->[0] if exists $varMap->{$fn} && ! defined $fnRef; # default
                $fnRef = $builtinsMap->{$fn} if ! defined $fnRef; # overridable
                $fnRef = $builtinsMap->{encode_utf8 $fn} if ! defined $fnRef; # overridable

                $fnRef = $o->referToLookup($fn, $fnRef) if !defined $fnRef;
                #   print STDERR " -- fn: $fn, fnRef: '$fnRef'\n";
                # _report([" fn: %s, fnRef: '%s'", $fn, $fnRef]);;

				# If the function name looks like a number, we'll make it eval to a number.
				if(!defined $fnRef && $fn =~ /^(-|\+)?\d+(\.\d*)?$/) {
					$fnRef = sub { 0+$fn };
				}

				# If the function name begins or ends with ++ or --, treat the remainder as the variable name and do the incr/decr.
				if(!defined $fnRef && $fn =~ /^(--|\+\+)?(.+?)(--|\+\+)?$/) {
					   if( defined $1 && !defined $3) { $fn = $fnRef = sprintf '(fn () (%s_ %s))', $1, $2 }
					elsif(!defined $1 &&  defined $3) { $fn = $fnRef = sprintf '(fn () (_%s %s))', $3, $2 }
				}

                # If the function name starts with a qualified descriptor name, look it up, cache it and call it in its own namespace
                if(!defined $fnRef && $fn =~ /^(((([^:]+):)?([^:]+):)?([^:]+):)?([^:]+):([^:]+)$/) {
                                            #  1234         5         6         7       8
                    my ($hostSpec, $userSpec, $wkspSpce, $descName, $fnName) = ($4, $5, $6, $7, $8);

                    _report([' !! Unimplemented pseudo function...']);
                    _report(['hostSpec: %s, userSpec: %s, wkspSpce: %s, descName: %s, fnName: %s',
                             $hostSpec,    $userSpec,    $wkspSpce,    $descName,    $fnName]);
                    return;
                }

                if(!defined $fnRef) {
                    $fnRef = $varMap->{'-undefined-function-handler-'}->[0]; # last chance handler.
                    if(defined $fnRef) { unshift @$args, $fn }
                }

				if(!defined $fnRef) {
                    # my $bik = [ sort keys %$builtinsMap ];
					# if($XXXXX++ > 5) { $o->eval('(emit (desc-to-string $bik))', {bik => $bik}); }
					$o->eval('(report %s (desc-to-string (callstack)))', {});
					$o->throw("An undefined function '$fn' was called.");
				}
				#------------------------------------------------------------------------------------

				# If the fnRef isn't found, let's assume what we have is a variable name and see where that goes...

                while(!ref $fnRef && $fnRef !~ /^\(\s*(fn|lambda|λ)\s/) {
                    $o->throw("Oh sure, an infinite lookup for '$fn'") if $fnRef eq $fn;

                    $fn = $fnRef;

					## Smells like refactoring in the morning...
                
					#------------------------------------------------------------------------------------
                    $fnRef = $vars->{$fn}        if   defined $vars;  # caller provided
                    $fnRef = $varMap->{$fn}->[0] if ! defined $fnRef; # default
                    $fnRef = $builtinsMap->{$fn} if ! defined $fnRef; # overridable
                    $fnRef = $builtinsMap->{encode_utf8 $fn} if ! defined $fnRef; # overridable

                    $fnRef = $o->referToLookup($fn, $fnRef) if !defined $fnRef;
                    #   print STDERR " -- fn: $fn, fnRef: '$fnRef'\n";

					# If the function name looks like a number, we'll make it eval to a number.
					if(!defined $fnRef && $fn =~ /^(-|\+)?\d+(\.\d*)?$/) {
						$fnRef = sub { 0+$fn };
					}

					# If the function name begins or ends with ++ or --, treat the remainder as the variable name and do the incr/decr.
					if(!defined $fnRef && $fn =~ /^(--|\+\+)?(.+?)(--|\+\+)?$/) {
						   if( defined $1 && !defined $3) { $fn = $fnRef = sprintf '(fn () (%s_ %s))', $1, $2 }
						elsif(!defined $1 &&  defined $3) { $fn = $fnRef = sprintf '(fn () (_%s %s))', $3, $2 }
					}

                    # If the function name starts with a qualified descriptor name, look it up, cache it and call it in its own namespace
                    if(!defined $fnRef && $fn =~ /^(((([^:]+):)?([^:]+):)?([^:]+):)?([^:]+):([^:]+)$/) {
                                                #  1234         5         6         7       8
                        my ($hostSpec, $userSpec, $wkspSpce, $descName, $fnName) = ($4, $5, $6, $7, $8);

                        _report([' !! Unimplemented pseudo function...']);
                        _report(['hostSpec: %s, userSpec: %s, wkspSpce: %s, descName: %s, fnName: %s',
                                 $hostSpec,    $userSpec,    $wkspSpce,    $descName,    $fnName]);
                        return;
                    }

                    if(!defined $fnRef) {
                        $fnRef = $varMap->{'-undefined-function-handler-'}->[0]; # last chance handler.
                        if(defined $fnRef) { unshift @$args, $fn }
                    }

					if(!defined $fnRef) {
						$o->eval("(report %s (desc-to-string (callstack)))", {});
						$o->throw("An undefined function '$fn' was called..");
					}
					#------------------------------------------------------------------------------------
                }
            }



            if(ref $fnRef eq 'CODE') {
                # Function is a builtin function. call it.
				unshift @{$varMap->{'.'}}, "i $fn $ns";
                $value = eval { $fnRef->($o, $args, $vars) };
				shift @{$varMap->{'.'}};

				if($@) { die $@ }

                # print STDERR " -- value: '$value'\n";
            }
            elsif($fnRef =~ /^\(\s*(fn|lambda|λ)\s/) {
                my $afterL = $';

                my $params;

                # Read param list in copy not eval mode.
                my $oEvaling = $o->{evaling};
                $o->{evaling} = 0;
                ($params, $afterL) = $o->eval($afterL, $vars);
                $o->{evaling} = $oEvaling;


                $o->throw("Unexpected params: $params")
                    unless $params =~ /^\((.*)\)$/;

                $params = $1;


                my @params0 = split /\s+/, $params;
 
                my @params = ();
                my @closed = ();
 
                my $isInClosed;
                foreach my $paramName (@params0) {
                    if(!defined $isInClosed) {
                        if($paramName ne '--') { push @params, $paramName }
                        else { $isInClosed = 1 }
                    }
                    else { push @closed, $paramName }
                }


                map {
                    my $value;
                    if(defined $args) {
                        if(!s/^&//) { $value = shift(@$args)      }    # common case, no &var in param list
                        else        { $value = $args; undef $args }    # first time seeing &var
                    }

                    unshift @{$varMap->{$_}}, $value;
                } @params;


                # My plan is to introduce closures by calling variables after a '--' in the params list 'closed' variables.
                # (I guess the formal params are open)
                # (i.e. (fn (-formal- -- -closed-) ...)
                # There should be no compatibility issues with introducing this change.
                # That naming conventions currently indicates the end of formal params and beginning of 'private' variables

                # The value a so-named variable has when a function is defined
                # (i.e. when a lambda expression (fn (-formal- -- -closed-) ...) is evaluated)
                # will be in effect when that function is called.
                # After the function returns, the so-named values will be saved and
                # passed back to the function the next time it is called.

                # It might turn out to to expensive to store closed values for all defined functions using '--',
                #   Might use '---' instead?

                # Mising piece right now is mapping from fn defn to fn application. 
                #   Hmm, what fn are we calling now, lookup its closed vars and pass in/save out.



                # TODO close over vars named after '--'
                #  Need to ba able to identify which function is being applied, its id based on the function's definition sequence number. {lambdaSeq}
                if(@closed) {
                    #_report(['saw fn %d, expr %s', $o->{lambdaSeq}, $expr]);
                    map {
                        # TODO Copy in value from a per-fn namespace
                        #_report(['saw fn %d, var %s', $o->{lambdaSeq}, $_]);
                        unshift @{$varMap->{$_}}, undef;
                    } @closed;
                }

                $afterL =~ s/\)$//;


				unshift @{$varMap->{'.'}}, "  $fn $ns";

#-------------------
                if(-f '/tmp/dpl-callstack-dump.doit') {
                    my $now = int time;
                    my $fh = new FileHandle("> /tmp/dpl-callstack-dump-$now-$$");
                    printf $fh "%s\n", join("\n", @{$varMap->{'.'}});
                    close $fh;
                    unlink '/tmp/dpl-callstack-dump.doit';
                }
#-------------------

                # Eval function body, capture perl die caused by early out (return)
                $value = eval { $o->evalAll($afterL, { }) };

				shift @{$varMap->{'.'}};


                # TODO save 'close' values over vars named after '--'
                map {
                    # TODO Copy out value to a per-fn namespace
                    shift @{$varMap->{$_}};
                } @closed;

                map { shift @{$varMap->{$_}} } @params;


                my $e = $@;

                if($e) {
                    # _report "Exception after function return: e: $e->{exception}" if defined $e->{exception};

                    if(ref $e eq 'HASH') {
                        # It's a meta exception (one of ours.)
                        # map { _report " -> $_ : $e->{$_}"}  sort keys %$e;

                        die $e if $ismacro;

                           if(exists $e->{value}) {
							$value = $e->{value};
						}
                        elsif(exists $e->{values}) {
							my $V = $o->{multi_value_return} = $e->{values};
							$value = pop @$V; push @$V, $value;	# get last value.
						}
						elsif(exists $e->{exception}) {
							$o->throw($e->{exception});
						}
                        else { $o->throw("Unexpected meta-exception (after function return): $e; " . join(',', map { "$_=$e->{$_}" } sort keys %$e)) }
                    }
                    else { $o->throw($e) }
                }
            }
            elsif(ref $fnRef eq 'HASH') {
                # Handle calls to dropped builtins.  Doesn't actually return.
                $o->throw($fnRef->{msg});
            }

            else { 
                # if(ref $fnRef eq 'ARRAY') { map { _report "fnRef: $_" } @$fnRef }
                $o->throw("Unexpected fnRef: '$fnRef'");
            }

            return wantarray? ($value, $after): $value;
        }

        $o->throw("Unclosed expression: <|$expr|>, <|$after|>");
    }

    #--------------------------------------------------------------------------------------------------
    # If it's a line comment, skip it and move on to next thing.
    elsif($expr =~ /^((#|\/\/|;)[^\n]*)\n?/s) {
        ($value, $after) = ($1, $');
        $pf->click(__LINE__) if defined $pf;
        return $o->eval($after, $vars);
    }

    #--------------------------------------------------------------------------------------------------
    # If it's a inline comment, skip it and move on to next thing.
    elsif($expr =~ m~^/\*(([^/]|/[^\*])*)\*/~s) {
        ($value, $after) = ($1, $');
        $pf->click(__LINE__) if defined $pf;
        return $o->eval($after, $vars);
    }

    #--------------------------------------------------------------------------------------------------
    # If it's a here document, Look for the end of marker, return string.
    elsif($expr =~ /^(<<([-.\w]+)(\r?\n)?(.*?)\n\2)/s) {
        ($match, $value, $after) = ($1, $4, $');
        $pf->click(__LINE__) if defined $pf;
         
        if(!$o->{evaling}) { return wantarray? ($match, $after): $match } 
 
        # Do some (simple) variable interpolation *after* unescaping backslash escaped chars.
        #           1      2           3
        $value =~ s/([^\\])(\$[-!+\w]+)(\s)?/$o->_interpolate_($vars, $1, $2, $3)/egs;

        $value =~ s/\\\$/\$/gs;

        $value = decode_utf8($value);

        return wantarray? ($value, $after): $value;
    }

    #--------------------------------------------------------------------------------------------------
    # If it's a regexp quote....
    elsif($expr =~ /^(\|\/(([^\/\\]|\\.)+)\/\|)/s) {
         ($match, $value, $after) = ($1, $2, $');
         # _report "regexp: |/$value/|";
 
         if(!$o->{evaling}) { return wantarray? ($match, $after): $match } 
  
         return wantarray? ($value, $after): $value;
    }

    #--------------------------------------------------------------------------------------------------
    # If it's an literal, it eval's to itself. return it as is.
    elsif($expr =~ /^([^\(',"\)\s\$\[\]{}]+)/) {
        ($match, $value, $after) = ($1, $1, $');
        $pf->click(__LINE__) if defined $pf;
        $value = decode_utf8($value);
        return wantarray? ($value, $after): $value;
    }

    #--------------------------------------------------------------------------------------------------
    # If it's a variable lookup, eval what's after the '$' and return its value using var-get.
    elsif($expr =~ /^(\$)/) {
        ($match, $after) = ($1, $');
        $pf->click(__LINE__) if defined $pf;

        my $name;

        # map { printf STDERR " -- '%s' --> '%s'\n", $_, $vars->{$_} } sort keys %$vars;
        ($name, $after) = $o->eval($after, $vars);
        # print STDERR " -- name: $name\n";

        if(!$o->{evaling}) { return wantarray? ("$match$name", $after): "$match$name" }

        # print STDERR " -- name: $name\n";
        # map { print STDERR " -- '%s' ---> '%s'\n", $_, $vars->{$_} } sort keys %$vars;

        my $value2 = $vars->{$name}  if   defined $vars;  # caller provided
        # printf STDERR " -- value2: %s\n", defined$ value2? $value2: '<undef>';

		# !! ################
		# !! #  If Core.dpli hasn't been loaded because the specified descriptor directory is bogus,
		# !! #  this call will fail (because var-get is defined in code.dpli) with an obscure error:
		# !! #  Use of uninitialized value in subroutine entry at /usr/local/lib/perl/PrimaLisp.pm line 2360, <DATA> line 1.
		# !! ################

          $value = defined $value2? $value2: $o->{builtinsMap}->{'var-get'}->($o, [$name]);
        # $value = defined $value2? $value2: $varMap->{$name}->[0];
        # print STDERR " -- value: $value\n";

        return wantarray? ($value, $after): $value;
    }

    #--------------------------------------------------------------------------------------------------
    # If it's a literal string, Look for the end of the string, return string.
    elsif($expr =~ /^('(([^'\\]|\\.)*)')/s) {
        ($match, $value, $after) = ($1, $2, $');
        $pf->click(__LINE__) if defined $pf;
        
        if(!$o->{evaling}) { return wantarray? ($match, $after): $match } 


        # Collapse backslash escaped chars.
        $value =~ s/\\0/\0/gs;
        $value =~ s/\\t/\t/gs;
        $value =~ s/\\r/\r/gs;
        $value =~ s/\\n/\n/gs;
        $value =~ s/\\'/'/gs;
        $value =~ s/\\\\/\\/gs;

		# printf STDERR " **** is_utf8: %s '%s'\n", utf8::is_utf8($value)? 'yes': 'no', utf8::is_utf8($value)? encode_utf8($value): $value;
        $value = decode_utf8($value);

        return wantarray? ($value, $after): $value;
    }

    #--------------------------------------------------------------------------------------------------
    # If it's an interpolated string, Look for the end of the string, return string.
    elsif($expr =~ /^("(([^"\\]|\\.)*)")/s) {
        ($match, $value, $after) = ($1, $2, $');
        $pf->click(__LINE__) if defined $pf;
        
        if(!$o->{evaling}) { return wantarray? ($match, $after): $match } 


        # Collapse backslash escaped chars.
        $value =~ s/\\0/\0/gs;
        $value =~ s/\\\$/\$/gs;
        $value =~ s/\\t/\t/gs;
        $value =~ s/\\r/\r/gs;
        $value =~ s/\\n/\n/gs;
        $value =~ s/\\"/"/gs;
        $value =~ s/\\\\/\\/gs;

        # Do some (simple) variable interpolation *after* unescaping backslash escaped chars.
        #           1       2             3
        $value =~ s/([^\\])?(\${?[-!+\w;?]+}?)(\s)?/$o->_interpolate_($vars, $1, $2, $3)/egs;

        $value = decode_utf8($value);

        return wantarray? ($value, $after): $value;
    }

    #--------------------------------------------------------------------------------------------------
    # If it's a leading comma, turn on eval mode.
    elsif($expr =~ /^,/) {
        ($match, $after) = ($1, $');
        $pf->click(__LINE__) if defined $pf;
        # _report " -- after: <|$after|>";

		# _report(['on commma: %s', defined $o->{oncomma}? $o->{oncomma}: '<undef>']);

		my $setting = $o->{oncomma};

		my $evalSetting;

		   if(!defined $setting)    { $evalSetting = 1 }
		elsif($setting eq 'ignore') { $evalSetting = 0 }
		else { $o->throw("Unknown oncomma setting '$setting'") }


        my $oEvaling = $o->{evaling};
        $o->{evaling} = $o->{lambda}? $oEvaling: $evalSetting;
        # map { printf STDERR " -- '%s' x> '%s'\n, $_, $vars->{$_})\n" } sort keys %$vars;
        ($value, $after) = eval { $o->eval($after, $vars) };
        $o->{evaling} = $oEvaling;

        $o->throw() if $@;

        if($o->{lambda}) { $value = ",$value" }

        return wantarray? ($value, $after): $value;
    }

    #--------------------------------------------------------------------------------------------------
    else { $o->throw("Unhandled expression: <|$expr|>") }

    #--------------------------------------------------------------------------------------------------
#----------------------------------------------

    $o->throw(" !! Shouldn't get here! expr: <|$expr|>; " . __FILE__ . ":" . __LINE__);

    return;
}


#X# doc_Method(rep, <|
# # $o->rep;
# # |>, <| Read, Eval, Print
#X# |>)
sub
rep {
    my ($o) = @_;
    # print STDERR " >> rep\n";

    my ($ifh, $ofh) = map { $o->{$_} } (qw(ifh ofh));

    my $rl = $o->{rl};


    if(-t $ifh) {
        $ofh->autoflush(1);
    }

    $o->{evalStartTime} = time;

    my $p;
    if(defined $o->{repSeq} && $o->{repSeq}) { $p = sprintf "[%d]%s ", $o->{repSeq}, $o->{prompt} }
    else                                     { $p = sprintf "%s ", $o->{prompt}                   }

    if(defined $rl) {
        $p = '' if ! -t $ifh;
        $expr = $rl->readline($p);
    }
    else {
        printf $ofh $p if -t $ifh;
        local $/ = undef;
        $expr = <$ifh>;
    }

    # Took this out and it made  (printf '%f\n' (Ã· 10 4))  find Ã· ...
    $expr = decode_utf8 $expr;

    unless(defined $expr) {
        print $ofh "\n" if $o->{prompt} && -t $ifh;
        return 1;
    }

    chomp $expr;

    ++$o->{repSeq} if $expr;

    # $expr =~ s/#.*$//  unless -t $ifh;

    $o->{evalStartTime} = time;
    # _report [ " -- evalStartTime: %s\n", $o->{evalStartTime} ];

    my $env = { };
    my ($value, $after);

    $value = $o->evalAll($expr, $env);

    $o->{evalEndTime} = time;
    # _report [  " -- evalEndTime: %s\n", $o->{evalEndTime} ];


    if(-t $ifh) {
        if(defined $value) {
            if(ref $value ne '') {
                # Serialize list/hash for display.
                  my $valueOut = eval { JSON->new->pretty->canonical->allow_blessed->convert_blessed->encode($value) };
                # my $valueOut = $o->eval('(desc-to-string $_value)', { _value => $value });

                # Unless it doesn't work out...
                unless($@) { $value = $valueOut }
                print $ofh encode_utf8($value);
            }
            else {
                printf $ofh "%s\n", encode_utf8($value);
            }
        }
    }

    _report "Left over expression: '$after'"
        if defined $after && $after ne '';

    return 0;
}


#X# doc_Method(repl, <|
# # $o->repl();
# # |>, <| Read, Eval, Print Loop
#X# |>)
sub
repl {
    my ($o) = @_;
    # print STDERR " >> repl()\n";

    my ($ifh, $ofh) = map { $o->{$_} } (qw(ifh ofh));

    $o->initReadLine if -t $ifh;

    my $done;

    do {
        delete $o->{evalEndTime};

        $o->{evalStartTime} = time;
        my $n_eval_start = $o->{n_evals};

        $done = eval { $o->rep };
        my $e = $@;
        if($e) {
            if(exists $e->{exception}) { _report([" !! Exception: %s", $e->{exception}]); $o->{evaling} = 1  }
            elsif(exists $e->{value} ) { _report " -- Value: $e->{value}"         }
            else                       { _report([" !! Exception: %s", $e]); $o->{evaling} = 1  }
        }
        my $n_eval_end = $o->{n_evals};

        $o->{evalEndTime} = time unless defined $o->{evalEndTime};

        if($o->{showTimes}) {
			my $n_evals_this = $n_eval_end - $n_eval_start;
			my $elapsed = $o->{evalEndTime} - $o->{evalStartTime};
			my $unit = 's';

			if($elapsed < 1) {
				$elapsed *= 1000; $unit = 'ms';
				if($elapsed < 1) {
					$elapsed *= 1000; $unit = 'us';
					if($elapsed < 1) {
						$elapsed *= 1000; $unit = 'ns'; }}}

            printf " ~~ elapsed: %.2f%s (%.2fu %.2fs %.2fcu %.2fcs)\n", $elapsed, $unit, times;
            printf " ~~ n_evals: %d, n_evals_this: %d\n", $n_eval_end, $n_evals_this;
            print "\n";
        }

    } while !$done;

    return;
}




#----------------------------------------------------------------------------------------
#  isagn/CLI/Driver.pm
#
#X# doc_Method(initReadLine, <|
# # $o->initReadLine();
# # |>, <|
#X# |>)
sub
initReadLine {
    my ($o, ) = @_;
    # print STDERR " >> initReadLine()\n";

      my $stub = 'Gnu';
    # my $stub = 'Perl';

    # Attempt to load $stub Readline...
    my $rlClass = 'Term::ReadLine';
    $ENV{PERL_RL} = $stub;

    # Generate some code to call Term::ReadLine->new().
    # Assemble args to new() from corresponding instance variables.
    my $rlCode = sprintf 'use %s; new %s(%s)',
                $rlClass, $rlClass,
                join(', ', map { "\$o->{$_}" } qw( prompt ifh ofh ));

    # ReadLine uses Term/Cap, which uses $HOME,
    # so we'd better make sure it's set to something...
    $ENV{HOME} = '/' unless defined $ENV{HOME};


    # print STDERR " %> $rlCode\n";
    my $rl = eval $rlCode;


    # Test for $stub readline; suggest installation if missing...
    $rlClass = "Term::ReadLine::$stub";
    eval "use $rlClass";
    if($@ && !$o->{ReadLineNoticeWasGiven}) {
        print STDERR " **        If $rlClass was installed, commandline\n";
        print STDERR " **        editing and history would be available...\n";
        sleep 1;
        $o->{ReadLineNoticeWasGiven} = 1;
    }


#X# doc_Instance(rl, <|
    $o->{rl} = $rl;
#X# |>, <|ReadLine module if available.|>)

    $rl->ornaments(0) if defined $rl;

    return;
}

my $initCode;

#X# doc_Method(initInterpreter, <|
# # $o->initInterpreter();
# # |>, <|
#X# |>)
sub
initInterpreter {
    my ($o, ) = @_;
    # print STDERR " >> initInterpreter()\n";

    $pf->click(__LINE__) if defined $pf;

	my $lastDir;

	if($o->{descDir} =~ /:/) {
        my $dirs = [ split /:/, $o->{descDir} ];
		$o->{descDir} = $dirs->[0];
		$lastDir = $dirs->[@$dirs-1];

		$o->{descDirList} = $dirs;
	}
	else {
		$o->{descDirList} = [ $o->{descDir} ];
		$lastDir = $o->{descDir};
	}

    $o->throw("Descriptor dir '$o->{descDir}' does not exist")  unless -d $o->{descDir};
    $o->throw("Descriptor dir '$o->{descDir}' is not writable") unless -w $lastDir;

    $o->{topDescDir} = exists $o->{outerInterp}? $o->{outerInterp}->{topDescDir}: $o->{descDir};
    $o->{topInterp}  = exists $o->{outerInterp}? $o->{outerInterp}->{topInterp}: $o;


    $_sig_o = $o unless defined $_sig_o;

	$logFH->autoflush(1);

    my $baseInitFile = $o->getFullPathFromPath('.dpl-init.pl');
    if(defined $baseInitFile && -f $baseInitFile) {
        my $fh = new FileHandle("< $baseInitFile");
        if(!defined $fh) {
            _report([' !! Can\'t open init file: %s', $!])
        }
        else {
            local $/ = undef;
            my $code = <$fh>;
            eval $code;
        }
    }

    if(!defined $initCode) {
        local $/ = undef;

        # Can only read from DATA once, so have to cache what's read.
        $initCode = eval { <DATA> };
        die " !! Exception1: $@\n" if $@;
    }

    $o->{lambdaSeq} = 0;

    return if exists $o->{noInit};


    # print STDERR "initCode: $initCode\n";
    my $r = eval { $o->evalAll($initCode, { }) if defined $initCode };
    # print STDERR "r: $r\n";

    $pf->click(__LINE__) if defined $pf;
    return $r if $r;

    if($@) {
		print STDERR " !! initInterpreter()\n";
        my $e = $@;
		my $cs = $e->{callstack};
		$cs = defined $cs? join("\n", @$cs): '-no-cs-';
		
        # map { print STDERR " -- Exception k: '$_' v: '$e->{$_}'\n" } keys %$e;
        die " !! $e\n"              if !ref $e;
        die " !! Exception: $e->{exception}, callstack: \n$cs\n" if exists $e->{exception};
        die " !! Value: $e->{value}\n"     if exists $e->{value};
    }

    # Set up some migration aliases.
    my $bi = $o->{builtinsMap};
    $bi->{'set-var'  } = $bi->{'var-set'};
    $bi->{'get-var'  } = $bi->{'var-get'};
    $bi->{'push-var' } = $bi->{'var-push'};
    $bi->{'pop-var'  } = $bi->{'var-pop'};
    $bi->{'clear-var'} = $bi->{'var-clear'};
    $bi->{'dump-vars'} = $bi->{'var-dump'};

    $bi->{'drop-builtins'} = $bi->{'bi-drop'};

    $bi->{'is-string'}  = $bi->{'string?'};
    $bi->{'is-sym'}     = $bi->{'sym?'};    
    $bi->{'is-hash'}    = $bi->{'hash?'};   
    $bi->{'is-list'}    = $bi->{'list?'};   
    $bi->{'is-undef'}   = $bi->{'undef?'};
    $bi->{'is-defined'} = $bi->{'defined?'};

    $bi->{'start-worker'}         = $bi->{'worker-start'};
    $bi->{'start-worker/ismacro'} = $bi->{'worker-start/ismacro'};
    $bi->{'stop-worker'}          = $bi->{'worker-stop'};

    # Convenience aliases.
    $bi->{'='} = $bi->{'var-set'};
    $bi->{'*'} = $bi->{'×'};
    $bi->{'/'} = $bi->{'÷'};

    # Try to ensure that files/dirs we make are group writable.
    umask 002;

    return;
}


#----------------------------------------------------------------------------------------

#X# doc_Method(setInputFH, <|
# # $o->setInputFH($fh);
# # |>, <|
#X# |>)
sub setInputFH  { my $o = shift; ($o->{ifh})     = @_ }

#X# doc_Method(setOutputFH, <|
# # $o->setOutputFH($fh);
# # |>, <|
#X# |>)
sub setOutputFH { my $o = shift; ($o->{ofh})     = @_ }

#X# doc_Method(setLogFH, <|
# # $o->setLogFH($fh);
# # |>, <|
#X# |>)
sub setLogFH    { my $o = shift; ($logFH)   = @_ }

#X# doc_Method(setDescDir, <|
# # $o->setDescDir($dir);
# # |>, <|
#X# |>)
sub setDescDir  { my $o = shift; ($o->{descDir}) = @_ ; $o->initInterpreter }

#X# doc_Method(setDebug, <|
# # $o->setDebug($val);
# # |>, <|
#X# |>)
sub setDebug    { my $o = shift; ($o->{debug})   = @_ }

#X# doc_Method(setPrompt, <|
# # $o->setPrompt($str);
# # |>, <|
#X# |>)
sub setPrompt   { my $o = shift; ($o->{prompt})  = @_ }

#X# doc_Method(setShowTimes, <|
# # $o->setShowTimes($str);
# # |>, <|
#X# |>)
sub setShowTimes   { my $o = shift; ($o->{showTimes})  = @_ }

#----------------------------------------------------------------------------------------


#X# doc_Method(new, <|
# # my $xxx = new doc_DocFullClass<||>($ifh, $ofh, [$noInit]);
# # |>, <|
# # Constructor for this class.
# # |>, <|
# # Longer discussion.
#X# |>)
sub
new {
    my ($o, $ifh, $ofh, $noInit) = @_;

    # BaseClass
    $o = bless {}, $o;
    # print STDERR " >> new($ifh, $ofh, $noInit)\n";

    $o->{prompt}    = '>';
    $o->{repSeq}    = 1;
    $o->{namespace} = '';

    my $cgiDir = getCGIDir;

	my $descDir =    './descriptor-dir'        if -d './descriptor-dir';
	$descDir = $ENV{DPL_DESC_DIR}              if ! defined $descDir && exists $ENV{DPL_DESC_DIR} && -d $ENV{DPL_DESC_DIR};
	$descDir = "$ENV{HOME}/descriptor-dir"     if ! defined $descDir && exists $ENV{HOME} && -d "$ENV{HOME}/descriptor-dir";
	$descDir = "$cgiDir/descriptor-dir"        if ! defined $descDir && -d "$cgiDir/descriptor-dir";

    $o->{descDir} = $descDir;


    $o->{n_evals} = 0;

    my $varMap = {
        # default namespace.
        '' => {
            pid => [ $$ ],
        },
    };


    $o->{varMap}         = $varMap;
    $o->{varMapNS}       = $varMap->{$o->{namespace}};
    $o->{varMapNS}->{':vars:'} = [ $o->{varMapNS} ];

    %{$o->{builtinsMap}} = %$Default_builtinsMap;


    # To keep track of (import)'d classes.
    # $o->{importInst} = { };

    # Maybe these should be accessible from varMap?
    $o->{descCache}   = { };
    $o->{iteratorMap} = { };

    if($nInner == 0 ) {
        $o->{varMapNS}->{':interpreter:'} = [ $o ];
    }

    $o->{lambda}    = 1;
    $o->{evaling}   = 1;
    $o->{debug}     = 0;
    $o->{showTimes} = 0;

    $o->{lockHoldTimes} = { };



    $ifh = new FileHandle("< -") unless defined $ifh;
    $ofh = new FileHandle("> -") unless defined $ofh;

    $o->{ifh}   = $ifh;
    $o->{ofh}   = $ofh;

    $ofh->autoflush(1);

    $o->{noInit} = $noInit if defined $noInit;

    $o->initInterpreter;

    return $o;
}

1;
__DATA__
(bi-import Core!)
(bi-import Core_sockets!)

// The basics.

(def-fn esrap (expr n -- pad first)
  '(esrap -expr-) : !! Convert expresion from list form to S expression form.'
  (if (! (list? $expr)) (return $expr))

  (= n (if (undef? $n) 0 $n))
  (= pad (x '. ' $n))

  (if (== 0 (list-len $expr)) (return (sprintf "%s()"      $pad)))
  (if (== 1 (list-len $expr)) (return (sprintf "%s(%s)"    $pad (first $expr))))
  (if (== 2 (list-len $expr)) (return (sprintf "%s(%s %s)" $pad (first $expr)(next $expr))))

  (sprintf "%s(%s)"
    $pad
    (join '\n' (map-list (fn (_ -- out)
        (= out (sprintf '%s%s' (if (defined? $first) $first '') $_))
        (= first $pad)
        $out)
      (map-list (fn (_) (esrap $_ (+ 1 $n))) $expr)))))


// Compatability aliases.
// Consistancy with hash-* desc-*.
(var-set define-hash-fn def-hash-fn {deprecated 1})
(var-set define-desc-fn def-desc-fn {deprecated 1})

// Usability functions
(def-fn Help (str)
  '(Help -str-) : Show readable usage messages from (help -str-).'
  '(Help) : Show all readable usage messages from (help).'
  (print "Usage messages for builtin functions; DPL-") (Version)
  (map-list (fn (_) (emit "$_\n")) (help $str)) ())

(def-fn Stats () 
  '(Stats) : Show readable stats from (statistics).'
  // should be able to do:
  // (namespace () {s (statistics)}  )
  // but lexical s isn't visible in called function.
  (var-push s (statistics))

  (map-list (fn (k)
      (printf '%s : %s\n' $k (hash-get $s / $k)))
    (sort-alpha (hash-keys $s /)))

  (var-pop s) ())

(def-fn Version ()
  '(Version) : Show readable version information.'
  (printf '%s/%s\n' (hash-get (version) / version) (hash-get (version) / date)))

// Tie in to http:/api/eval
(def-macro rem-eval (host &exprs) (report ' >> (rem-eval %s{%s} %s)' $host (eval $host) (join ' ' $exprs))
  '(rem-eval -host- -expr- ...) : Evaluate -expr- ... on interpreter at -host-.'

  (deref-url (sprintf http://%s/api/eval (eval $host))
    {noserialize 1 method POST body (join '\n' $exprs)}))


// should try with def-macro...
(var-set accumulate-time
 (fn (tvar &exprs -- ret err t0 t1 info)
  (= tvar (eval $tvar))
  (= info (hash-get $self / accumulate-time))
  // (report ' >> (%s) self/name %s  %s %s %s %s' $. $self $self/name $tvar $info (desc-to-string $[info]))

  (catch
    (do
      (= t0  (time))
      (= ret (eval-all $exprs))
      (= t1  (time)))
    (= err $_err))

  (hash-incr $info / $tvar (hash-set $info /.meta elapsed (- $t1 $t0)))

  // (report ' << (%s) self/name %s  %s %s %s %s %s' $. $self $self/name $tvar $info $err (desc-to-string $[info]))

  (if (&& (undef? $ret) (defined? $err))
    (/*re*/throw $err))

  $ret
 )
 {ismacro 1})

(def-fn accumulate-time-last (name) 
  (hash-get $self /accumulate-time/.meta $name))

(def-fn report-timings (n -- vars info max t )
  (= info (hash-clear $self / accumulate-time))
  // (report ' >> (%s) self/name %s  %s %s %s' $. $self $self/name $info (desc-to-string $[info]))
  (hash-set $self / accumulate-time {})

  (if (undef? $n) (= n 1))

  (= vars
    (sort-pred  (fn (a b) (<=> (hash-get $info / $b) (hash-get $info / $a)))
      (grep  (fn (_) (ne . (substr $_ 0 1)) )
        (hash-keys $info /))))

  (= max (hash-get $info / (ith $vars 0)))

  {
    stats          (statistics)
    report         (foreach v $vars
                     (sprintf '%9.6f %.5f  %s'
                       (/ (= t (hash-get $info / $v)) $n)
                       (/ $t $max)
                        $v)) 
  })

(def-fn hash-index (D desc-name idx -- val)
  (if (undef? $idx)       (= idx {}))
  (= desc-name (if (defined? $desc-name)
    "$desc-name " ''))

  (map-list (fn (path)
    (map-list (fn (key)
        (= val "$desc-name$path")
        (if (defined? (= postings (hash-get $idx / $key)))
          (push $postings $val)
          (hash-set $idx / $key [ $val ])))
      (sort-alpha (hash-keys $D $path))))
    (hash-paths $D))

  $idx
)

849931f043111b430106404f7d93631a
