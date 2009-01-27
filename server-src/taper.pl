#! @PERL@
# Copyright (c) 2008 Zmanda Inc.  All Rights Reserved.
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License version 2 as published
# by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
#
# Contact information: Zmanda Inc., 465 S Mathlida Ave, Suite 300
# Sunnyvale, CA 94086, USA, or: http://www.zmanda.com

use lib '@amperldir@';
use strict;
use warnings;

package Amanda::Taper::DriverIO;
# This package listens for driver commands and translates
# them into method calls on a taper object.

use Amanda::MainLoop qw( :constants );
use Amanda::Util qw( :quoting );
use Amanda::Debug qw( :logging );

sub new {
    my $class = shift;
    my ($fh, $controller) = @_;
    return bless {
	fh => $fh,
	controller => $controller,
	buf => '',
    }, $class;
}

sub start {
    my $self = shift;

    $self->{'src'} = Amanda::MainLoop::fd_source(*STDIN, $G_IO_IN|$G_IO_HUP);
    $self->{'src'}->set_callback(sub { $self->read_from_driver(@_); });
}

my %cmd_handlers;
sub read_from_driver {
    my $self = shift;
    my $data;
    if (sysread(STDIN, $data, 1024) == 0) {
	$self->{'buf'} = '';
	$data = "QUIT\n";
    }
    $self->{'buf'} .= $data;

    # now take any complete lines from the buffer and handle them
    while (my ($line, $rest) = ($self->{'buf'} =~ /([^\n]*)\n(.*)/)) {
	$self->{'buf'} = $rest;
	my ($cmd) = ($line =~ /^(\S+)/);
	next unless (defined $cmd);

	# TODO: cmd_handlers need not be functions, but can just be lists
	# of command-line argument names

	debug(">> $line");
	$cmd = uc $cmd;
	if (exists($cmd_handlers{$cmd})) {
	    $cmd_handlers{$cmd}->($self, $line);
	} else {
	    $self->send_bad_command(message => "Unknown command '$cmd'");
	}
    }
}

##
# handlers for incoming messages

$cmd_handlers{'START-TAPER'} = sub {
    my $self = shift;
    my ($line) = @_;
    my @args = split_quoted_strings($line);
    unless (@args == 2) {
	$self->send_bad_command(message => "Syntax: START-TAPER <timestamp>");
	return;
    }
    $self->{'controller'}->cmd_start_taper(
	timestamp => $args[1],
    );
};

$cmd_handlers{'NEW-TAPE'} = sub {
    my $self = shift;
    my ($line) = @_;
    my @args = split_quoted_strings($line);
    unless (@args == 1) {
	$self->send_bad_command(message => "Syntax: NEW-TAPE");
	return;
    }
    $self->{'controller'}->cmd_new_tape();
};

$cmd_handlers{'NO-NEW-TAPE'} = sub {
    my $self = shift;
    my ($line) = @_;
    my @args = split_quoted_strings($line);
    unless (@args == 2) {
	$self->send_bad_command(message => "Syntax: NO-NEW-TAPE <message>");
	return;
    }
    $self->{'controller'}->cmd_no_new_tape(
	message => $args[1],
    );
};

$cmd_handlers{'FAILED'} = sub {
    my $self = shift;
    my ($line) = @_;
    my @args = split_quoted_strings($line);
    unless (@args == 2) {
	$self->send_bad_command(message => "Syntax: FAILED <handle>");
	return;
    }
    $self->{'controller'}->cmd_failed(
	handle => $args[1],
    );
};

$cmd_handlers{'DONE'} = sub {
    my $self = shift;
    my ($line) = @_;
    my @args = split_quoted_strings($line);
    unless (@args == 2) {
	$self->send_bad_command(message => "Syntax: DONE <handle>");
	return;
    }
    $self->{'controller'}->cmd_done(
	handle => $args[1],
    );
};

$cmd_handlers{'PORT-WRITE'} = sub {
    my $self = shift;
    my ($line) = @_;
    my @args = split_quoted_strings($line);
    unless (@args == 9) {
	$self->send_bad_command(message => "Syntax: PORT-WRITE <handle> <hostname> <diskname> " .
				"<level> <datestamp> <splitsize> <split_diskbuffer> " .
				"<fallback_splitsize>");
	return;
    }
    $self->{'controller'}->cmd_port_write(
	handle => $args[1],
	hostname => $args[2],
	diskname => $args[3],
	level => $args[4],
	datestamp => $args[5],
	splitsize => $args[6],
	split_diskbuffer => $args[7],
	fallback_splitsize => $args[8],
    );
};

$cmd_handlers{'FILE-WRITE'} = sub {
    my $self = shift;
    my ($line) = @_;
    my @args = split_quoted_strings($line);
    unless (@args == 8) {
	$self->send_bad_command(message => "Syntax: FILE-WRITE <handle> <filename> <hostname> " .
				"<diskname> <level> <datestamp> <splitsize>");
	return;
    }
    $self->{'controller'}->cmd_file_write(
	handle => $args[1],
	filename => $args[2],
	hostname => $args[3],
	diskname => $args[4],
	level => $args[5],
	datestamp => $args[6],
	splitsize => $args[7],
    );
};

$cmd_handlers{'QUIT'} = sub {
    my $self = shift;
    my ($line) = @_;
    $self->{'controller'}->cmd_quit();
};

##
# messages back to the driver

# utility sub for the subs below
sub send_to_driver {
    my $self = shift;
    my ($line) = @_;

    debug("<< $line");
    syswrite(*STDOUT, "$line\n");
}

# TAPER_OK
# params: none
sub send_taper_ok {
    my $self = shift;
    my %params = @_;

    $self->send_to_driver("TAPER-OK");
}

# TAPE_ERROR
# params:
#   message: the error message
#   handle: (optional) handle
sub send_tape_error {
    my $self = shift;
    my %params = @_;

    my $qmsg = quote_string($params{'message'});

    # this command has two forms: one with a handle, one without
    if ($params{'handle'}) {
	my $handle = $params{'handle'};
	$self->send_to_driver("TAPE-ERROR $handle $qmsg");
    } else {
	$self->send_to_driver("TAPE-ERROR $qmsg");
    }
}

# PARTIAL, DONE, or FAILED
# params:
#   partial: true if this is a PARTIAL message
#   done: true if this is a DONE message
#   failed: true if this is a FAILED message
#   handle: transaction handle
#   input_error: input error message (PARTIAL and FAILED only)
#   tape_error: tape error message (PARTIAL and FAILED only)
#   sec: seconds used (PARTIAL and DONE only)
#   kb: kb written (PARTIAL and DONE only)
#   kps: write speed (default: calculated from sec and kb)
sub send_completion {
    my $self = shift;
    my %params = @_;

    my $msgtype;
    if ($params{'partial'}) {
	$msgtype = 'PARTIAL';
    } elsif ($params{'done'}) {
	$msgtype = 'DONE';
    } elsif ($params{'failed'}) {
	$msgtype = 'FAILED';
    } else {
	die("must specify 'partial', 'failed', or 'done'")
    }

    (my $handle = $params{'handle'})
	or die("must specify 'handle'");

    my $input_error = quote_string($params{'input_error'} or '');
    my $input_state = $params{'input_error'}?"INPUT-ERROR":"INPUT-GOOD";

    my $tape_error = quote_string($params{'tape_error'} or '');
    my $tape_state = $params{'tape_error'}?"TAPE-ERROR":"TAPE-GOOD";

    my $timeinfo = '';
    if ($msgtype ne 'FAILED') {
	(my $sec = $params{'sec'})
	    or die("must specify 'sec'");
	(my $kb = $params{'kb'})
	    or die("must specify 'kb'");
	my $kps = (exists $params{'kps'})? $params{'kps'} : ($kb/$sec);
	$timeinfo = "\"[sec $sec kb $kb kps $kps]\" ";
    }

    $self->send_to_driver("$msgtype $handle $input_state $tape_state $timeinfo" .
	  "$input_error $tape_error");
}

# NEW_TAPE
# params:
#   handle: transaction handle
#   label: new tape label
sub send_new_tape {
    my $self = shift;
    my %params = @_;

    my $handle = $params{'handle'};
    my $qlabel = quote_string($params{'label'});

    $self->send_to_driver("NEW_TAPE $handle $qlabel");
}

# NO_NEW_TAPE
# params:
#   handle: transaction handle
sub send_no_new_tape {
    my $self = shift;
    my %params = @_;

    my $handle = $params{'handle'};

    $self->send_to_driver("NO_NEW_TAPE $handle");
}

# PARTDONE
# params:
#   handle: transaction handle
#   label: new tape label
#   fileno: device file number
#   kb: size of this part
#   sec: time spent writing part
#   kps: speed at which part was written (default: calculated from kb and sec)
sub send_partdone {
    my $self = shift;
    my %params = @_;

    my $handle = $params{'handle'};
    my $qlabel = quote_string($params{'label'});
    my $fileno = $params{'fileno'};
    my $sec = $params{'sec'};
    my $kb = $params{'kb'};
    my $kps = (exists $params{'kps'})? $params{'kps'} : ($kb/$sec);
    my $ikb = int($kb);

    $self->send_to_driver("PARTDONE $handle $qlabel $fileno $ikb \"[sec $sec kb $kb kps $kps]\"");
}

# REQUEST_NEW_TAPE
# params:
#   handle: transaction handle
sub send_request_new_tape {
    my $self = shift;
    my %params = @_;

    my $handle = $params{'handle'};

    $self->send_to_driver("REQUEST-NEW-TAPE $handle");
}

# DUMPER_STATUS
# params:
#   handle: transaction handle
sub send_dumper_status {
    my $self = shift;
    my %params = @_;

    my $handle = $params{'handle'};

    $self->send_to_driver("DUMPER-STATUS $handle");
}

# PORT
# params:
#   port: port to connect to
sub send_port {
    my $self = shift;
    my %params = @_;

    my $port = $params{'port'};

    $self->send_to_driver("PORT $port");
}

# QUITTING
# params: none
sub send_quitting {
    my $self = shift;
    my %params = @_;

    $self->send_to_driver("QUITTING");
}

# BAD_COMMAND
# params:
#   message: error message
sub send_bad_command {
    my $self = shift;
    my %params = @_;

    my $qmsg = quote_string($params{'message'});

    $self->send_to_driver("BAD-COMMAND $qmsg");
}


package Amanda::Taper::Controller;

use Amanda::MainLoop;

# The controller mediates between messages from the driver and the ongoing
# action with the taper.  This is made a little bit complicated because the
# driver conversation is fairly contextual, with some responses answering
# "questions" asked earlier.  This is modeled with the following taper
# "states":

# init:
#   waiting for START-TAPER command
# starting:
#   warming up devices; TAPER-OK not sent yet
# idle:
#   not currently dumping anything
# writing:
#   in the middle of writing a file
# taperq:
#   waiting for permission to use a new tape, then back to
#   the writing state

sub new {
    my $class = shift;
    my $self = bless {
	io => undef, # set below
	state => "init",
	tape_space => 0,
	handle => undef,
	labelctr => 1,
	label => undef,
    }, $class;
    $self->{'io'} = Amanda::Taper::DriverIO->new(*STDIN, $self);
    return $self;
}

sub start {
    my $self = shift;
    $self->{'io'}->start();
}

sub assert_in_state {
    my $self = shift;
    my ($state) = @_;
    if ($self->{'state'} eq $state) {
	return 1;
    } else {
	$self->{'io'}->send_bad_command(message => "Command not valid in current taper state");
	return 0;
    }
}

sub do_write {
    my $self = shift;

    if ($self->{'tape_space'} == 0) {
	$self->{'io'}->send_request_new_tape(handle => $self->{'handle'});
	$self->{'state'} = 'taperq';
    } elsif ($self->{'tape_space'} == -1) {
	# we've already tried to get a tape, to no avail, so..
	$self->{'io'}->send_completion(
	    handle => $self->{'handle'},
	    failed => 1, # TODO: send partial if any parts were written
	    tape_error => "No new tape",
	);
	$self->{'state'} = "idle";
    } else {
	# pretend we wrote a part

	$self->{'io'}->send_partdone(
	    handle => $self->{'handle'},
	    label => $self->{'label'},
	    fileno => 11 - $self->{'tape_space'},
	    kb => 128,
	    sec => 1,
	);
	$self->{'tape_space'}--;

	# last part?
	if (--$self->{'nparts'} == 0) {
	    $self->{'io'}->send_completion(
		handle => $self->{'handle'},
		done => 1,
		kb => 128 * 3,
		sec => 3,
	    );
	    $self->{'state'} = 'idle';
	} else {
	    Amanda::MainLoop::call_later(\&do_write, $self);
	}
    }
}

##
# driver commands

sub cmd_start_taper {
    my $self = shift;
    my %params = @_;

    $self->assert_in_state("init") or return;

    $self->{'state'} = "starting";
    # TODO .. do some starting stuff..
    $self->{'io'}->send_taper_ok();
    $self->{'state'} = "idle";
}

sub cmd_new_tape {
    my $self = shift;
    my %params = @_;

    $self->assert_in_state("taperq") or return;
    $self->{'tape_space'} = 10;
    $self->{'label'} = 'TESTCONF0' . $self->{'labelctr'}++;
    $self->{'state'} = 'writing';
    $self->{'io'}->send_new_tape(
	handle => $self->{'handle'},
	label => $self->{'label'},
    );

    Amanda::MainLoop::call_later(\&do_write, $self);
}

sub cmd_no_new_tape {
    my $self = shift;
    my %params = @_;

    $self->assert_in_state("taperq") or return;
    $self->{'tape_space'} = -1;
    $self->{'state'} = 'writing';

    Amanda::MainLoop::call_later(\&do_write, $self);
}

sub cmd_failed {
    my $self = shift;
    my %params = @_;

    # TODO DUMPER-STATUS reply
}

sub cmd_done {
    my $self = shift;
    my %params = @_;

    # TODO DUMPER-STATUS reply
}

sub cmd_port_write {
    my $self = shift;
    my %params = @_;

    $self->assert_in_state("idle") or return;

    # TODO - open a port, send PORT, wait for connection .. need socket handling
    # support in MainLoop?
}

sub cmd_file_write {
    my $self = shift;
    my %params = @_;

    $self->assert_in_state("idle") or return;

    $self->{'state'} = 'writing';
    $self->{'nparts'} = 3; # XXX temp
    $self->{'handle'} = $params{'handle'};

    print STDERR "NOTE: writing $params{hostname}:$params{diskname}\n";

    Amanda::MainLoop::call_later(\&do_write, $self);
}

sub cmd_quit {
    my $self = shift;
    my %params = @_;

    # TODO: cleanup

    $self->{'io'}->send_quitting();
    Amanda::MainLoop::quit();

    # remove a reference loop
    $self->{'io'} = undef;
}


package main;

use Amanda::Util qw( :constants );
use Amanda::Config qw( :init );
use Getopt::Long;

# TODO: need a new context??
Amanda::Util::setup_application("taper", "server", $CONTEXT_DAEMON);

my $config_overwrites = new_config_overwrites($#ARGV+1);
Getopt::Long::Configure(qw{bundling});
GetOptions(
    'o=s' => sub { add_config_overwrite_opt($config_overwrites, $_[1]); },
) or usage();

if (@ARGV != 1) {
    die "USAGE: taper <config> <config-overwrites>";
}

config_init($CONFIG_INIT_EXPLICIT_NAME, $ARGV[0]);
my ($cfgerr_level, @cfgerr_errors) = config_errors();
if ($cfgerr_level >= $CFGERR_WARNINGS) {
    config_print_errors();
    if ($cfgerr_level >= $CFGERR_ERRORS) {
        die "Errors processing config file";
    }
}

Amanda::Util::finish_setup($RUNNING_AS_DUMPUSER);

my $controller = Amanda::Taper::Controller->new();
$controller->start();
Amanda::MainLoop::run();
