# Copyright (c) 2005-2008 Zmanda, Inc.  All Rights Reserved.
#
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License version 2.1 as
# published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
# License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this library; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
#
# Contact information: Zmanda Inc., 465 S Mathlida Ave, Suite 300
# Sunnyvale, CA 94086, USA, or: http://www.zmanda.com

package Amanda::Taper::Scribe;

use strict;
use warnings;
use Carp;

use Amanda::Xfer qw( :constants );
use Amanda::Device qw( :constants );
use Amanda::Header;
use Amanda::Debug;

=head1 NAME

Amanda::Taper::Scribe

=head1 SYNOPSIS

  my $scribe = Amanda::Taper::Scribe->new(
	taperscan => $taperscan_algo,
        feedback => $feedback_obj);

  my $dump_cb = sub {
    my ($err) = @_;
    die $err if $err;

    print "DONE\n";
    Amanda::MainLoop::quit();
  };

  $scribe->start_xfer(
    dump_header => $my_header,
    xfer_elements => [ $xfer_source ],
    max_memory => 64 * 1024,
    split_method => 'disk',
    part_size => 150 * 1024**2,
    disk_cache_filename => "$tmpdir/splitbuffer",
    dump_cb => $dump_cb);

  Amanda::MainLoop::run();

=head1 OVERVIEW

This package provides a high-level abstraction of Amanda's procedure for
writing dumpfiles to tape.

Amanda writes a sequence of dumpfiles to a sequence of volumes.  The
volumes are supplied by a taperscan algorithm, which operates a changer
to find and load each volume.  As dumpfiles are written to volumes and
those volumes fill up, the taperscan algorithm supplies additional
volumes.

In order to reduce internal fragmentation within volumes, Amanda can
"split" dumpfiles into smaller pieces, so that the overall dumpfile can
span multiple volumes.  Each fixed-size "part" is written to the volume
in sequence.  If a device encounters an error while writing a part, then
that part is considered "partial", and is rewritten from its beginning
on the next volume.

To facilitate this rewriting, Amanda must have access to all of the data
in a part, even after that data is written to the volume.  The scribe
provides several methods to support this: caching the part in memory,
caching the part in a special on-disk file, or relying on pre-existing
on-disk storage.  The latter method is used when reading from holding
disks.

The details of efficiently splitting dumpfiles and rewriting parts are
handled by the low-level C<Amanda::Xfer::Dest::Taper>.  The Scribe
operates a transfer containing this element, supplying it with volumes
from an C<Amanda::Taper::Scan> object.  It calls a number of
C<Amanda::Taper::Scribe::Feedback> methods to indicate the status of the
dump process and to request permission for each additional volume.

=head1 OPERATING A SCRIBE

The C<Amanda::Taper::Scribe> constructor takes two arguments:
C<taperscan> and C<feedback>.  The first specifies the taper scan
algorithm that the Scribe should use, and the second specifies the
C<Feedback> object that will receive notifications from the Scribe (see
below).

  my $scribe = Amanda::Taper::Scribe->new(
        taperscan => $my_taperscan,
        feedback => $my_feedback);

Once the object is in place, call its C<start> method to invoke the
taperscan algorithm and scan for a tape, but will not overwrite the
tape.  This method takes a single parameter, the timestamp for this dump
run.  This timestamp will be written to each volume written by the
Scribe.

  $scribe->start(
        dump_timestamp => $ts);

Once the scribe is started, call C<start_xfer> to begin transfering a
dumpfile to volumes.  Supply the initial components of a transfer (see
L<Amanda::Xfer>) which will produce the data to be backed up, as well as
a header for the dumpfile and information on how to split the dump.

The C<dump_cb> is called when the Scribe is completely finished with the
transfer.  If the scribe encounters a fatal error, the callback's first
argument is an error object (either an C<Amanda::Changer::Error> or a
string); otherwise, it is undef.  Note that errors which result in a
FAILED or PARTIAL dump, but leave the scribe ready to start a new xfer,
are not reported to the C<dump_cb>.

In most cases, C<xfer_elements> is an arrayref containing a transfer
source, e.g., C<< [ Amanda::Xfer::Source::Fd->new($src_fd) ] >>.  In
principle, there is no reason that transfer filters could not be
included.

The underlying C<Amanda::Xfer::Dest::Taper> handles device streaming
properly.  It uses C<max_memory> bytes of memory for this purpose.

  $scribe->start_xfer(
        dump_cb => $dump_cb,
        xfer_elements => $xfer_elements,
        dump_header => $dump_header,
        max_memory => $max_memory,
        # .. split parameters
        );

The arguments to C<start_xfer> differ for the various split methods.
For no splitting:

  $scribe->start_xfer(
        # ...
        split_method => 'none');

For buffering the split parts in memory:

  $scribe->start_xfer(
        # ...
        split_method => 'memory',
        part_size => $part_size);

For buffering the split parts on disk:

  $scribe->start_xfer(
        # ...
        split_method => 'disk',
        part_size => $part_size,
        disk_cache_filename => $disk_cache_filename);

Finally, if the transfer source is capable of calling
C<Amanda::Xfer::Dest::Taper>'s C<cache_inform> method:

  $scribe->start_xfer(
        # ...
        split_method => 'cache_inform',
        part_size => $part_size);

Only one transfer can run at any given time, so do not call
C<start_xfer> until any previous C<dump_cb> has been called back.

When all of the dumpfiles are transferred, call the C<quit> method to
release any resources and clean up.  This method takes a typical
C<finished_cb>.

  $scribe->quit(finished_cb => sub {
    print "ALL DONE!\n";
  });

=head1 FEEDBACK

The C<Amanda::Taper::Scribe::Feedback> class is intended to be
subclassed by the user.  It provides a number of notification methods
that enable the historical logging and driver/taper interactions
required by Amanda.  The parent class does nothing of interest, but
allows subclasses to omit methods it does not need.

The C<request_volume_permission> method provides a means for the caller
to limit the number of volumes the Scribe consumes.  It is called as

  $fb->request_volume_permission(perm_cb => $cb);

where the C<perm_cb> is a callback which expects a single argument:
C<undef> if permission is granted, or reason (as a string) if permission
is denied.  The default implementation always calls C<< perm_cb->(undef)
>>.

All of the remaining methods are notifications, and do not take a
callback.

  $fb->notif_scan_finished(
        error => $error,
        reservation => $reservation,
        volume_label => $volume_label,
	access_mode => $access_mode);

The Scribe calls C<notif_scan_finished> every time a taperscan is finished,
with the same arguments that the taperscan's C<result_cb> produces, passed by
name.

  $fb->notif_new_tape(
        error => $error,
        volume_label => $volume_label);

The Scribe calls C<notif_new_tape> when a new volume is started.  If the
C<volume_label> is undefined, then the volume was not successfully
relabled, and its previous contents may still be available.  If C<error>
is defined, then no useful data was written to the volume.  Note that
C<error> and C<volume_label> may I<both> be defined if the previous
contents of the volume were erased, but no useful, new data was written
to the volume.

This method will be called exactly once for every call to
C<request_volume_permission> that calls C<< perm_cb->(undef) >>.

  $fb->notif_part_done(
        partnum => $partnum,
        fileno => $fileno,
        successful => $successful,
        size => $size,
        duration => $duration);

The Scribe calls C<notif_part_done> for each part written to the volume,
including partial parts.  If the part was not written successfully, then
C<successful> is false.  The C<size> is in bytes, and the C<duration> is
a floating-point number of seconds.  If a part fails before a new device
file is created, then C<fileno> may be zero.

  $fb->notif_dump_done(
        result => $result,
        input_errors => $input_errors,
        device_errors => $device_errors,
        size => $size,
        duration => $duration);

The Scribe calls this at the completion of each dump file, just before
calling the C<dump_cb>.  The C<result> is one of C<FAILED>, C<PARTIAL>,
or C<DONE>.  Even when C<dump_cb> will get a fatal error, C<result> may
be C<PARTIAL> if some data was written successfully.

Note that C<duration> does not include time spent operating the changer.
The input and device errors may be empty arrayrefs, if no such errors
occurred.

A typical Feedback subclass might begin like this:

  package main::Feedback;
  use base 'Amanda::Taper::Scribe::Feedback';

  sub request_volume_permission {
    my $self = shift;
    my %params = @_;

    Amanda::MainLoop::call_later($params{'perm_cb'},
                                 "NO VOLUMES FOR YOU!");
  }

=cut

# TODO: what kind of errors does the XDT send? are they fatal?

sub new {
    my $class = shift;
    my %params = @_;

    for my $rq_param qw(taperscan feedback) {
	croak "required parameter '$rq_param' mising"
	    unless exists $params{$rq_param};
    }

    my $self = {
	feedback => $params{'feedback'},
	dump_timestamp => undef,

	# device handling, and our current device and reservation
	devhandling => Amanda::Taper::Scribe::DevHandling->new(
	    taperscan => $params{'taperscan'},
	    feedback => $params{'feedback'},
	),
	reservation => undef,
	device => undef,

	# information for the current dumpfile
	dump_header => undef,
	dump_cb => undef,
	split_method => undef,
	xfer => undef,
	xdt => undef,
	size => 0,
	duration => 0.0,
	last_part_successful => 0,
	fatal_error => undef,
	input_errors => [],
    };

    return bless ($self, $class);
}

sub start {
    my $self = shift;
    my %params = @_;

    for my $rq_param qw(dump_timestamp) {
	croak "required parameter '$rq_param' mising"
	    unless exists $params{$rq_param};
    }

    $self->{'dump_timestamp'} = $params{'dump_timestamp'};
    $self->{'devhandling'}->start();
}

sub quit {
    my $self = shift;
    my %params = @_;

    for my $rq_param qw(finished_cb) {
	croak "required parameter '$rq_param' mising"
	    unless exists $params{$rq_param};
    }

    # since there's little other option than to barrel on through the
    # quitting procedure, quit() just accumulates its error messages
    # and, if necessary, concantenates them for the finished_cb.
    my @errors;

    if ($self->{'xfer'}) {
	die "Scribe cannot quit while a transfer is active";
        # Supporting this would be complicated:
        # - cancel the xfer and wait for it to complete
        # - ensure that the taperscan not be started afterward
        # and isn't required for normal Amanda operation.
    }

    my $cleanup_cb = sub {
	my ($error) = @_;
	push @errors, $error if $error;

        if (@errors == 1) {
            $error = $errors[0];
        } elsif (@errors > 1) {
            $error = join("; ", @errors);
        }

        $params{'finished_cb'}->($error);
    };

    if ($self->{'reservation'}) {
	if ($self->{'device'}) {
	    if (!$self->{'device'}->finish()) {
		push @errors, $self->{'device'}->error_or_status();
	    }
	}

	$self->{'reservation'}->release(finished_cb => $cleanup_cb);
    } else {
	Amanda::MainLoop::call_later($cleanup_cb, undef);
    }
}

# $dump_cb gets one argument: a fatal error

# Start a new transfer.
#
sub start_xfer {
    my $self = shift;
    my %params = @_;

    for my $rq_param qw(dump_cb xfer_elements dump_header max_memory split_method) {
	croak "required parameter '$rq_param' mising"
	    unless exists $params{$rq_param};
    }

    if ($params{'split_method'} ne 'none') {
        croak("required parameter 'part_size' missing")
            unless exists $params{'part_size'};
    }

    $self->{'split_method'} = $params{'split_method'};
    my ($part_size, $use_mem_cache, $disk_cache_filename) = (0, 0, undef);
    if ($params{'split_method'} eq 'none') {
        $part_size = 0;
    } elsif ($params{'split_method'} eq 'memory') {
        $part_size = $params{'part_size'};
        $use_mem_cache = 1;
    } elsif ($params{'split_method'} eq 'disk') {
        $part_size = $params{'part_size'};
        croak("required parameter 'disk_cache_filename' missing")
            unless exists $params{'disk_cache_filename'};
        $disk_cache_filename = $params{'disk_cache_filename'};
    } elsif ($params{'split_method'} eq 'cache_inform') {
        croak("split_method 'cache_inform' not currently supported"); # TODO
        $part_size = $params{'part_size'};
        $use_mem_cache = 0;
    } else {
        croak("invalid split_method $params{split_method}");
    }

    die "not yet started"
	unless ($self->{'dump_timestamp'});
    die "xfer already running"
	if ($self->{'xfer'});

    # fatal means *fatal*, caller!
    if ($self->{'fatal_error'}) {
	Amanda::MainLoop::call_later($params{'dump_cb'}, $self->{'fatal_error'});
	return;
    }

    my $xdt = Amanda::Xfer::Dest::Taper->new(
	$params{'max_memory'}, $part_size,
	$use_mem_cache, $disk_cache_filename);

    my $xfer_elements = $params{'xfer_elements'};
    my $xfer = Amanda::Xfer->new([ @$xfer_elements, $xdt ]);

    $xfer->get_source()->set_callback(sub {
	$self->_xfer_callback(@_);
    });

    # get the header ready for writing
    my $dump_header = $params{'dump_header'};
    $dump_header->{'partnum'} = 1;
    $dump_header->{'totalparts'} = -1;
    $dump_header->{'type'} = $Amanda::Header::F_DUMPFILE;

    $self->{'xfer'} = $xfer;
    $self->{'xdt'} = $xdt;
    $self->{'dump_header'} = $dump_header;
    $self->{'dump_cb'} = $params{'dump_cb'};
    $self->{'size'} = 0;
    $self->{'duration'} = 0.0;
    $self->{'last_part_successful'} = 1;
    $self->{'input_errors'} = [];

    $xfer->start();

    $self->_start_part();
}

sub _start_part {
    my $self = shift;

    # if the dump wasn't successful, and we're not splitting, then bail out.  It's
    # up to higher-level components to re-try this dump on a new volume, if desired.
    if (!$self->{'last_part_successful'} and $self->{'split_method'} eq 'none') {
	$self->_fatal_error("no space left on volume");
	return;
    }

    # invoke the devhandling object if we need a device
    if (!$self->{'device'}) {
	return $self->_get_new_volume();
    }

    # change the header so that the next part has the right type
    $self->{'dump_header'}->{'type'} = $Amanda::Header::F_SPLIT_DUMPFILE;

    # and start writing this part
    $self->{'xdt'}->start_part(!$self->{'last_part_successful'},
			       $self->{'device'},
			       $self->{'dump_header'});
}

sub _xfer_callback {
    my $self = shift;
    my ($src, $msg, $xfer) = @_;

    if ($msg->{'type'} == $XMSG_PART_DONE) {
	$self->_xmsg_part_done($src, $msg, $xfer);
    } elsif ($msg->{'type'} == $XMSG_INFO) {
	Amanda::Debug::info($msg->{'message'});
    } elsif ($msg->{'type'} == $XMSG_ERROR) {
	$self->_xmsg_error($src, $msg, $xfer);
    } elsif ($msg->{'type'} == $XMSG_DONE) {
	$self->_xmsg_done($src, $msg, $xfer);
    }
}

sub _xmsg_part_done {
    my $self = shift;
    my ($src, $msg, $xfer) = @_;

    # double-check partnum
    die "Part numbers do not match!"
	unless ($self->{'dump_header'}->{'partnum'} == $msg->{'partnum'});

    $self->{'feedback'}->notif_part_done(
	partnum => $msg->{'partnum'},
	fileno => $msg->{'fileno'},
	successful => $msg->{'successful'},
	size => $msg->{'size'},
	duration => $msg->{'duration'});

    $self->{'last_part_successful'} = $msg->{'successful'};

    if ($msg->{'successful'}) {
	$self->{'size'} += $msg->{'size'};
	$self->{'dutation'} += $msg->{'duration'};
    }

    if (!$msg->{'eof'}) {
	if ($msg->{'successful'}) {
	    # update the header for the next dumpfile
	    $self->{'dump_header'}->{'partnum'}++;

	    # change the header so that the next part has the right type
	    $self->{'dump_header'}->{'type'} = $Amanda::Header::F_SPLIT_DUMPFILE;

	    # and go on to the next part
	    $self->_start_part();
	} else {
	    # if there's an error finishing the device, it's probably just carryover
	    # from the error the Xfer::Dest::Taper encountered while writing to the
	    # device, so we ignore it.
	    if (!$self->{'device'}->finish()) {
		my $devname = $self->{'device'}->device_name;
		my $errmsg = $self->{'device'}->error_or_status();
		Amanda::Debug::debug("ignoring error while finishing device '$devname': $errmsg");
	    }

	    # get a new volume, then go on to the next part
	    $self->_get_new_volume();
	}
    }
}

sub _xmsg_error {
    my $self = shift;
    my ($src, $msg, $xfer) = @_;

    if ($msg->{'elt'}->isa("Amanda::Xfer::Dest::Taper")) {
	# XMSG_ERROR from the XDT is always fatal
	$self->_fatal_error($msg->{'message'});
    } else {
	push @{$self->{'input_errors'}}, $msg->{'message'};
    }
}

sub _xmsg_done {
    my $self = shift;
    my ($src, $msg, $xfer) = @_;

    if ($xfer->get_status() == $Amanda::Xfer::XFER_DONE) {
	$src->remove();

	# determine the correct final status
	my $result;
	if ($self->{'fatal_error'} or @{$self->{'input_errors'}}) {
	    $result = ($self->{'size'} > 0)? 'PARTIAL' : 'FAILED';
	} else {
	    $result = 'DONE';
	}

	# for expandability, we call notif_dump_done with a list of device
	# errors, but any device error that's not swallowed by the XDT is
	# fatal to the Scribe, so we only put $self->{'fatal_erro'} in this
	# list.
	my @device_errors;
	if ($self->{'fatal_error'}) {
	    push @device_errors, $self->{'fatal_error'};
	}

	$self->{'feedback'}->notif_dump_done(
	    result => $result,
	    input_errors => $self->{'input_errors'},
	    device_errors => [ @device_errors ],
	    size => $self->{'size'},
	    duration => $self->{'duration'});

	my $dump_cb = $self->{'dump_cb'};

	# reset everything and let the original caller know we're done
	$self->{'xfer'} = undef;
	$self->{'xdt'} = undef;
	$self->{'dump_header'} = undef;
	$self->{'dump_cb'} = undef;
	$self->{'size'} = 0;
	$self->{'duration'} = 0.0;
	$self->{'input_errors'} = [];

	Amanda::MainLoop::call_later($dump_cb, $self->{'fatal_error'});
    }
}

sub _fatal_error {
    my $self = shift;
    my ($error) = @_;

    $self->{'fatal_error'} = $error;

    # cancelling the xdt will eventually cause an XMSG_DONE, which will notice
    # the fatal_error and call notif_dump_done and dump_cb correctly.
    $self->{'xfer'}->cancel();
}

sub _get_new_volume {
    my $self = shift;

    # release first, if necessary
    if ($self->{'reservation'}) {
	my $res = $self->{'reservation'};

	$self->{'reservation'} = undef;
	$self->{'device'} = undef;

	$res->release(finished_cb => sub {
	    my ($error) = @_;

	    if ($error) {
		$self->_fatal_error($error);
	    } else {
		$self->_get_new_volume();
	    }
	});

	return;
    }

    $self->{'devhandling'}->get_volume(volume_cb => sub { $self->_volume_cb(@_); });
}

sub _volume_cb  {
    my $self = shift;
    my ($scan_error, $request_denied_reason, $reservation, $new_label, $access_mode) = @_;

    # note that we prefer the scan error over the request_denied_reason, in
    # the case that both are set

    if ($scan_error) {
	# if we had permission to use a tape, but didn't find a tape, we need to
	# notify of such
	if (!$request_denied_reason) {
	    $self->{'feedback'}->notif_new_tape(
		error => $scan_error,
		volume_label => undef);
	}

	$self->_fatal_error($scan_error);
	return;
    }

    if ($request_denied_reason) {
	$self->_fatal_error("Cannot use new volume: $request_denied_reason");
	return;
    }

    # from here on, if an error occurs, we must send notif_new_tape, and look
    # for a new volume
    $self->{'reservation'} = $reservation;
    my $device = $self->{'device'} = $reservation->{'device'};

    if (!$device->start($access_mode, $new_label, $self->{'dump_timestamp'})) {
	# try reading the label to see whether we erased the tape
	my $erased = !$device->read_label()
		# (does the device thing something is broken?)
		|| (($device->status & ~$DEVICE_STATUS_VOLUME_UNLABELED)
		    && !($device->status & $DEVICE_STATUS_VOLUME_UNLABELED))
		# (does the label and (more importantly) timestamp match)?
		|| ($device->volume_label eq $new_label
		    && $device->volume_time eq $self->{'dump_timestamp'});

	$self->{'feedback'}->notif_new_tape(
	    error => "while labeling new volume: " . $device->error_or_status(),
	    volume_label => $erased? $new_label : undef);

	return $self->_get_new_volume();
    }

    # success!
    $self->{'feedback'}->notif_new_tape(
	error => undef,
	volume_label => $new_label);
    $self->_start_part();
}

##
## Feedback
##

package Amanda::Taper::Scribe::Feedback;

# request permission to use a volume.
#
# $params{'perm_cb'} - callback taking one argument: an error message or 'undef'
sub request_volume_permission {
    my $self = shift;
    my %params = @_;

    # sure, you can have as many volumes as you want!
    Amanda::MainLoop::call_later($params{'perm_cb'}, undef);
}

sub notif_scan_finished { }
sub notif_new_tape { }
sub notif_part_done { }
sub notif_dump_done { }

##
## Device Handling
##

package Amanda::Taper::Scribe::DevHandling;

# This class handles scanning for volumes, requesting permission for those
# volumes (the driver likes to feel like it's in control), and providing those
# volumes to the scribe on request.  These can all happen independently, but
# the scribe cannot begin writing to a volume until all three have finished.
# That is: the scan is finished, the driver has given its permission, and the
# scribe has requested a volume.
#
# On start, the class starts scanning immediately, even though the scribe has
# not requested a volume.  Subsequently, a new scan does not begin until the
# scribe requests a volume.
#
# This class is "private" to Amanda::Taper::Scribe, so it is documented in
# comments, rather than POD.

# Create a new DevHandling object.
#
# request_cb takes no arguments, and should request permission to use a new
# tape, calling $devh->permission_response($err) with the response (undef for
# permission granted, or an error message on denial).  If request_cb is
# undefined, it is not used.
sub new {
    my $class = shift;
    my %params = @_;

    my $self = {
	taperscan => $params{'taperscan'},
	feedback => $params{'feedback'},

	# is a scan currently running, or completed?
	scan_running => 0,
	scan_finished => 0,
	scan_error => undef,

	# scan results
	reservation => undef,
	device => undef,
	volume_label => undef,

	# requests for permissiont to use a new volume
	request_pending => 0,
	request_complete => 0,
	request_denied_reason => undef,

	# the callback to provide the scribe with a volume
	volume_cb => undef,
    };

    return bless ($self, $class);
}

## public methods

# Called at scribe startup, this starts the instance off with a scan.
sub start {
    my $self = shift;

    $self->_start_scanning();
}

# Get an open, started device and label to start writing to available.  The
# volume_callback takes the following arguments:
#   $scan_error -- error message, or undef if no error occurred
#   $request_denied_reason -- reason volume request was denied, or undef
#   $reservation -- Amanda::Changer reservation
#   $device -- open, started device
# It is the responsibility of the caller to close the device and release the
# reservation when finished.  If $scan_error or $request_denied_reason are
# defined, then $reservation and $device will be undef.
sub get_volume {
    my $self = shift;
    my (%params) = @_;

    die "already processing a volume request"
	if ($self->{'volume_cb'});

    $self->{'volume_cb'} = $params{'volume_cb'};

    # kick off the relevant processes, if they're not already running
    $self->_start_scanning();
    $self->_start_request();

    $self->_maybe_volume_ready();
}

## private methods

sub _start_scanning {
    my $self = shift;

    return if $self->{'scan_running'} or $self->{'scan_finished'};

    $self->{'scan_running'} = 1;

    $self->{'taperscan'}->scan(result_cb => sub {
	my ($error, $reservation, $volume_label, $access_mode) = @_;

	$self->{'scan_running'} = 0;
	$self->{'scan_finished'} = 1;

	if ($error) {
	    $self->{'scan_error'} = $error;
	} else {
	    $self->{'reservation'} = $reservation;
	    $self->{'device'} = $reservation->{'device'};
	    $self->{'volume_label'} = $volume_label;
	    $self->{'access_mode'} = $access_mode;
	}

	$self->{'feedback'}->notif_scan_finished(
	    error => $error,
	    reservation => $reservation,
	    volume_label => $volume_label,
	    access_mode => $access_mode);

	$self->_maybe_volume_ready();
    });
}

sub _start_request {
    my $self = shift;

    return if $self->{'request_pending'} or $self->{'request_complete'};

    $self->{'request_pending'} = 1;

    $self->{'feedback'}->request_volume_permission(perm_cb => sub {
	my ($refusal_reason) = @_;

	$self->{'request_pending'} = 0;
	$self->{'request_complete'} = 1;
	$self->{'request_denied_reason'} = $refusal_reason;

	$self->_maybe_volume_ready();
    });
}

sub _maybe_volume_ready {
    my $self = shift;

    # we must have all three factors complete to return a volume
    return unless $self->{'volume_cb'};
    return unless $self->{'request_complete'};
    return unless $self->{'scan_finished'};

    # if we have any kind of error, release the reservation and come back
    # later
    if (($self->{'scan_error'} or $self->{'request_denied_reason'}) and $self->{'reservation'}) {
	$self->{'device'} = undef;

	$self->{'reservation'}->release(finished_cb => sub {
	    my ($error) = @_;

	    # so many errors, so little time..
	    if ($error) {
		if ($self->{'scan_error'}) {
		    warning("ignoring error releasing reservation ($error) after a scan error");
		} else {
		    $self->{'scan_error'} = $error;
		}
	    }

	    $self->{'reservation'} = undef;
	    $self->_maybe_volume_ready();
	});

	return;
    }

    # get the cb and its arguments lined up before passing them
    # to call_later
    my $volume_cb = $self->{'volume_cb'};
    my @volume_cb_args = (
	$self->{'scan_error'},
	$self->{'request_denied_reason'},
	$self->{'reservation'},
	$self->{'volume_label'},
	$self->{'access_mode'},
    );

    # reset everything and prepare for a new scan
    $self->{'scan_finished'} = 0;

    $self->{'reservation'} = undef;
    $self->{'device'} = undef;
    $self->{'volume_label'} = undef;

    $self->{'request_complete'} = 0;
    $self->{'volume_cb'} = undef;

    Amanda::MainLoop::call_later($volume_cb, @volume_cb_args);
}

1;
