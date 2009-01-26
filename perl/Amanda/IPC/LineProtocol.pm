# Copyright (c) 2005-2008 Zmanda Inc.  All Rights Reserved.
#
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License version 2.1 as
# published by the Free Software Foundation.
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
# Contact information: Zmanda Inc, 465 S Mathlida Ave, Suite 300
# Sunnyvale, CA 94086, USA, or: http://www.zmanda.com

package Amanda::IPC::LineProtocol;
=head1 NAME

Amanda::IPC::LineProtocol -- parent class for line-based protocols

=head1 SYNOPSIS

Define your protocol:

    packge MyProtocol;
    use Amanda::IPC::LineProtocol;
    use constant MSG_SETSTATUS => message("SETSTATUS",
	match => qr/^FOO$/i,
	format => [ qw( param1 param2 optional? list* ) ],
    );
    use constant MSG_PING => message("PING",
	match => qr/^PING$/i,
	format => [ qw( id ) ],
    );
    use constant MSG_PONG => message("PONG",
	match => qr/^PONG$/i,
	format => [ qw( id ) ],
    );
    # ...

# And use the protocol
    packge main;
    my $input_fh = IO::Handle->new(...);
    my $output_fh = IO::Handle->new(...);
    my $proto;

    my $ping_cb = sub {
	my ($msg, %args) = @_;
	$proto->send(MyProtocol::MSG_PONG, id => $args{'id'});
    };

    my $message_cb = sub {
	my ($msg, %args) = @_;
	if (!$msg) {
	    die $args{'error'};
	}
    };

    $proto = MyProtocol->new(
	    rx_fh => $input_fh,
	    tx_fh => $output_fh,
	    message_cb => $message_cb,
	    MyProtocol::MSG_PING => $ping_cb);

    # or, to add callbacks afterward
    $proto->set_message_cb(MyProtocol::MSG_PONG,
	message_cb => $pong_cb);

# send a message
    $proto->send(MyProtocol::MSG_SETSTATUS,
	param1 => "x",
	param2 => "y",
	);

=head1 DESCRIPTION

This library is used to implement communications between Amanda processes.
Amanda has historically implemented a number of distinct text-based protocols
for communications between various components, and this library servces to
abstract and centralize the implementation of those protocols.

The package supports point-to-point, message-based, symmetric protocols.  Two
communicating processes exchange discrete messages, and in principle either
process can send a message at any time, although this is limited by the (often
unwritten) rules of the protocol.

In protocols based on this package, each message is a single text line,
terminated with a newline and consisting of a sequence of quoted strings.  The
first string determines the type of message.  For example:

  SEND-MORE-MONEY $150.00 "Books and pencils"
  ORDER-PIZZA Large Mushrooms Olives Onions "Green Peppers"

The package is asynchronous (see L<Amanda::MainLoop>), triggering callbacks for
incoming messages rather than implementing a C<get_message> method or the like.
If necessary, outgoing messages are queued for later transmission, thus
avoiding deadlocks from full pipe buffers.  This allows processing to continue
unhindered in both processes while messages are in transit in either direction.

=head2 DEFINING A PROTOCOL

There are two parts to any use of this package.  First, define the protocol by
creating a subclass and populating it using the C<message> package method.
This begins with something like

  package CollegeProtocol;
  use base "Amanda::IPC::LineProtocol";
  use Amanda::IPC::LineProtocol;

The usual trick for specifying messages is to simultaneously define a series of
constants, using the following idiom:

  use constant ORDER_PIZZA => message("ORDER-PIZZA",
    match => qr/^ORDER-PIZZA$/,
    format => [ qw( size toppings* ) ],
  );

The first argument to C<message> is the word with which this particular message
type will be sent.  The C<match> parameter gives a regular expression which
will be used to recognize incoming messages of this type.   If this parameter
is not specified, the default is to match the first argument with a
case-insensitive regexp.

The C<format> parameter describes the format of the arguments for this message
type.  A format parameter with the C<*> suffix gathers all remaining arguments
into a list.  A C<?> suffix indicates an optional parameter.  Note that it is
quite possible to specify ambiguous formats which will not work like you
expect.  The default format is an empty list (taking no arguments).

The optional C<on_eof> parameter will cause a a message of this type to be
generated on EOF.  For example, with:

  use constant DROP_OUT => message("DROP-OUT",
    on_eof => 1,
  );

when an EOF is detected, a C<DROP_OUT> message will be generated.

The protocol class should contain, in POD, a full description of the syntax of
the protcol -- which messages may be sent when, and what they mean.  No
facility is provided to encode this description in perl.

=head2 USING A PROTOCOL

Once a protocol is defined, it forms a class which can be used to run the
protocol.  Multiple instances of this class can be created to handle
simultaneous uses of the protocol over different channels.

The constructor, C<new>, takes two C<IO::Handle> objects -- one to read from
(C<rx_fh>) and one to write to (C<tx_fh>).  In some cases (e.g., a socket),
these may be the same handle.  It takes an optional callback, C<message_cb>,
which will be called for any received messages not handled by a more specific
callback.  Any other parameters are considered message-type-specific callbacks.

For example, given a socket handle C<$sockh>, the following will start the
C<CollegeProtocol> running on that socket:

  my $proto = CollegeProtocol->new(
    rx_fh => $sockh,
    tx_fh => $sockh,
    CollegeProtocol::PIZZA_DELIVERY => $pizza_delivery_cb,
  );

For protocols with a lot of message types, it may be easier to specify the
callbacks in separate statements:

  $proto->set_message_cb(CollegeProtocol::MIDTERM,
    sub { ... });

All message callbacks have the same signature:

  my $pizza_delivery_cb = sub {
    my ($msgtype, %params) = @_;
  }

where C<%params> contains all of the arguments to the message, keyed by the
argument names given in the message's C<format>.  Note that parameters
specified with the C<*> suffix will appear as arrayrefs.

In case of an error, the C<message_cb> (if specified) is called with
C<$msgtype> undefined and with a single parameter named C<error> giving the
error message.  This generally indicates either an unknown or badly-formatted
message.

To send a message, use the C<send> method, which takes the same arguments as a
message callback:

  $proto->send(CollegeProtocol::SEND_MORE_MONEY,
    how_much => "$150.00",
    what_for => "Books and pencils");

=cut

use Exporter ();
our @ISA = qw( Exporter );
our @EXPORT = qw( message new );

use IO::Handle;
use POSIX qw( :errno_h );
use strict;
use warnings;

use Amanda::Debug qw( debug );
use Amanda::MainLoop qw( :GIOCondition );
use Amanda::Util;

##
# Package methods to support protocol definition

my %msgspecs_by_protocol;
sub message {
    my ($name, @params) = @_;

    my $msgspec = ($msgspecs_by_protocol{caller()}->{$name} = { @params });

    # do some parameter sanity checks
    my $param;
    my @allowed_params = qw( match format on_eof );
    for $param (keys %$msgspec) {
	die "invalid message() parameter '$param'"
	    unless grep { $_ eq $param } @allowed_params;
    }

    # normalize the results a little bit
    $msgspec->{'name'} = $name;

    if (!exists $msgspec->{'match'}) {
	$msgspec->{'match'} = qr/$msgspec->{'name'}/i;
    }
    if (!exists $msgspec->{'format'}) {
	$msgspec->{'format'} = [];
    }

    return $name;
}

##
# class methods

sub new {
    my $class = shift;
    my %params = @_;

    my $self = bless {
	stopped => 0,

	rx_fh => $params{'rx_fh'},
	rx_buffer => '',
	rx_source => undef,

	tx_fh => $params{'tx_fh'},
	tx_buffer => '',
	tx_source => undef,
	tx_finished_cb => undef,

	cmd_cbs => {},
	default_cb => $params{'message_cb'},

	# a ref to the existing structure
	msgspecs => $msgspecs_by_protocol{$class},
    }, $class;

    # strip the known values from %params and use the rest as
    # command callbacks
    delete $params{'rx_fh'};
    delete $params{'tx_fh'};
    delete $params{'message_cb'};
    $self->{'cmd_cbs'} = \%params;

    # set nonblocking mode on both file descriptor
    if (!defined($self->{'rx_fh'}->blocking(0))) {
	die("Could not make protocol filehandle non-blocking");
    }
    if (!defined($self->{'tx_fh'}->blocking(0))) {
	die("Could not make protocol filehandle non-blocking");
    }

    # and autoflush on the write file descriptor
    $self->{'tx_fh'}->autoflush(1);

    # and set up a read callback
    $self->{'rx_source'} = Amanda::MainLoop::fd_source($self->{'rx_fh'}->fileno(),
	$G_IO_IN | $G_IO_ERR | $G_IO_HUP);
    $self->{'rx_source'}->set_callback(sub {
	$self->_incoming_data(@_);
    });

    return $self;
}

sub set_message_cb {
    my $self = shift;
    my ($name, $message_cb) = @_;

    $self->{'cmd_cbs'}->{$name} = $message_cb;
}

sub stop {
    my $self = shift;
    my %params = @_;

    $self->{'stopped'} = 1;

    if (defined $self->{'rx_source'}) {
	$self->{'rx_source'}->remove();
    }

    if (defined $self->{'tx_source'}) {
	$self->{'tx_finished_cb'} = $params{'finished_cb'};
    } else {
	Amanda::MainLoop::call_later($params{'finished_cb'});
    }
}

sub send {
    my $self = shift;
    my ($name, %info) = @_;

    my $msgspec = $self->{'msgspecs'}->{$name};
    die "No message spec for '$name'" unless defined($msgspec);

    my @line = $msgspec->{'name'};
    for my $elt (@{$msgspec->{'format'}}) {
	my ($name, $kind)= ($elt =~ /^(.*?)([*?]?)$/);
	my $val = $info{$name};

	if ($kind eq "*") {
	    die "message key $name must be an array"
		unless defined $val and ref($val) eq "ARRAY";
	    push @line, @$val;
	} else {
	    die "message key $name is required"
		unless defined $val or $kind eq "?";
	    push @line, $val if defined $val;
	}
    }

    my $line = join(" ", map { Amanda::Util::quote_string($_) } @line);
    $self->_write("$line\n");
}

##
# Handle outgoing messages

sub _write {
    my $self = shift;
    my ($data) = @_;

    if ($self->{'tx_buffer'}) {
	# if we already have outgoing data buffered, just add to it
	$self->{'tx_buffer'} .= $data;
    } else {
	# otherwise, set up the buffer and start writing from it
	$self->{'tx_buffer'} = $data;
	$self->_try_write();
    }
}

sub _try_write {
    my $self = shift;

    my $to_write = length($self->{'tx_buffer'});
    #print "need to write $to_write bytes\n";

    # io::handle::write doesn't retur the number of bytes written, so we can't use it here.
    my $written = POSIX::write($self->{'tx_fh'}->fileno(), $self->{'tx_buffer'}, $to_write);
    if (!defined $written and $! != EAGAIN) {
	die "IPC: while writing: $!";
    }

    # trim the buffer as necessary
    if (defined $written) {
	if ($written == $to_write) {
	    $self->{'tx_buffer'} = '';
	} elsif ($written) {
	    $self->{'tx_buffer'} = substr($self->{'tx_buffer'}, $written);
	}
    }

    # enable or disable the fd_source, as appropriate
    if ($self->{'tx_buffer'} and !$self->{'tx_source'}) {
	$self->{'tx_source'} = Amanda::MainLoop::fd_source($self->{'tx_fh'}->fileno(),
	    $G_IO_OUT);
	$self->{'tx_source'}->set_callback(sub {
	    $self->_try_write();
	});
    } elsif (!$self->{'tx_buffer'} and $self->{'tx_source'}) {
	$self->{'tx_source'}->remove();
	$self->{'tx_source'} = undef;

	# if we were waiting for this before finishing, call the callback
	if ($self->{'tx_finished_cb'}) {
	    Amanda::MainLoop::call_later($self->{'tx_finished_cb'});
	}
    }
}

##
# Handle incoming messages

sub _incoming_line {
    my $self = shift;
    my ($line) = @_;

    $line =~ s/\n//g;
    return unless $line;

    # turn the line into a list of strings..
    my @line = Amanda::Util::split_quoted_strings($line);
    return unless @line;

    # get the specification for this message
    my $msgspec = $self->_find_msgspec(shift @line);
    if (!defined $msgspec) {
	$self->_call_message_cb(undef, $line, {error => 'unknown command'});
	return;
    }

    my ($parserr, $args) = $self->_parse_line($msgspec, @line);
    if ($parserr) {
	$self->_call_message_cb(undef, $line, {error => $parserr});
	return;
    }

    $self->_call_message_cb($msgspec, $line, $args);
}

sub _incoming_eof {
    my $self = shift;

    # find the EOF msgspec and call it
    for my $msgspec (values %{$self->{'msgspecs'}}) {
	if ($msgspec->{'on_eof'}) {
	    $self->_call_message_cb($msgspec, "(EOF)", {});
	    last;
	}
    }
}

sub _find_msgspec {
    my $self = shift;
    my ($cmdstr) = @_;

    for my $msgspec (values %{$self->{'msgspecs'}}) {
	my $match = $msgspec->{'match'};
	return $msgspec if ($cmdstr =~ $match);
    }

    return undef;
}

sub _parse_line {
    my $self = shift;
    my ($msgspec, @line) = @_;

    # parse the message according to the format
    my $args = {};
    for my $elt (@{$msgspec->{'format'}}) {
	my ($name, $kind)= ($elt =~ /^(.*?)([*?]?)$/);

	if ($kind eq "*") {
	    $args->{$name} = [ @line ];
	    @line = ();
	    last;
	}

	next if ($kind eq "?" and !@line);

	if (!@line) {
	    return "too few arguments to '$msgspec->{name}': first missing argument is $name";
	}

	$args->{$name} = shift @line;
    }

    if (@line) {
	return "too many arguments to '$msgspec->{name}': first unmatched argument is '$line[0]'";
    }

    return (undef, $args);
}

sub _call_message_cb {
    my $self = shift;
    my ($msgspec, $line, $args) = @_;

    # after the user calls stop(), don't call any more callbacks
    return if $self->{'stopped'};

    # send a bogus line message to the default_cb if there's no msgspec
    if (!defined $msgspec) {
	if ($self->{'default_cb'}) {
	    $self->{'default_cb'}->(undef, %$args);
	} else {
	    debug("IPC: " . ($args->{'error'} or "bogus line '$line'"));
	}
	return;
    }

    # otherwise, call the relevant callback
    if (exists $self->{'cmd_cbs'}{$msgspec->{'name'}}) {
	$self->{'cmd_cbs'}->{$msgspec->{'name'}}->($msgspec->{'name'}, %$args);
    } else {
	if ($self->{'default_cb'}) {
	    $self->{'default_cb'}->($msgspec->{'name'}, %$args);
	} else {
	    debug("IPC: Ignored unhandled line '$line'");
	}
    }
}

sub _incoming_data {
    my $self = shift;
    my ($src) = @_;

    READ: while (1) {
	my $buf;
	my $nbytes = $self->{'rx_fh'}->read($buf, 32768);

	if ($nbytes) {
	    $self->{'rx_buffer'} .= $buf;
	    next READ;
	} elsif (!defined $nbytes or $nbytes == 0) {
	    # if this read would block, then just process what we have.
	    last READ if $! == EAGAIN;

	    # EOF -- flush the rx_buffer
	    if ($self->{'rx_buffer'} ne '') {
		$self->_incoming_line($self->{'rx_buffer'} . "\n");
	    }

	    # notify of EOF
	    $self->_incoming_eof();

	    # and stop the fd source
	    $self->{'rx_source'}->remove();
	    $self->{'rx_source'} = undef;
	}

	last READ;
    }

    while (my ($line, $rest) = ($self->{'rx_buffer'} =~ /([^\n]*)\n(.*)/)) {
	$self->{'rx_buffer'} = $rest;
	$self->_incoming_line($line);
    }
}

1;
