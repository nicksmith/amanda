# Copyright (c) 2005-2008 Zmanda Inc.  All Rights Reserved.
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
# Contact information: Zmanda Inc, 465 S Mathlida Ave, Suite 300
# Sunnyvale, CA 94086, USA, or: http://www.zmanda.com

use Test::More; # test count is given below

use lib '@amperldir@';
use Installcheck::Run qw( run_expect );

if ($Installcheck::Run::have_expect) {
    plan tests => 18;
} else {
    plan skip_all => "Expect.pm not installed"
}

use Amanda::Paths;
use Amanda::Header;
use Amanda::Debug;

# ABOUT THESE TESTS:
#
# We run a sequence of fixed interactions with the taper, putting it
# through its paces.  Each message to or from the taper is represented
# as a test, for readability.  If the taper produces unexpected results,
# the script dies, on the assumption that subsequent tests will be
# meaningless.

# put the debug messages somewhere
Amanda::Debug::dbopen("installcheck");

my $testconf = Installcheck::Run::setup();
$testconf->add_param('label_new_tapes', '"TESTCONF%%"');

my ($ok, $exp, $hdr, @results);

# create a test file with a header and some fun data
my $test_filename = "$AMANDA_TMPDIR/installcheck-taper";
$hdr = Amanda::Header->new();
$hdr->{type} = $Amanda::Header::F_DUMPFILE;
$hdr->{datestamp} = "20070102030405";
$hdr->{dumplevel} = 0;
$hdr->{compressed} = 1;
$hdr->{name} = "localhost";
$hdr->{disk} = "/home";
$hdr->{program} = "INSTALLCHECK";
$hdr = $hdr->to_string(32768,32768);

open(my $fh, ">", $test_filename) or die("Could not open '$test_filename': $!");
# as a cheap and easy way to fill space, just write the header a bunch of times
for (1 .. 64) {
    syswrite($fh, $hdr) or die("Error writing: $!");
}
close($fh);

##
# fire up the taper and write a couple of files

$testconf->write();
$exp = run_expect("$amlibexecdir/taper", "TESTCONF");
$exp->log_stdout(0); # comment out to see taper communication

$exp->send("START-TAPER 20090102030405\n");
pass("sent START-TAPER");

$exp->expect(120,
    [ qr/^TAPER-OK/ => sub {
	pass("got TAPER-OK");
	$ok = 1;
    } ],
    [ qr/^TAPE-ERROR (.*)/ => sub {
	diag($exp->match);
	die;
    } ],
    ) or die($exp->error());
die unless $ok;

$exp->send("FILE-WRITE 77-12345 \"$test_filename\" localhost /home 0 20070102030405 0\n");
pass("sent FILE-WRITE");

@results = ();
$exp->expect(120,
    [ qr/^REQUEST-NEW-TAPE 77-12345/ => sub {
	push @results, "REQUEST-NEW-TAPE";
	pass("got REQUEST-NEW-TAPE");

	$exp->send("NEW-TAPE\n");
	pass("sent NEW-TAPE");

	exp_continue;
    } ],
    [ qr/^NEW-TAPE 77-12345 TESTCONF01/ => sub {
	push @results, "NEW-TAPE";
	pass("got NEW-TAPE");

	exp_continue;
    } ],
    [ qr/^PARTDONE 77-12345 TESTCONF01 1 2016 "\[sec [\d.]+ kb 2016 kps [\d.]+\]"/ => sub {
	push @results, "PARTDONE";
	pass("got PARTDONE");

	exp_continue;
    } ],
    [ qr/^DONE 77-12345 INPUT-GOOD TAPE-GOOD "\[sec [\d.]+ kb 2016 kps [\d.]+\]" "" ""/ => sub {
	push @results, "DONE";
	pass("got DONE");
    } ],

    # failure
    [ qr/^.* 77-12345 INPUT-ERROR.*/ => sub {
	diag($exp->match);
	die;
    } ],
    [ qr/^.* 77-12345 .* TAPE-ERROR.*/ => sub {
	diag($exp->match);
	die;
    } ],
    [ qr/^NO-NEW-TAPE (.*)/ => sub {
	diag($exp->match);
	die;
    } ],
    ) or die($exp->error());
is_deeply([ @results ],
	  [ "REQUEST-NEW-TAPE", "NEW-TAPE", "PARTDONE", "DONE" ],
	  "got correct sequence of taper responses");

$exp->send("FILE-WRITE 77-12345 \"$test_filename\" localhost /home2 0 20070102030405 525000\n");
pass("sent second FILE-WRITE (with splitting)");

@results = ();
$exp->expect(120,
    [ qr/^PARTDONE 77-12345 TESTCONF01 (\d+) \d+ "\[sec [\d.]+ kb \d+ kps [\d.]+\]"/ => sub {
	my $filenum = ($exp->matchlist())[0];
	push @results, "PARTDONE $filenum";
	pass("got PARTDONE $filenum");

	exp_continue;
    } ],
    [ qr/^DONE 77-12345 INPUT-GOOD TAPE-GOOD "\[sec [\d.]+ kb 2016 kps [\d.]+\]" "" ""/ => sub {
	push @results, "DONE";
	pass("got DONE");
    } ],

    # failure
    [ qr/^.* 77-12345 INPUT-ERROR.*/ => sub {
	diag($exp->match);
	die;
    } ],
    [ qr/^.* 77-12345 .* TAPE-ERROR.*/ => sub {
	diag($exp->match);
	die;
    } ],
    [ qr/^NO-NEW-TAPE (.*)/ => sub {
	diag($exp->match);
	die;
    } ],
    ) or die($exp->error());
is_deeply([ @results ],
	  [ "PARTDONE 2", "PARTDONE 3", "PARTDONE 4", "PARTDONE 5", "DONE" ],
	  "got correct sequence of taper responses");

$exp->send("QUIT\n");
pass("sent QUIT");

$ok = 0;
$exp->expect(120,
    [ qr/^QUITTING/ => sub {
	$ok = 1;
    } ],
    [ qr/^TAPE-ERROR (.*)/ => sub {
	diag($exp->match);
    } ],
    ) or die($exp->error());
ok($ok, "got QUITTING") or die;

# TODO:
#  - check handling of NO-NEW-TAPE
#  - check EOT (PARTIAL)

unlink($test_filename);
