# $Id$
#
# This is all linked into the binary and evaluated when perl starts up...
#
#####################################################################
#####################################################################
# XXX NOTE: This file is sourced before almost any barnowl
# architecture is loaded. This means, for example, that it cannot
# execute any owl commands. Any code that needs to do so should live
# in BarnOwl::Hooks::_startup

use strict;
use warnings;

package BarnOwl;

=head1 NAME

BarnOwl

=head1 DESCRIPTION

The BarnOwl module contains the core of BarnOwl's perl
bindings. Source in this module is also run at startup to bootstrap
barnowl by defining things like the default style.

=for NOTE
These following functions are defined in perlglue.xs. Keep the
documentation here in sync with the user-visible commands defined
there!

=head2 command STRING

Executes a BarnOwl command in the same manner as if the user had
executed it at the BarnOwl command prompt. If the command returns a
value, return it as a string, otherwise return undef.

=head2 getcurmsg

Returns the current message as a C<BarnOwl::Message> subclass, or
undef if there is no message selected

=head2 getnumcols

Returns the width of the display window BarnOwl is currently using

=head2 getidletime

Returns the length of time since the user has pressed a key, in
seconds.

=head2 zephyr_getrealm

Returns the zephyr realm barnowl is running in

=head2 zephyr_getsender

Returns the fully-qualified name of the zephyr sender barnowl is
running as, e.g. C<nelhage@ATHENA.MIT.EDU>

=head2 zephyr_zwrite COMMAND MESSAGE

Sends a zephyr programmatically. C<COMMAND> should be a C<zwrite>
command line, and C<MESSAGE> is the zephyr body to send.

=head2 ztext_stylestrip STRING

Strips zephyr formatting from a string and returns the result

=head2 queue_message MESSAGE

Enqueue a message in the BarnOwl message list, logging it and
processing it appropriately. C<MESSAGE> should be an instance of
BarnOwl::Message or a subclass.

=head2 admin_message HEADER BODY

Display a BarnOwl B<Admin> message, with the given header and body.

=head2 start_question PROMPT CALLBACK

Displays C<PROMPT> on the screen and lets the user enter a line of
text, and calls C<CALLBACK>, which must be a perl subroutine
reference, with the text the user entered

=head2 start_password PROMPT CALLBACK

Like C<start_question>, but echoes the user's input as C<*>s when they
input.

=head2 start_edit_win PROMPT CALLBACK

Like C<start_question>, but displays C<PROMPT> on a line of its own
and opens the editwin. If the user cancels the edit win, C<CALLBACK>
is not invoked.

=head2 get_data_dir

Returns the BarnOwl system data directory, where system libraries and
modules are stored

=head2 get_config_dir

Returns the BarnOwl user configuration directory, where user modules
and configuration are stored (by default, C<$HOME/.owl>)

=head2 popless_text TEXT

Show a popup window containing the given C<TEXT>

=head2 popless_ztext TEXT

Show a popup window containing the provided zephyr-formatted C<TEXT>

=head2 error STRING

Reports an error and log it in `show errors'. Note that in any
callback or hook called in perl code from BarnOwl, a C<die> will be
caught and passed to C<error>.

=head2 debug STRING

Logs a debugging message to BarnOwl's debug log

=head2 getnumcolors

Returns the number of colors this BarnOwl is capable of displaying

=head2 add_dispatch FD CALLBACK

Adds a file descriptor to C<BarnOwl>'s internal C<select()>
loop. C<CALLBACK> will be invoked whenever data is available to be
read from C<FD>.

=head2 remove_dispatch FD

Remove a file descriptor previously registered via C<add_dispatch>

=cut


BEGIN {
# bootstrap in C bindings and glue
    *owl:: = \*BarnOwl::;
    bootstrap BarnOwl 1.2;
};

use lib(get_data_dir() . "/lib");
use lib(get_config_dir() . "/lib");

# perlconfig.c will set this to the value of the -c command-line
# switch, if present.
our $configfile;

our @__startup_errors = ();

if(!$configfile && -f $ENV{HOME} . "/.barnowlconf") {
    $configfile = $ENV{HOME} . "/.barnowlconf";
}
$configfile ||= $ENV{HOME}."/.owlconf";

# populate global variable space for legacy owlconf files
sub _format_msg_legacy_wrap {
    my ($m) = @_;
    $m->legacy_populate_global();
    return &BarnOwl::format_msg($m);
}

# populate global variable space for legacy owlconf files
sub _receive_msg_legacy_wrap {
    my ($m) = @_;
    $m->legacy_populate_global();
    return &BarnOwl::Hooks::_receive_msg($m);
}

=head2 AUTOLOAD

BarnOwl.pm has a C<AUTOLOAD> method that translates unused names in
the BarnOwl:: namespace to a call to BarnOwl::command() with that
command. Underscores are also translated to C<->s, so you can do
e.g. C<BarnOwl::start_command()> and it will be translated into
C<start-command>.

So, if you're looking for functionality that you can't find in the
perl interface, check C<:show commands> or C<commands.c> in the
BarnOwl source tree -- there's a good chance it exists as a BarnOwl
command.

=head3 BUGS

There are horrible quoting issues here. The AUTOLOAD simple joins your
commands with spaces and passes them unmodified to C<::command>

=cut

# make BarnOwl::<command>("foo") be aliases to BarnOwl::command("<command> foo");
sub AUTOLOAD {
    our $AUTOLOAD;
    my $called = $AUTOLOAD;
    $called =~ s/.*:://;
    $called =~ s/_/-/g;
    return &BarnOwl::command("$called ".join(" ",@_));
}

=head2 new_command NAME FUNC [{ARGS}]

Add a new owl command. When owl executes the command NAME, FUNC will
be called with the arguments passed to the command, with NAME as the
first argument.

ARGS should be a hashref containing any or all of C<summary>,
C<usage>, or C<description> keys:

=over 4

=item summary

A one-line summary of the purpose of the command

=item usage

A one-line usage synopsis, showing available options and syntax

=item description

A longer description of the syntax and semantics of the command,
explaining usage and options

=back

=cut

sub new_command {
    my $name = shift;
    my $func = shift;
    my $args = shift || {};
    my %args = (
        summary     => "",
        usage       => "",
        description => "",
        %{$args}
    );

    BarnOwl::new_command_internal($name, $func, $args{summary}, $args{usage}, $args{description});
}

=head2 new_variable_int NAME [{ARGS}]

=head2 new_variable_bool NAME [{ARGS}]

=head2 new_variable_string NAME [{ARGS}]

Add a new owl variable, either an int, a bool, or a string, with the
specified name.

ARGS can optionally contain the following keys:

=over 4

=item default

The default and initial value for the variable

=item summary

A one-line summary of the variable's purpose

=item description

A longer description of the function of the variable

=back

=cut

sub new_variable_int {
    unshift @_, \&BarnOwl::new_variable_int_internal, 0;
    goto \&_new_variable;
}

sub new_variable_bool {
    unshift @_, \&BarnOwl::new_variable_bool_internal, 0;
    goto \&_new_variable;
}

sub new_variable_string {
    unshift @_, \&BarnOwl::new_variable_string_internal, "";
    goto \&_new_variable;
}

sub _new_variable {
    my $func = shift;
    my $default_default = shift;
    my $name = shift;
    my $args = shift || {};
    my %args = (
        summary     => "",
        description => "",
        default     => $default_default,
        %{$args});
    $func->($name, $args{default}, $args{summary}, $args{description});
}

#####################################################################
#####################################################################

package BarnOwl::MessageList;

sub binsearch {
    my $list = shift;
    my $val  = shift;
    my $key  = shift || sub {return $_[0]};
    my $left = 0;
    my $right = scalar @{$list} - 1;
    my $mid = $left;
    while($left <= $right) {
        $mid = ($left + $right)/2;
        if($key->($list->[$mid]) < $val) {
            $left = $mid + 1;
        } else {
            $right = $mid - 1;
        }
    }
    return $mid;
}

my $__next_id = 0;

sub next_id {
    return $__next_id++;
}

sub new {
    my $ml;
    eval q{
        use BarnOwl::MessageList::SQL;
        $ml = BarnOwl::MessageList::SQL->new;
    };
    
    if($@) {
        push @BarnOwl::__startup_errors, "Unable to load SQL message list\n$@";
    } else {
        return $ml;
    }
    my $class = shift;
    my $self = {messages => {}};
    return bless $self, $class;
}

sub set_attribute {
    
}

sub get_size {
    my $self = shift;
    return scalar keys %{$self->{messages}};
}

sub iterate_begin {
    my $self = shift;
    my $id   = shift;
    my $rev  = shift;
    $self->{keys} = [sort {$a <=> $b} keys %{$self->{messages}}];
    if($id < 0) {
        $self->{iterator} = scalar @{$self->{keys}} - 1;
    } else {
        $self->{iterator} = binsearch($self->{keys}, $id);
    }
    
    $self->{iterate_direction} = $rev ? -1 : 1;
}

sub iterate_next {
    my $self = shift;
    if($self->{iterator} >= scalar @{$self->{keys}}) {
        return undef;
    }
    return $self->get_by_id($self->{keys}->[$self->{iterator}++]);
}

sub iterate_done {
    # nop
}

sub get_by_id {
    my $self = shift;
    my $id = shift;
    return $self->{messages}{$id};
}

sub add_message {
    my $self = shift;
    my $m = shift;
    $self->{messages}->{$m->id} = $m;
}

sub expunge {
    my $self = shift;
    for my $message (values %{$self->{messages}}) {
        if($message->is_deleted) {
            delete $self->{messages}->{$message->id};
        }
    }
}

sub close {
    
}

#####################################################################
#####################################################################

package BarnOwl::View;

sub get_name   {return shift->{name}};
sub messages   {return shift->{messages}};
sub get_filter {return shift->{filter}};

sub new {
    my $class = shift;
    my $name = shift;
    my $filter = shift;
    my $self  = {messages  => [],
                 name      => $name,
                 filter    => $filter};
    bless $self, $class;
    $self->recalculate;
    return $self;
}

sub consider_message {
    my $self = shift;
    my $msg  = shift;
    if(BarnOwl::filter_message_match($self->get_filter, $msg)) {
        push @{$self->messages}, $msg;
    }
}

sub recalculate {
    my $self = shift;
    my $ml   = BarnOwl::message_list();
    $ml->iterate_begin(0, 0);
    $self->{messages} = [];
    while(my $msg = $ml->iterate_next) {
        $self->consider_message($msg);
    }
}

sub new_filter {
    my $self = shift;
    my $filter = shift;
    $self->{filter} = $filter;
    $self->recalculate;
}

sub is_empty {
    my $self = shift;
    return $self->_size == 0;
}

sub _size {
    my $self = shift;
    return scalar @{$self->messages};
}

#####################################################################
#####################################################################

package BarnOwl::View::Iterator;

sub view {return shift->{view}}
sub index {return shift->{index}}

sub new {
    my $class = shift;
    my $self = {
        view  => undef,
        index => undef
       };
    return bless $self, $class;
}

sub invalidate {
    my $self = shift;
    $self->{view} = undef;
    $self->{index} = undef;
}

sub initialize_at_start {
    my $self = shift;
    my $view = shift;
    $self->{view}  = $view;
    $self->{index} = 0;
}

sub initialize_at_end {
    my $self = shift;
    my $view = shift;
    $self->{view}  = $view;
    $self->{index} = $self->view->_size - 1;
}

sub initialize_at_id {
    my $self = shift;
    my $view = shift;
    my $id   = shift;
    $self->{view} = $view;
    my $list = $self->view->messages;
    $self->{index} = BarnOwl::MessageList::binsearch($list, $id, sub{shift->id});
}

sub clone {
    my $self = shift;
    my $other = shift;
    $self->{view} = $other->{view};
    $self->{index} = $other->{index};
}

sub has_prev {
    my $self = shift;
    return $self->index > 0;
}

sub has_next {
    my $self = shift;
    return $self->index < $self->view->_size - 1;
}

sub at_start {
    my $self = shift;
    return $self->index < 0;
}

sub at_end {
    my $self = shift;
    return $self->index >= $self->view->_size;
}


sub valid {
    my $self = shift;
    return defined($self->view) &&
            !$self->at_start &&
            !$self->at_end;
}

sub prev {
    my $self = shift;
    $self->{index}-- unless $self->at_start
}

sub next {
    my $self = shift;
    $self->{index}++ unless $self->at_end
}

sub get_message {
    my $self = shift;
    return $self->view->messages->[$self->index];
}

sub cmp {
    my $self = shift;
    my $other = shift;
    return $self->index - $other->index;
}

#####################################################################
#####################################################################

package BarnOwl::Message;
use POSIX qw(ctime);

sub new {
    my $class = shift;
    my $time = time;
    my $timestr = ctime($time);
    $timestr =~ s/\n$//;
    my %args = (
        deleted   => 0,
        time      => $timestr,
        _time     => $time,
        login     => 'none',
        direction => 'none',
        @_);
    unless(exists($args{id})) {
        my $msglist = BarnOwl::message_list();
        $args{id} = $msglist->next_id;
    }
    if(exists $args{loginout}) {
        $args{login} = $args{loginout};
        delete $args{loginout};
    }
    if($class eq __PACKAGE__ && $args{type}) {
        $class = "BarnOwl::Message::" . ucfirst $args{type};
    }
    return bless {%args}, $class;
}

sub __set_attribute {
    my $self = shift;
    my $attr = shift;
    my $val  = shift;
    $self->{$attr} = $val;
}

sub __format_attributes {
    my $self = shift;
    my %skip = map {$_ => 1} qw(_time fields id deleted __fmtext type);
    my $text = "";
    my @keys = sort keys %$self;
    for my $k (@keys) {
        my $v = $self->{$k};
        unless($skip{$k}) {
            $text .= sprintf("  %-15.15s: %-35.35s\n", $k, $v);
        }
    }
    return $text;
}

sub type        { return shift->{"type"}; }
sub direction   { return shift->{"direction"}; }
sub time        { return shift->{"time"}; }
sub id          { return shift->{"id"}; }
sub body        { return shift->{"body"}; }
sub sender      { return shift->{"sender"}; }
sub recipient   { return shift->{"recipient"}; }
sub login       { return shift->{"login"}; }
sub is_private  { return shift->{"private"}; }

sub is_login    { return shift->login eq "login"; }
sub is_logout   { return shift->login eq "logout"; }
sub is_loginout { my $m=shift; return ($m->is_login or $m->is_logout); }
sub is_incoming { return (shift->{"direction"} eq "in"); }
sub is_outgoing { return (shift->{"direction"} eq "out"); }

sub is_deleted  { return shift->{"deleted"}; }

sub is_admin    { return (shift->{"type"} eq "admin"); }
sub is_generic  { return (shift->{"type"} eq "generic"); }
sub is_zephyr   { return (shift->{"type"} eq "zephyr"); }
sub is_aim      { return (shift->{"type"} eq "AIM"); }
sub is_jabber   { return (shift->{"type"} eq "jabber"); }
sub is_icq      { return (shift->{"type"} eq "icq"); }
sub is_yahoo    { return (shift->{"type"} eq "yahoo"); }
sub is_msn      { return (shift->{"type"} eq "msn"); }
sub is_loopback { return (shift->{"type"} eq "loopback"); }

# These are overridden by appropriate message types
sub is_ping     { return 0; }
sub is_mail     { return 0; }
sub is_personal { return shift->is_private; }
sub class       { return undef; }
sub instance    { return undef; }
sub realm       { return undef; }
sub opcode      { return undef; }
sub header      { return undef; }
sub host        { return undef; }
sub hostname    { return undef; }
sub auth        { return undef; }
sub fields      { return undef; }
sub zsig        { return undef; }
sub zwriteline  { return undef; }
sub login_host  { return undef; }
sub login_tty   { return undef; }

sub pretty_sender    { return shift->sender; }
sub pretty_recipient { return shift->recipient; }

sub delete {
    my $self = shift;
    $self->{deleted} = 1;
    BarnOwl::message_list()->set_attribute($self => deleted => 1);
}

sub undelete {
    my $self = shift;
    $self->{deleted} = 0;
    BarnOwl::message_list()->set_attribute($self => deleted => 0);
}

# Serializes the message into something similar to the zwgc->vt format
sub serialize {
    my ($this) = @_;
    my $s;
    for my $f (keys %$this) {
	my $val = $this->{$f};
	if (ref($val) eq "ARRAY") {
	    for my $i (0..@$val-1) {
		my $aval;
		$aval = $val->[$i];
		$aval =~ s/\n/\n$f.$i: /g;
		$s .= "$f.$i: $aval\n";
	    }
	} else {
	    $val =~ s/\n/\n$f: /g;
	    $s .= "$f: $val\n";
	}
    }
    return $s;
}

# Populate the annoying legacy global variables
sub legacy_populate_global {
    my ($m) = @_;
    $BarnOwl::direction  = $m->direction ;
    $BarnOwl::type       = $m->type      ;
    $BarnOwl::id         = $m->id        ;
    $BarnOwl::class      = $m->class     ;
    $BarnOwl::instance   = $m->instance  ;
    $BarnOwl::recipient  = $m->recipient ;
    $BarnOwl::sender     = $m->sender    ;
    $BarnOwl::realm      = $m->realm     ;
    $BarnOwl::opcode     = $m->opcode    ;
    $BarnOwl::zsig       = $m->zsig      ;
    $BarnOwl::msg        = $m->body      ;
    $BarnOwl::time       = $m->time      ;
    $BarnOwl::host       = $m->host      ;
    $BarnOwl::login      = $m->login     ;
    $BarnOwl::auth       = $m->auth      ;
    if ($m->fields) {
	@BarnOwl::fields = @{$m->fields};
	@main::fields = @{$m->fields};
    } else {
	@BarnOwl::fields = undef;
	@main::fields = undef;
    }
}

sub smartfilter {
    die("smartfilter not supported for this message\n");
}

# Display fields -- overridden by subclasses when needed
sub login_type {""}
sub login_extra {""}
sub long_sender {""}

# The context in which a non-personal message was sent, e.g. a chat or
# class
sub context {""}

# Some indicator of context *within* $self->context. e.g. the zephyr
# instance
sub subcontext {""}

#####################################################################
#####################################################################

package BarnOwl::Message::Admin;

use base qw( BarnOwl::Message );

sub header       { return shift->{"header"}; }

#####################################################################
#####################################################################

package BarnOwl::Message::Generic;

use base qw( BarnOwl::Message );

#####################################################################
#####################################################################

package BarnOwl::Message::Loopback;

use base qw( BarnOwl::Message );

# all loopback messages are private
sub is_private {
  return 1;
}

#####################################################################
#####################################################################

package BarnOwl::Message::AIM;

use base qw( BarnOwl::Message );

# all non-loginout AIM messages are private for now...
sub is_private {
    return !(shift->is_loginout);
}

#####################################################################
#####################################################################

package BarnOwl::Message::Zephyr;

use base qw( BarnOwl::Message );

sub login_type {
    return (shift->zsig eq "") ? "(PSEUDO)" : "";
}

sub login_extra {
    my $m = shift;
    return undef if (!$m->is_loginout);
    my $s = lc($m->host);
    $s .= " " . $m->login_tty if defined $m->login_tty;
    return $s;
}

sub long_sender {
    my $m = shift;
    return $m->zsig;
}

sub context {
    return shift->class;
}

sub subcontext {
    return shift->instance;
}

sub login_tty {
    my ($m) = @_;
    return undef if (!$m->is_loginout);
    return $m->fields->[2];
}

sub login_host {
    my ($m) = @_;
    return undef if (!$m->is_loginout);
    return $m->fields->[0];
}

sub zwriteline  { return shift->{"zwriteline"}; }

sub is_ping     { return (lc(shift->opcode) eq "ping"); }

sub is_personal {
    my ($m) = @_;
    return ((lc($m->class) eq "message")
	    && (lc($m->instance) eq "personal")
	    && $m->is_private);
}

sub is_mail {
    my ($m) = @_;
    return ((lc($m->class) eq "mail") && $m->is_private);
}

sub pretty_sender {
    my ($m) = @_;
    my $sender = $m->sender;
    my $realm = BarnOwl::zephyr_getrealm();
    $sender =~ s/\@$realm$//;
    return $sender;
}

sub pretty_recipient {
    my ($m) = @_;
    my $recip = $m->recipient;
    my $realm = BarnOwl::zephyr_getrealm();
    $recip =~ s/\@$realm$//;
    return $recip;
}

# These are arguably zephyr-specific
sub class       { return shift->{"class"}; }
sub instance    { return shift->{"instance"}; }
sub realm       { return shift->{"realm"}; }
sub opcode      { return shift->{"opcode"}; }
sub host        { return shift->{"hostname"}; }
sub hostname    { return shift->{"hostname"}; }
sub header      { return shift->{"header"}; }
sub auth        { return shift->{"auth"}; }
sub fields      { return shift->{"fields"}; }
sub zsig        { return shift->{"zsig"}; }

#####################################################################
#####################################################################
################################################################################

package BarnOwl::Hook;

=head1 BarnOwl::Hook

=head1 DESCRIPTION

A C<BarnOwl::Hook> represents a list of functions to be triggered on
some event. C<BarnOwl> exports a default set of these (see
C<BarnOwl::Hooks>), but can also be created and used by module code.

=head2 new

Creates a new Hook object

=cut

sub new {
    my $class = shift;
    return bless [], $class;
}

=head2 run [ARGS]

Calls each of the functions registered with this hook with the given
arguments.

=cut

sub run {
    my $self = shift;
    my @args = @_;
    return map {$_->(@args)} @$self;
}

=head2 add SUBREF

Registers a given subroutine with this hook

=cut

sub add {
    my $self = shift;
    my $func = shift;
    die("Not a coderef!") unless ref($func) eq 'CODE';
    push @$self, $func;
}

=head2 clear

Remove all functions registered with this hook.

=cut

sub clear {
    my $self = shift;
    @$self = ();
}

package BarnOwl::Hooks;

=head1 BarnOwl::Hooks

=head1 DESCRIPTION

C<BarnOwl::Hooks> exports a set of C<BarnOwl::Hook> objects made
available by BarnOwl internally. 

=head2 USAGE

Modules wishing to respond to events in BarnOwl should register
functions with these hooks.

=head2 EXPORTS

None by default. Either import the hooks you need explicitly, or refer
to them with fully-qualified names. Available hooks are:

=over 4

=item $startup

Called on BarnOwl startup, and whenever modules are
reloaded. Functions registered with the C<$startup> hook get a true
argument if this is a reload, and false if this is a true startup

=item $shutdown

Called before BarnOwl shutdown

=item $receiveMessage

Called with a C<BarnOwl::Message> object every time BarnOwl appends a
new message to its message list

=item $mainLoop

Called on every pass through the C<BarnOwl> main loop. This is
guaranteed to be called at least once/sec and may be called more
frequently.

=item $getBuddyList

Called to display buddy lists for all protocol handlers. The result
from every function registered with this hook will be appended and
displayed in a popup window, with zephyr formatting parsed.

=back

=cut

use Exporter;

our @EXPORT_OK = qw($startup $shutdown
                    $receiveMessage $mainLoop
                    $getBuddyList);

our %EXPORT_TAGS = (all => [@EXPORT_OK]);

our $startup = BarnOwl::Hook->new;
our $shutdown = BarnOwl::Hook->new;
our $receiveMessage = BarnOwl::Hook->new;
our $mainLoop = BarnOwl::Hook->new;
our $getBuddyList = BarnOwl::Hook->new;

# Internal startup/shutdown routines called by the C code

sub _load_owlconf {
    # load the config  file
    if ( -r $BarnOwl::configfile ) {
        undef $@;
        package main;
        do $BarnOwl::configfile;
        die $@ if $@;
        package BarnOwl;
        if(*BarnOwl::format_msg{CODE}) {
            # if the config defines a legacy formatting function, add 'perl' as a style 
            BarnOwl::_create_style("perl", "BarnOwl::_format_msg_legacy_wrap",
                                   "User-defined perl style that calls BarnOwl::format_msg"
                                   . " with legacy global variable support");
            BarnOwl::set("-q default_style perl");
        }
    }
}

# These are the internal hooks called by the barnowl C code, which
# take care of dispatching to the appropriate perl hooks, and deal
# with compatibility by calling the old, fixed-name hooks.

sub _startup {
    for my $e (@BarnOwl::__startup_errors) {
        BarnOwl::admin_message('Startup', $e);
    }
    @BarnOwl::__startup_errors = ();
    
    _load_owlconf();

    if(eval {require BarnOwl::ModuleLoader}) {
        eval {
            BarnOwl::ModuleLoader->load_all;
        };
        BarnOwl::error("$@") if $@;

    } else {
        BarnOwl::error("Can't load BarnOwl::ModuleLoader, loadable module support disabled:\n$@");
    }
    
    $startup->run(0);
    BarnOwl::startup() if *BarnOwl::startup{CODE};
}

sub _shutdown {
    $shutdown->run;
    
    BarnOwl::shutdown() if *BarnOwl::shutdown{CODE};
    BarnOwl::message_list()->close;
}

sub _receive_msg {
    my $m = shift;

    $receiveMessage->run($m);
    
    BarnOwl::receive_msg($m) if *BarnOwl::receive_msg{CODE};
}

sub _mainloop_hook {
    $mainLoop->run;
    BarnOwl::mainloop_hook() if *BarnOwl::mainloop_hook{CODE};
}

sub _get_blist {
    return join("\n", $getBuddyList->run);
}

################################################################################
# Built-in perl styles
################################################################################
package BarnOwl::Style::Default;
################################################################################
# Branching point for various formatting functions in this style.
################################################################################
sub format_message($)
{
    my $m = shift;

    if ( $m->is_loginout) {
        return format_login($m);
    } elsif($m->is_ping && $m->is_personal) {
        return ( "\@b(PING) from \@b(" . $m->pretty_sender . ")\n" );
    } elsif($m->is_admin) {
        return "\@bold(OWL ADMIN)\n" . indentBody($m);
    } else {
        return format_chat($m);
    }
}

BarnOwl::_create_style("default", "BarnOwl::Style::Default::format_message", "Default style");

################################################################################

sub time_hhmm {
    my $m = shift;
    my ($time) = $m->time =~ /(\d\d:\d\d)/;
    return $time;
}

sub format_login($) {
    my $m = shift;
    return sprintf(
        '@b<%s%s> for @b(%s) (%s) %s',
        uc( $m->login ),
        $m->login_type,
        $m->pretty_sender,
        $m->login_extra,
        time_hhmm($m)
       );
}

sub format_chat($) {
    my $m = shift;
    my $header;
    if ( $m->is_personal ) {
        if ( $m->direction eq "out" ) {
            $header = ucfirst $m->type . " sent to " . $m->pretty_recipient;
        } else {
            $header = ucfirst $m->type . " from " . $m->pretty_sender;
        }
    } else {
        $header = $m->context;
        if(defined $m->subcontext) {
            $header .= ' / ' . $m->subcontext;
        }
        $header .= ' / @b{' . $m->pretty_sender . '}';
    }

    if($m->opcode) {
        $header .= " [" . $m->opcode . "]";
    }
    $header .= "  " . time_hhmm($m);
    my $sender = $m->long_sender;
    $sender =~ s/\n.*$//s;
    $header .= " " x (4 - ((length $header) % 4));
    $header .= "(" . $sender . '@color[default]' . ")";
    my $message = $header . "\n". indentBody($m);
    if($m->is_personal && $m->direction eq "in") {
        $message = BarnOwl::Style::boldify($message);
    }
    return $message;
}

sub indentBody($)
{
    my $m = shift;

    my $body = $m->body;
    if ($m->{should_wordwrap}) {
      $body = BarnOwl::wordwrap($body, BarnOwl::getnumcols()-8);
    }
    # replace newline followed by anything with
    # newline plus four spaces and that thing.
    $body =~ s/\n(.)/\n    $1/g;
    # Trim trailing newlines.
    $body =~ s/\n*$//;
    return "    ".$body;
}

package BarnOwl::Style::OneLine;
################################################################################
# Branching point for various formatting functions in this style.
################################################################################
use constant BASE_FORMAT => '%s %-13.13s %-11.11s %-12.12s ';
sub format_message($) {
  my $m = shift;

#  if ( $m->is_zephyr ) {
#    return format_zephyr($m);
#  }
  if ( $m->is_loginout ) {
    return format_login($m);
  }
  elsif ( $m->is_ping) {
    return format_ping($m);
  }
  elsif ( $m->is_admin || $m->is_loopback) {
    return format_local($m);
  }
  else {
    return format_chat($m);
  }
}

BarnOwl::_create_style("oneline", "BarnOwl::Style::OneLine::format_message", "Formats for one-line-per-message");

################################################################################

sub format_login($) {
  my $m = shift;
  return sprintf(
    BASE_FORMAT,
    '<',
    $m->type,
    uc( $m->login ),
    $m->pretty_sender)
    . ($m->login_extra ? "at ".$m->login_extra : '');
}

sub format_ping($) {
  my $m = shift;
  return sprintf(
    BASE_FORMAT,
    '<',
    $m->type,
    'PING',
    $m->pretty_sender)
}

sub format_chat($)
{
  my $m = shift;
  my $dir = lc($m->{direction});
  my $dirsym = '-';
  if ($dir eq 'in') {
    $dirsym = '<';
  }
  elsif ($dir eq 'out') {
    $dirsym = '>';
  }

  my $line;
  if ($m->is_personal) {
    $line= sprintf(BASE_FORMAT,
		   $dirsym,
		   $m->type,
		   '',
		   ($dir eq 'out'
		      ? $m->pretty_recipient
		      : $m->pretty_sender));
  }
  else {
    $line = sprintf(BASE_FORMAT,
		    $dirsym,
		    $m->context,
		    $m->subcontext,
		    ($dir eq 'out'
		       ? $m->pretty_recipient
		       : $m->pretty_sender));
  }

  my $body = $m->{body};
  $body =~ tr/\n/ /;
  $line .= $body;
  $line = BarnOwl::Style::boldify($line) if ($m->is_personal && lc($m->direction) eq 'in');
  return $line;
}

# Format locally generated messages
sub format_local($)
{
  my $m = shift;
  my $type = uc($m->{type});
  my $line = sprintf(BASE_FORMAT, '<', $type, '', '');
  my $body = $m->{body};
  $body =~ tr/\n/ /;
  return $line.$body;
}

package BarnOwl::Style;

# This takes a zephyr to be displayed and modifies it to be displayed
# entirely in bold.
sub boldify($)
{
    local $_ = shift;
    if ( !(/\)/) ) {
        return '@b(' . $_ . ')';
    } elsif ( !(/\>/) ) {
        return '@b<' . $_ . '>';
    } elsif ( !(/\}/) ) {
        return '@b{' . $_ . '}';
    } elsif ( !(/\]/) ) {
        return '@b[' . $_ . ']';
    } else {
        my $txt = "\@b($_";
        $txt =~ s/\)/\)\@b\[\)\]\@b\(/g;
        return $txt . ')';
    }
}


# switch to package main when we're done
package main;

# Shove a bunch of fake entries into @INC so modules can use or
# require them without choking
$::INC{$_} = 1 for (qw(BarnOwl.pm BarnOwl/Hooks.pm
                       BarnOwl/Message.pm BarnOwl/Style.pm));

1;

