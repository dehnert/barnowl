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

=head2 zephyr_getsubs

Returns the list of subscription triples <class,instance,recipient>,
separated by newlines.

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

=head2 create_style NAME OBJECT

Creates a new barnowl style with the given NAME defined by the given
object. The object must have a C<description> method which returns a
string description of the style, and a and C<format_message> method
which accepts a C<BarnOwl::Message> object and returns a string that
is the result of formatting the message for display.

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

=head2 quote STRING

Return a version of STRING fully quoted to survive processing by
BarnOwl's command parser.

=cut

sub quote {
    my $str = shift;
    return "''" if $str eq '';
    if ($str !~ /['" ]/) {
        return "$str";
    }
    if ($str !~ /'/) {
        return "'$str'";
    }
    $str =~ s/"/"'"'"/g;
    return '"' . $str . '"';
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

our %view_cache;

sub get_name   {shift->{name}};
sub message {
    my $self = shift;
    my $idx = shift;
    my $val = shift;
    if(defined $val) {
        vec($self->{messages}, $idx, 1) = $val;
    }
    return vec($self->{messages}, $idx, 1);
};
sub get_filter {shift->{filter}};

sub next_fwd      {shift->{next_fwd}};
sub next_bk       {shift->{next_bk}};
sub at_start      {shift->{at_start}};
sub at_end        {shift->{at_end}};
sub is_empty      {shift->{is_empty}};

sub new {
    my $class = shift;
    my $name = shift;
    my $filter = shift;
    if(defined($view_cache{$filter})) {
        my $self = $view_cache{$filter};
        return $self;
    }

    my $self  = {messages  => "",
                 name      => $name,
                 filter    => $filter,
                 is_empty  => 1};
    bless $self, $class;
    $self->reset;
    $view_cache{$filter} = $self;
    return $self;
}

sub consider_message {
    my $self = shift;
    my $msg = shift;
    for (values %view_cache) {
        $_->_consider_message($msg);
    }
}

sub _consider_message {
    my $self = shift;
    my $msg  = shift;
    return unless $self->at_end;
    if(BarnOwl::filter_message_match($self->get_filter, $msg)) {
        $self->message($msg->{id}, 1);
        $self->{is_empty} = 0;
    }
    $self->{next_fwd} = $msg->{id} + 1;
}

sub reset {
    my $self = shift;
    $self->{messages} = "";
    $self->{at_start} = $self->{at_end} = 0;
    $self->{next_fwd} = $self->{next_bk} = -1;
}

sub recalculate_around {
    my $self = shift;
    my $where = shift;
    BarnOwl::debug("recalulate @{[$self->get_filter]} around $where");
    if($where == 0) {
        return if $self->at_start;
        $self->{at_start} = 1;
        $self->{at_end}   = 0;
        $self->{next_bk}  = -1;
        $self->{next_fwd} = 0;
    } elsif($where < 0) {
        return if $self->at_end;
        $self->{at_start} = 0;
        $self->{at_end}   = 1;
        $self->{next_bk}  = -1;
        $self->{next_fwd} = undef;
    } else {
        if(defined($self->next_fwd) &&
           defined($self->next_bk) &&
           $self->next_fwd > $where &&
           $self->next_bk < $where) {
            BarnOwl::debug("Hit cache for @{[$self->get_filter]}");
            return;
        } else {
            $self->{at_end} = $self->{at_start} = 0;
            $self->{next_fwd} = $where;
            $self->{next_bk} = $where - 1;
        }
    }
    $self->{is_empty} = 1;
    $self->{messages} = "";
    $self->fill_forward;
    $self->fill_back;
}

my $FILL_STEP = 100;

sub fill_back {
    my $self = shift;
    return if $self->at_start;
    my $pos  = $self->next_bk;
    BarnOwl::debug("Fill back from $pos...");
    my $ml   = BarnOwl::message_list();
    $ml->iterate_begin($pos, 1);
    my $count = 0;
    my $m;
    while($count++ < $FILL_STEP) {
        $m = $ml->iterate_next;
        $self->{next_fwd} = $m->{id} + 1 unless defined $self->{next_fwd};
        unless(defined $m) {
            BarnOwl::debug("Hit start in fill_back.");
            $self->{at_start} = 1;
            last;
        }
        if(BarnOwl::filter_message_match($self->get_filter, $m)) {
            $self->{is_empty} = 0;
            $self->message($m->{id}, 1);
        }
        $self->{next_bk} = $m->{id} - 1;
    }
    $ml->iterate_done;
}

sub fill_forward {
    my $self = shift;
    return if $self->at_end;
    my $pos  = $self->next_fwd;
    BarnOwl::debug("Fill forward from $pos...");
    my $ml   = BarnOwl::message_list();
    $ml->iterate_begin($pos, 0);
    my $count = 0;
    my $m;
    while($count++ < $FILL_STEP) {
        $m = $ml->iterate_next;
        unless(defined $m) {
            BarnOwl::debug("Hit end in fill_forward.");
            $self->{at_end} = 1;
            last;
        }
        if(BarnOwl::filter_message_match($self->get_filter, $m)) {
            $self->{is_empty} = 0;
            $self->message($m->{id}, 1);
        }
        $self->{next_fwd} = $m->{id} + 1;
    }
    $ml->iterate_done;
    BarnOwl::debug("After fill, at @{[$self->next_fwd]}: " . unpack("b*", $self->{messages}));
}

sub invalidate_filter {
    my $filter = shift;
    delete $view_cache{$filter};
}

#####################################################################
#####################################################################

package BarnOwl::View::Iterator;

sub view {return shift->{view}}
sub index {return shift->{index}}

sub new {
    my $class = shift;
    my $self = {
        view     => undef,
        index    => undef,
        at_start => 0,
        at_end   => 0
       };
    return bless $self, $class;
}

sub invalidate {
    my $self = shift;
    $self->{view} = undef;
    $self->{index} = undef;
    $self->{at_start} = $self->{at_end} = 0;
}

sub initialize_at_start {
    my $self = shift;
    my $view = shift;
    $self->{view}  = $view;
    $self->{index} = -1;
    $self->{at_start} = $self->{at_end} = 0;
    $view->recalculate_around(0);
    $self->next;
    BarnOwl::debug("Initialize at start");
}

sub initialize_at_end {
    my $self = shift;
    my $view = shift;
    $self->{view}  = $view;
    $view->recalculate_around(-1);
    $self->{index} = $view->next_fwd;
    $self->{at_start} = $self->{at_end} = 0;
    $self->prev;
    BarnOwl::debug("Initialize at end");
}

sub initialize_at_id {
    my $self = shift;
    my $view = shift;
    my $id   = shift;
    $self->{view} = $view;
    $self->{index} = $id;
    $self->{at_start} = $self->{at_end} = 0;
    $view->recalculate_around($id);
    if(!$view->message($id)) {
        $self->next;
    }
    BarnOwl::debug("Initialize at $id");
}

sub clone {
    my $self = shift;
    my $other = shift;
    BarnOwl::debug("clone from @{[$other->{index}||0]}");
    $self->{view} = $other->{view};
    $self->{index} = $other->{index};
    $self->{at_start} = $other->{at_start};
    $self->{at_end} = $other->{at_end};
}

sub has_prev {
    my $self = shift;
    return 0 if $self->at_start;
    my $rv;
    my $idx = $self->index;
    $self->prev;
    $rv = !$self->at_start;
    $self->{index} = $idx;
    $self->{at_start} = 0;
    return $rv;
}

sub has_next {
    my $self = shift;
    return 0 if $self->at_end;
    my $idx = $self->index;
    my $rv;
    $self->next;
    $rv = !$self->at_end;
    $self->{index} = $idx;
    $self->{at_end} = 0;
    return $rv;
}

sub at_start {shift->{at_start}};
sub at_end {shift->{at_end}};

sub valid {
    my $self = shift;
    return defined($self->view) &&
            !$self->at_start &&
            !$self->at_end;
}

sub prev {
    my $self = shift;
    return if $self->at_start;
    $self->{index} = $self->view->next_fwd if $self->at_end;
    do {
        $self->{index}--;
        if($self->{index} == $self->view->next_bk) {
            $self->view->fill_back;
        }
    } while(!$self->view->message($self->index)
            && $self->index >= 0);

    BarnOwl::debug("PREV newid=@{[$self->index]}");

    if($self->index < 0) {
        BarnOwl::debug("At start");
        $self->{at_start} = 1;
    }
    $self->{at_end} = 0;
}

sub next {
    my $self = shift;
    return if $self->at_end;
    BarnOwl::debug("NEXT: next_fwd=@{[$self->view->next_fwd]}");
    do {
        $self->{index}++;
        if($self->index == $self->view->next_fwd) {
            BarnOwl::debug("Forward: fill, id=@{[$self->index]}");
            $self->view->fill_forward;
        }
    } while(!$self->view->message($self->index)
            && $self->index < $self->view->next_fwd);

    BarnOwl::debug("NEXT newid=@{[$self->index]}");

    if(!$self->view->message($self->index)) {
        $self->{at_end} = 1;
    }
    $self->{at_start} = 0;
}

sub get_message {
    my $self = shift;
    BarnOwl::debug("get_message: index=@{[$self->index]}");
    return BarnOwl::message_list->get_by_id($self->index);
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

# This is for back-compat with old messages that set these properties
# New protocol implementations are encourages to user override these
# methods.
sub replycmd         { return shift->{replycmd}};
sub replysendercmd   { return shift->{replysendercmd}};

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

sub replycmd {return 'loopwrite';}
sub replysendercmd {return 'loopwrite';}

#####################################################################
#####################################################################

package BarnOwl::Message::AIM;

use base qw( BarnOwl::Message );

# all non-loginout AIM messages are private for now...
sub is_private {
    return !(shift->is_loginout);
}

sub replycmd {
    my $self = shift;
    if ($self->is_incoming) {
        return "aimwrite " . BarnOwl::quote($self->sender);
    } else {
        return "aimwrite " . BarnOwl::quote($self->recipient);
    }
}

sub replysendercmd {
    return shift->replycmd;
}

#####################################################################
#####################################################################

package BarnOwl::Message::Zephyr;

use constant WEBZEPHYR_PRINCIPAL => "daemon.webzephyr";
use constant WEBZEPHYR_CLASS     => "webzephyr";
use constant WEBZEPHYR_OPCODE    => "webzephyr";

use base qw( BarnOwl::Message );

sub strip_realm {
    my $sender = shift;
    my $realm = BarnOwl::zephyr_getrealm();
    $sender =~ s/\@$realm$//;
    return $sender;
}

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
    return strip_realm($m->sender);
}

sub pretty_recipient {
    my ($m) = @_;
    return strip_realm($m->recipient);
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

sub zephyr_cc {
    my $self = shift;
    return $1 if $self->body =~ /^\s*cc:\s+([^\n]+)/i;
    return undef;
}

sub replycmd {
    my $self = shift;
    my $sender = shift;
    $sender = 0 unless defined $sender;
    my ($class, $instance, $to, $cc);
    if($self->is_outgoing) {
        return $self->{zwriteline};
    }

    if($sender && $self->opcode eq WEBZEPHYR_OPCODE) {
        $class = WEBZEPHYR_CLASS;
        $instance = $self->sender;
        $to = WEBZEPHYR_PRINCIPAL;
    } elsif($self->class eq WEBZEPHYR_CLASS
            && $self->is_loginout) {
        $class = WEBZEPHYR_CLASS;
        $instance = $self->instance;
        $to = WEBZEPHYR_PRINCIPAL;
    } elsif($self->is_loginout || $sender) {
        $class = 'MESSAGE';
        $instance = 'PERSONAL';
        $to = $self->sender;
    } else {
        $class = $self->class;
        $instance = $self->instance;
        $to = $self->recipient;
        $cc = $self->zephyr_cc();
        if($to eq '*' || $to eq '') {
            $to = '';
        } elsif($to !~ /^@/) {
            $to = $self->sender;
        }
    }

    my $cmd;
    if(lc $self->opcode eq 'crypt') {
        $cmd = 'zcrypt';
    } else {
        $cmd = 'zwrite';
    }

    if (lc $class ne 'message') {
        $cmd .= " -c " . BarnOwl::quote($self->class);
    }
    if (lc $instance ne 'personal') {
        $cmd .= " -i " . BarnOwl::quote($self->instance);
    }
    if ($to ne '') {
        $to = strip_realm($to);
        if (defined $cc) {
            my @cc = grep /^[^-]/, ($to, split /\s+/, $cc);
            my %cc = map {$_ => 1} @cc;
            delete $cc{strip_realm(BarnOwl::zephyr_getsender())};
            @cc = keys %cc;
            $cmd .= " -C " . join(" ", @cc);
        } else {
            if(BarnOwl::getvar('smartstrip') eq 'on') {
                $to = BarnOwl::zephyr_smartstrip_user($to);
            }
            $cmd .= " $to";
        }
    }
    return $cmd;
}

sub replysendercmd {
    my $self = shift;
    return $self->replycmd(1);
}

#####################################################################
#####################################################################
#####################################################################

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
    return map {$self->_run($_,@args)} @$self;
}

sub _run {
    my $self = shift;
    my $fn = shift;
    my @args = @_;
    no strict 'refs';
    return $fn->(@args);
}

=head2 add SUBREF

Registers a given subroutine with this hook

=cut

sub add {
    my $self = shift;
    my $func = shift;
    die("Not a coderef!") unless ref($func) eq 'CODE' || !ref($func);
    return if grep {$_ eq $func} @$self;
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
                    $receiveMessage $newMessage
                    $mainLoop $getBuddyList);

our %EXPORT_TAGS = (all => [@EXPORT_OK]);

our $startup = BarnOwl::Hook->new;
our $shutdown = BarnOwl::Hook->new;
our $receiveMessage = BarnOwl::Hook->new;
our $newMessage = BarnOwl::Hook->new;
our $mainLoop = BarnOwl::Hook->new;
our $getBuddyList = BarnOwl::Hook->new;

# Internal startup/shutdown routines called by the C code

sub _load_perl_commands {
    # Load builtin perl commands
    BarnOwl::new_command(style => \&BarnOwl::Style::style_command,
                       {
                           summary => "creates a new style",
                           usage   => "style <name> perl <function_name>",
                           description =>
                           "A style named <name> will be created that will\n" .
                           "format messages using the perl function <function_name>.\n\n" .
                           "SEE ALSO: show styles, view -s, filter -s\n\n" .
                           "DEPRECATED in favor of BarnOwl::create_style(NAME, OBJECT)",
                          });
}

sub _load_owlconf {
    # load the config  file
    if ( -r $BarnOwl::configfile ) {
        undef $@;
        package main;
        do $BarnOwl::configfile;
        if($@) {
            BarnOwl::error("In startup: $@\n");
            return;
        }
        package BarnOwl;
        if(*BarnOwl::format_msg{CODE}) {
            # if the config defines a legacy formatting function, add 'perl' as a style 
            BarnOwl::create_style("perl", BarnOwl::Style::Legacy->new(
                "BarnOwl::format_msg",
                "User-defined perl style that calls BarnOwl::format_msg"
                . " with legacy global variable support",
                1));
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
    
    _load_perl_commands();
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

sub _new_msg {
    my $m = shift;

    $newMessage->run($m);
    
    BarnOwl::new_msg($m) if *BarnOwl::new_msg{CODE};
}

sub _mainloop_hook {
    $mainLoop->run;
    BarnOwl::mainloop_hook() if *BarnOwl::mainloop_hook{CODE};
}

sub _get_blist {
    return join("\n", $getBuddyList->run);
}

sub _invalidate_filter {
    my $filter = shift;
    BarnOwl::View::invalidate_filter($filter);
}

################################################################################
# Built-in perl styles
################################################################################
package BarnOwl::Style::Default;
################################################################################
# Branching point for various formatting functions in this style.
################################################################################
sub format_message
{
    my $self = shift;
    my $m    = shift;
    my $fmt;

    if ( $m->is_loginout) {
        $fmt = $self->format_login($m);
    } elsif($m->is_ping && $m->is_personal) {
        $fmt = $self->format_ping($m);
    } elsif($m->is_admin) {
        $fmt = $self->format_admin($m);
    } else {
        $fmt = $self->format_chat($m);
    }
    $fmt = BarnOwl::Style::boldify($fmt) if $self->should_bold($m);
    return $fmt;
}

sub should_bold {
    my $self = shift;
    my $m = shift;
    return $m->is_personal && $m->direction eq "in";
}

sub description {"Default style";}

BarnOwl::create_style("default", "BarnOwl::Style::Default");

################################################################################

sub format_time {
    my $self = shift;
    my $m = shift;
    my ($time) = $m->time =~ /(\d\d:\d\d)/;
    return $time;
}

sub format_login {
    my $self = shift;
    my $m = shift;
    return sprintf(
        '@b<%s%s> for @b(%s) (%s) %s',
        uc( $m->login ),
        $m->login_type,
        $m->pretty_sender,
        $m->login_extra,
        $self->format_time($m)
       );
}

sub format_ping {
    my $self = shift;
    my $m = shift;
    return "\@b(PING) from \@b(" . $m->pretty_sender . ")";
}

sub format_admin {
    my $self = shift;
    my $m = shift;
    return "\@bold(OWL ADMIN)\n" . $self->indent_body($m);
}

sub format_chat {
    my $self = shift;
    my $m = shift;
    my $header = $self->chat_header($m);
    return $header . "\n". $self->indent_body($m);
}

sub chat_header {
    my $self = shift;
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
    $header .= "  " . $self->format_time($m);
    $header .= $self->format_sender($m);
    return $header;
}

sub format_sender {
    my $self = shift;
    my $m = shift;
    my $sender = $m->long_sender;
    $sender =~ s/\n.*$//s;
    return "  (" . $sender . '@color[default]' . ")";
}

sub indent_body
{
    my $self = shift;
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

package BarnOwl::Style::Basic;
our @ISA=qw(BarnOwl::Style::Default);

sub description {"Compatability alias for the default style";}

BarnOwl::create_style("basic", "BarnOwl::Style::Basic");

package BarnOwl::Style::OneLine;
# Inherit format_message to dispatch
our @ISA = qw(BarnOwl::Style::Default);

use constant BASE_FORMAT => '%s %-13.13s %-11.11s %-12.12s ';

sub description {"Formats for one-line-per-message"}

BarnOwl::create_style("oneline", "BarnOwl::Style::OneLine");

################################################################################

sub format_login {
  my $self = shift;
  my $m = shift;
  return sprintf(
    BASE_FORMAT,
    '<',
    $m->type,
    uc( $m->login ),
    $m->pretty_sender)
    . ($m->login_extra ? "at ".$m->login_extra : '');
}

sub format_ping {
  my $self = shift;
  my $m = shift;
  return sprintf(
    BASE_FORMAT,
    '<',
    $m->type,
    'PING',
    $m->pretty_sender)
}

sub format_chat
{
  my $self = shift;
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
  return $line;
}

# Format owl admin messages
sub format_admin
{
  my $self = shift;
  my $m = shift;
  my $line = sprintf(BASE_FORMAT, '<', 'ADMIN', '', '');
  my $body = $m->{body};
  $body =~ tr/\n/ /;
  return $line.$body;
}

package BarnOwl::Style;

# This takes a zephyr to be displayed and modifies it to be displayed
# entirely in bold.
sub boldify
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

sub style_command {
    my $command = shift;
    if(scalar @_ != 3 || $_[1] ne 'perl') {
        die("Usage: style <name> perl <function>\n");
    }
    my $name = shift;
    my $perl = shift;
    my $fn   = shift;
    {
        # For historical reasons, assume unqualified references are
        # in main::
        package main;
        no strict 'refs';
        unless(*{$fn}{CODE}) {
            die("Unable to create style '$name': no perl function '$fn'\n");
        }
    }
    BarnOwl::create_style($name, BarnOwl::Style::Legacy->new($fn));
}

package BarnOwl::Style::Legacy;

sub new {
    my $class = shift;
    my $func  = shift;
    my $desc  = shift;
    my $useglobals = shift;
    $useglobals = 0 unless defined($useglobals);
    return bless {function    => $func,
                  description => $desc,
                  useglobals  => $useglobals}, $class;
}

sub description {
    my $self = shift;
    return $self->{description} ||
    ("User-defined perl style that calls " . $self->{function});
};

sub format_message {
    my $self = shift;
    if($self->{useglobals}) {
        $_[0]->legacy_populate_global();
    }
    {
      package main;
      no strict 'refs';
      goto \&{$self->{function}};
    }
}


# switch to package main when we're done
package main;

# Shove a bunch of fake entries into @INC so modules can use or
# require them without choking
$::INC{$_} = 1 for (qw(BarnOwl.pm BarnOwl/Hooks.pm
                       BarnOwl/Message.pm BarnOwl/Style.pm));

1;

