package Wat;

our $VERSION = '0.001001';

# Shamelessly copying http://github.com/manuel/wat-js for great justice.
#
# Considered to be under https://github.com/manuel/wat-js/blob/master/LICENSE
# until I can figure out if I can apply normal perl licensing on top of that
# without violating anything.

use strictures 1;
use Safe::Isa;
use Sub::Quote;
use Scalar::Util qw(looks_like_number blessed);
use List::Util qw(reduce);
use Try::Tiny;
use JSONY;
use Data::Dumper::Concise;
use Module::Runtime qw(use_module);
use constant DEBUG => !!$ENV{WAT_DEBUG};
no warnings 'once'; # reduce's $a/$b
no warnings 'qw'; # using #rest etc. in qw()

sub NIL; sub IGN;

## Continuations

sub Continuation {
  bless({ fun => $_[0], next => $_[1] }, 'Wat::Continuation')
}

sub is_Continuation { $_[0]->$_isa('Wat::Continuation') }

sub Capture {
  bless({ prompt => $_[0], handler => $_[1] }, 'Wat::Capture')
}

sub is_Capture { $_[0]->$_isa('Wat::Capture'); }

sub capture_frame {
  my ($capture, $fun) = @_;
  $capture->{k} = Continuation($fun, $capture->{k});
}

sub continue_frame {
  my ($k, $f) = @_;
  $k->{fun}->($k->{next}, $f);
}

sub evaluate {
  my ($e, $k, $f, $x) = @_;
  if ($x->$_can('wat_eval')) {
    our @Wat_Stack;
    local @Wat_Stack = (@Wat_Stack, $x);
    if (@Wat_Stack > 80) {
      fail("Stack depth limit exceeded");
    }
    if (DEBUG) {
      warn 'Evaluating: '.Dumper(repr($x));
      my $r = $x->wat_eval($e, $k, $f);
      warn 'Evaluated: '.Dumper(repr($x)).'  -> '.Dumper(repr($r));
      return $r;
    }
    return $x->wat_eval($e, $k, $f);
  } else {
    return $x;
  }
}

sub Sym { bless({ name => $_[0] }, 'Wat::Sym') }

sub Wat::Sym::wat_eval {
  my ($self, $e, $k, $f) = @_;
  return lookup($e, $self->{name});
}

sub Cons { bless({ car => $_[0], cdr => $_[1] }, 'Wat::Cons') }

sub Wat::Cons::wat_eval {
  my ($self, $e, $k, $f) = @_;
  my $op = do {
    if (is_Continuation($k)) {
      continue_frame($k, $f);
    } else {
      evaluate($e, undef, undef, $self->{car});
    }
  };
  if (is_Capture($op)) {
    capture_frame($op, sub { $self->wat_eval($e, @_) });
    return $op;
  }
  return combine($e, undef, undef, $op, $self->{cdr});
}
sub Wat::Cons::wat_repr {
  my ($self) = @_;
  return [ map repr($_), @{list_to_array($self)} ];
}

## Operative and applicative combiners

sub combine {
  my ($e, $k, $f, $cmb, $o) = @_;
  if ($cmb->$_can('wat_combine')) {
    return $cmb->wat_combine($e, $k, $f, $o);
  } elsif (ref($cmb) and ref($cmb) eq 'CODE') {
    return nwrap($cmb)->wat_combine($e, $k, $f, $o);
  } elsif (
    blessed($cmb)
    and $o->$_isa('Wat::Cons') and $o->{car}->$_isa('Wat::Sym')
    and $cmb->can(
          my $method_name = do { (my $x = $o->{car}{name}) =~ s/-/_/g; $x }
        )
  ) {
    return nwrap(sub { $cmb->$method_name(@_) })->wat_combine(
             $e, $k, $f, $o->{cdr}
           );
  }
  fail("not a combiner: $cmb");
}

sub Opv {
  bless({ p => $_[0], ep => $_[1], x => $_[2], e => $_[3] }, 'Wat::Opv');
}

sub Apv { bless({ cmb => $_[0] }, 'Wat::Apv') }

sub wrap { Apv($_[0]) }

sub unwrap { $_[0]->{cmb} }

sub Wat::Opv::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  my $xe = make_env($self->{e});
  env_bind($xe, $self->{p}, $o);
  env_bind($xe, $self->{ep}, $e);
  return evaluate($xe, $k, $f, $self->{x});
}
sub Wat::Opv::wat_repr {
  [ '--vau', map repr($_), @{$_[0]}{qw(p ep x)} ];
}

{ package Wat::Apv;
  use overload (
    '&{}' => 'to_perl_sub',
    fallback => 1,
  );
}

sub Wat::Apv::to_perl_sub {
  my ($self) = @_;
  my $cmb = $self->{cmb};
  sub {
    bless({ env => make_env() }, 'Wat')->run([ $cmb, @_ ])
  }
}

sub Wat::Apv::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  my $args = do {
    if (is_Continuation($k)) {
      continue_frame($k, $f);
    } else {
      eval_args($e, undef, undef, $o, NIL);
    }
  };
  if (is_Capture($args)) {
    capture_frame($args, sub { $self->wat_combine($e, @_, $o) });
    return $args;
  }
  return $self->{cmb}->wat_combine($e, undef, undef, $args);
}
sub Wat::Apv::wat_repr { [ wrap => repr($_[0]->{cmb}) ] }

sub eval_args {
  my ($e, $k, $f, $todo, $done) = @_;
  if ($todo eq NIL) {
    return reverse_list($done);
  }
  unless ($todo->$_isa('Wat::Cons')) {
    fail("Expected cons cell, got ${todo}");
  }
  my $arg = do {
    if (is_Continuation($k)) {
      continue_frame($k, $f);
    } else {
      evaluate($e, undef, undef, $todo->{car});
    }
  };
  if (is_Capture($arg)) {
    capture_frame($arg, sub { eval_args($e, @_, $todo, $done) });
    return $arg;
  }
  return eval_args($e, undef, undef, $todo->{cdr}, Cons($arg, $done));
}

## Built-in Combiners

sub Wat::__Vau::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  Opv(elt($o, 0), elt($o, 1), elt($o, 2), $e);
}

sub Wat::Def::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  my $val = do {
    if (is_Continuation($k)) {
      continue_frame($k, $f);
    } else {
      evaluate($e, undef, undef, elt($o, 1));
    }
  };
  if (is_Capture($val)) {
    capture_frame($val, sub { $self->wat_combine($e, @_, $o) });
    return $val;
  }
  return env_bind($e, elt($o, 0), $val);
}

sub Wat::Set::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  my $val = do {
    if (is_Continuation($k)) {
      continue_frame($k, $f);
    } else {
      evaluate($e, undef, undef, elt($o, 1));
    }
  };
  if (is_Capture($val)) {
    capture_frame($val, sub { $self->wat_combine($e, @_, $o) });
    return $val;
  }
  my $car = elt($o, 0);
  fail("Not a symbol: $car") unless $car->$_isa('Wat::Sym');
  return env_bind(lookup_env($e, $car->{name}), $car, $val);
}

sub Wat::Eval::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  return evaluate(elt($o, 1), $k, $f, elt($o, 0));
}

## First order control

sub Wat::Begin::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  if ($o eq NIL) {
    return undef;
  } else {
    return begin($e, $k, $f, $o);
  }
}

sub begin {
  my ($e, $k, $f, $xs) = @_;
  my $res = do {
    if (is_Continuation($k)) {
      continue_frame($k, $f);
    } else {
      evaluate($e, undef, undef, $xs->{car});
    }
  };
  if (is_Capture($res)) {
    capture_frame($res, sub { begin($e, @_, $xs) });
    return $res;
  }
  my $kdr = $xs->{cdr};
  if ($kdr eq NIL) {
    return $res;
  } else {
    return begin($e, undef, undef, $kdr);
  }
}

sub Wat::If::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  my $test = do {
    if (is_Continuation($k)) {
      continue_frame($k, $f);
    } else {
      evaluate($e, undef, undef, elt($o, 0));
    }
  };
  if (is_Capture($test)) {
    capture_frame($test, sub { $self->wat_combine($e, @_, $o) });
    return $test;
  }
  return evaluate($e, undef, undef, elt($o, ($test ? 1 : 2)));
}

sub Wat::__Loop::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  my $first = 1;
  while (1) {
    my $res = do {
      if ($first and is_Continuation($k)) {
        continue_frame($k, $f);
      } else {
        evaluate($e, undef, undef, elt($o, 0));
      }
    };
    $first = 0;
    if (is_Capture($res)) {
      capture_frame($res, sub { $self->wat_combine($e, @_, $o) });
      return $res;
    }
  }
}

sub Wat::__Catch::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  my $th = elt($o, 0);
  my $handler = elt($o, 1);
  my $res = try {
    if (is_Continuation($k)) {
      continue_frame($k, $f);
    } else {
      combine($e, undef, undef, $th, NIL);
    }
  } catch {
    combine($e, undef, undef, unwrap($handler), list($_));
  };
  if (is_Capture($res)) {
    capture_frame($res, sub { $self->wat_combine($e, @_, $o) });
  }
  return $res;
}

sub Wat::Finally::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  my $prot = elt($o, 0);
  my $cleanup = elt($o, 1);
  my $res = try {
    if (is_Continuation($k)) {
      continue_frame($k, $f);
    } else {
      evaluate($e, undef, undef, $prot);
    }
  } catch {
    # javascript try rethrows without a catch block.
    my $err = $_;
    do_cleanup($e, undef, undef, $cleanup, undef);
    die $err;
  };
  if (is_Capture($res)) {
    capture_frame($res, sub { $self->wat_combine($e, @_, $o) });
    return $res;
  } else {
    return do_cleanup($e, undef, undef, $cleanup, $res);
  }
}

sub do_cleanup {
  my ($e, $k, $f, $cleanup, $res) = @_;
  my $fres = do {
    if (is_Continuation($k)) {
      continue_frame($k, $f);
    } else {
      evaluate($e, undef, undef, $cleanup);
    }
  };
  if (is_Capture($fres)) {
    capture_frame($fres, sub { do_cleanup($e, @_, $cleanup, $res) });
    return $fres;
  } else {
    return $res;
  }
}

sub Wat::__PushPrompt::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  my $prompt = elt($o, 0);
  my $th = elt($o, 1);
  my $res = do {
    if (is_Continuation($k)) {
      continue_frame($k, $f);
    } else {
      combine($e, undef, undef, $th, NIL);
    }
  };
  if (is_Capture($res)) {
    if ($res->{prompt} eq $prompt) {
      my $continuation = $res->{k};
      my $handler = $res->{handler};
      return combine($e, undef, undef, $handler, Cons($continuation, NIL));
    } else {
      capture_frame($res, sub { $self->wat_combine($e, @_, $o) });
      return $res;
    }
  } else {
    return $res;
  }
}

sub Wat::__TakeSubcont::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  my $prompt = elt($o, 0);
  my $handler = elt($o, 1);
  my $cap = Capture($prompt, $handler);
  capture_frame($cap, sub { combine($e, undef, undef, $_[1], NIL) });
  return $cap;
}

sub Wat::__PushSubcont::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  my $thek = elt($o, 0);
  my $thef = elt($o, 1);
  my $res = do {
    if (is_Continuation($k)) {
      continue_frame($k, $f);
    } else {
      continue_frame($thek, $thef);
    }
  };
  if (is_Capture($res)) {
    capture_frame($res, sub { $self->wat_combine($e, @_, $o) });
    return $res;
  } else {
    return $res;
  }
}
  
sub DV { bless({ val => $_[0] }, 'Wat::DV') }

sub Wat::DNew::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  return DV(elt($o, 0));
}

sub Wat::DRef::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  return elt($o, 0)->{val};
}

sub Wat::__DLet::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  my $dv = elt($o, 0);
  my $val = elt($o, 1);
  my $th = elt($o, 2);
  local $dv->{val} = $val;
  my $res = do {
    if (is_Continuation($k)) {
      continue_frame($k, $f);
    } else {
      combine($e, undef, undef, $th, NIL);
    }
  };
  if (is_Capture($res)) {
    capture_frame($res, sub { $self->wat_combine($e, @_, $o) });
    return $res;
  } else {
    return $res;
  }
}

## Objects

sub NIL { our $_Nil ||= bless({}, 'Wat::Nil') }
sub IGN { our $_Ign ||= bless({}, 'Wat::Ign') }
sub RST { our $_Rst ||= bless({}, 'Wat::Rst') }

sub elt {
  (reduce { $a->{cdr} } $_[0], ('cdr') x $_[1])->{car}
}

sub make_env {
  bless({ bindings => {}, outer => $_[0] }, 'Wat::Env');
}
sub lookup_env {
  my ($e, $name) = @_;
  my $le = $e;
  while ($le) {
    return $le if exists $le->{bindings}{$name};
    $le = $le->{outer};
  }
  fail("unbound: ${name}");
}
sub lookup {
  my ($e, $name) = @_;
  lookup_env($e, $name)->{bindings}{$name};
}
sub env_bind {
  my ($e, $lhs, $rhs) = @_;
  $lhs->wat_match($e, $rhs);
  return $rhs;
}
sub Wat::Sym::wat_match {
  my ($self, $e, $rhs) = @_;
  warn "Binding ${\$self->{name}} to: ".Dumper(repr($rhs)) if DEBUG;
  $e->{bindings}{$self->{name}} = $rhs;
  return;
}
sub Wat::Sym::wat_repr { $_[0]->{name} }
sub Wat::Cons::wat_match {
  my ($self, $e, $rhs) = @_;
  if ($self->{car}->$_isa('Wat::Rst')) {
    $self->{cdr}{car}->wat_match($e, $rhs);
  } else {
    $self->{car}->wat_match($e, $rhs->{car});
    $self->{cdr}->wat_match($e, $rhs->{cdr});
  }
  return;
}
sub Wat::Nil::wat_match {
  my ($self, $e, $rhs) = @_;
  fail("NIL expected, but got: ${\($rhs||'undef')}") unless $rhs||'' eq NIL;
  return;
}
sub Wat::Nil::wat_repr { 'nil' }
sub Wat::Rst::wat_match { fail("Attempt to match #rest") }
sub Wat::Rst::wat_repr { '#rest' }
sub Wat::Ign::wat_match {}
sub Wat::Ign::wat_repr { '#ignore' }

## Utilities

sub fail {
  our @Wat_Stack;
  die $_[0] if ref($_[0]);
  die $_[0].' with stack '.Dumper([ map repr($_), @Wat_Stack ])
}
sub repr {
  my ($r) = @_;
  if ($r->$_can('wat_repr')) {
    $r->wat_repr
  } elsif (!ref($r)) {
    if (looks_like_number($r)) {
      $r
    } else {
      [ 'string', $r ]
    }
  } else {
    "${r}"
  }
}
sub list { array_to_list([ @_ ]) }
sub list_star {
  if (@_ > 1) {
    my $end = pop(@_);
    array_to_list([ @_ ], $end);
  } else {
    array_to_list(\@_);
  }
}
sub array_to_list {
  my ($ary, $end) = @_;
  reduce { Cons($b, $a) } $end||NIL, reverse @$ary;
}
sub list_to_array {
  my ($c) = @_;
  my @ary;
  while ($c ne NIL) {
    push @ary, $c->{car};
    $c = $c->{cdr};
  }
  return \@ary;
}
sub reverse_list { list reverse @{list_to_array($_[0])} }

## Parser

sub parse_value {
  my ($val) = @_;
  my $ref = ref($val);
  if (!$ref) {
    if (!defined($val)) {
      return $val;
    } elsif ($val eq '#ignore') {
      return IGN;
    } elsif ($val eq '#rest') {
      return RST;
    } elsif (looks_like_number($val)) {
      return $val;
    } elsif ($val =~ s/^://) {
      return $val;
    } else {
      return Sym($val);
    }
  } elsif ($ref eq 'ARRAY') {
    return array_to_list([ map parse_value($_), @$val ]);
  } else {
    return $val;
  }
}

## Native calling

sub NFun { bless({ fun => $_[0] }, 'Wat::NFun') }

sub Wat::NFun::wat_combine {
  my ($self, $e, $k, $f, $o) = @_;
  $self->{fun}->(@{list_to_array($o)});
}
sub nwrap { wrap(NFun($_[0])) }
sub n_unop { wrap(NFun(quote_sub $_[0].q{ $_[0]})) }
sub n_binop { wrap(NFun(quote_sub join ' ', '$_[0]', $_[0], '$_[1]')) }

sub primitives {
  [begin =>

  ## Core

  ## Fexprs
    [ def => '--vau', bless({}, 'Wat::__Vau') ],
    [ def => eval => wrap(bless({}, 'Wat::Eval')) ],
    [ def => 'make-environment' => nwrap(\&make_env) ],
    [ def => 'set!' => bless({}, 'Wat::Set') ],
    [ def => wrap => nwrap(\&wrap) ],
    [ def => unwrap => nwrap(\&unwrap) ],

  ## Values
    [ def => cons => nwrap(\&Cons) ],
    [ def => 'cons?' => nwrap(sub { $_[0]->$_isa('Wat::Cons') }) ],
    [ def => nil => NIL ],
    [ def => 'nil?' => nwrap(sub { $_[0] eq NIL }) ],
    [ def => 'symbol?' => nwrap(sub { $_[0]->$_isa('Wat::Sym') }) ],
    [ def => 'symbol-name' => nwrap(sub { $_[0]->{name} }) ],

  ## First order control
    [ def => 'if' => bless({}, 'Wat::If') ],
    [ def => '--loop' => bless({}, 'Wat::__Loop') ],
    [ def => throw => nwrap(sub { fail @_ }) ],
    [ def => '--catch' => wrap(bless({}, 'Wat::__Catch')) ],
    [ def => finally => bless({}, 'Wat::Finally') ],

  ## Delimited control
    [ def => '--push-prompt' => wrap(bless({}, 'Wat::__PushPrompt')) ],
    [ def => '--take-subcont' => wrap(bless({}, 'Wat::__TakeSubcont')) ],
    [ def => '--push-subcont' => wrap(bless({}, 'Wat::__PushSubcont')) ],

  ## Dynamically scoped variables
    [ def => dnew => wrap(bless({}, 'Wat::DNew')) ],
    [ def => '--dlet' => wrap(bless({}, 'Wat::__DLet')) ],
    [ def => dref => wrap(bless({}, 'Wat::DRef')) ],

  ## Optimisation
    [ def => 'list*' => nwrap(\&list_star) ],

  ## Primitives
    [ def => quote => [ '--vau' => [ 'x' ], '#ignore', 'x' ] ],
    [ def => list => [ wrap => [
      '--vau' => arglist => '#ignore' => 'arglist'
    ] ] ],
    [ def => string => [ '--vau', [ 'sym' ], '#ignore', [ 'symbol-name', 'sym' ] ] ],

    [ def => 'make-macro-expander',
      [ wrap =>
        [ '--vau', [ 'expander' ], '#ignore',
          [ '--vau', 'operands', 'env',
            [ eval =>
              [ eval =>
                [ cons => 'expander', 'operands' ],
                [ 'make-environment']
              ],
              'env'
            ]]]]],

    [ def => 'vau',
      [ 'make-macro-expander' =>
        [ '--vau' => [ qw(params env-param #rest body) ], '#ignore',
          [ list => qw(--vau params env-param), [ qw(cons begin body) ]]
        ]]],

    [ def => 'macro',
      [ 'make-macro-expander' =>
        [ vau => [ qw(params #rest body) ], '#ignore',
          [ list => 'make-macro-expander' =>
            [ qw(list* vau params #ignore body) ]
          ]
        ]]],

    [ def => 'lambda',
      [ macro => [ qw(params #rest body) ],
        [ list => wrap => [ qw(list* vau params #ignore body) ] ]]],

    [ def => 'loop',
      [ macro => 'body',
        [ list => '--loop' => [ qw(list* begin body) ] ]]],

    [ def => 'catch',
      [ macro => [ qw(protected handler) ],
        [ list => '--catch' =>
          [ 'list', 'lambda', [], 'protected' ],
          'handler',
        ]]],

    [ def => 'push-prompt',
      [ macro => [ qw(prompt #rest body) ],
        [ list => '--push-prompt' =>
          'prompt',
          [ 'list*', 'lambda', [], 'body' ],
        ]]],

    [ def => 'take-subcont',
      [ macro => [ qw(prompt k #rest body) ],
        [ list => '--take-subcont' =>
          'prompt',
          [ 'list*', 'lambda', [ qw(list k) ], 'body' ],
        ]]],

    [ def => 'push-subcont',
      [ macro => [ qw(k #rest body) ],
        [ list => '--push-subcont' =>
          'k',
          [ 'list*', 'lambda', [], 'body' ],
        ]]],

  ## Operators
    (map [ def => $_ => n_binop($_) ],
       qw(+ - * / % ** << >> x . < <= > >= == != <=> cmp lt le gt ge eq ne & |
          && || and or)
    ),
    (map [ def => $_ => n_unop($_) ], qw(- ! ~ not)),
    [ def => '===' => n_binop('eq') ],

  ## Data structures

    [ def => hash => sub { +{ @_ } } ],
    [ def => array => sub { [ @_ ] } ],
    [ def => fetch => sub {
        my $r = ref($_[0])||'';
        if ($r eq 'HASH') { $_[0]->{$_[1]} }
        elsif ($r eq 'ARRAY') { $_[0]->[$_[1]] }
        else { fail("Neither hash nor array (in fetch): $r") }
      }
    ],
    [ def => store => sub {
        my $r = ref($_[0])||'';
        if ($r eq 'HASH') { $_[0]->{$_[1]} = $_[2] }
        elsif ($r eq 'ARRAY') { $_[0]->[$_[1]] = $_[2] }
        else { fail("Neither hash nor array (in store): $r") }
      }
    ],
    [ def => keys => sub {
        my $r = ref($_[0])||'';
        if ($r eq 'HASH') { array_to_list([ keys %{$_[0]} ]) }
        elsif ($r eq 'ARRAY') { array_to_list([ 0..$#{$_[0]} ]) }
        else { fail("Neither hash nor array (in keys): $r") }
      }
    ],
    [ def => values => sub {
        my $r = ref($_[0])||'';
        if ($r eq 'HASH') { array_to_list([ values %{$_[0]} ]) }
        elsif ($r eq 'ARRAY') { array_to_list($_[0]) }
        else { fail("Neither hash nor array (in values): $r") }
      }
    ],
    [ def => push => sub { push @{$_[0]}, $_[1]; $_[1] } ],
    [ def => unshift => sub { unshift @{$_[0]}, $_[1]; $_[1] } ],
    [ def => pop => sub { pop @{$_[0]} } ],
    [ def => shift => sub { shift @{$_[0]} } ],
    [ def => 'exists-key' => sub { exists $_[0]->{$_[1]} } ],
    [ def => 'delete-key' => sub { delete $_[0]->{$_[1]} } ],

  ## Classes

    [ def => 'call-method' => sub {
        my ($inv, $meth) = (shift, shift);
        (ref($inv) ? $inv : use_module($inv))->$meth(@_)
      }
    ],
    [ def => new => [
      macro => [ qw(inv #rest args) ],
        [ qw(list* call-method inv :new args) ],
    ]],
    [ def => can => [
      macro => [ qw(inv #rest args) ],
        [ qw(list* call-method inv :can args) ],
    ]],
  ]
}

sub basics {
  our $Basics ||= do { local $/; <DATA> };
}

sub new {
  my ($class) = @_;
  my $env = make_env;
  env_bind($env, Sym('def'), bless({}, 'Wat::Def'));
  env_bind($env, Sym('begin'), bless({}, 'Wat::Begin'));
  my $new = bless({ env => $env }, $class);
  $new->run(primitives);
  $new->run_jsony(basics);
  return $new;
}

sub run {
  local our @Wat_Stack;
  evaluate($_[0]->{env}, undef, undef, parse_value($_[1]));
}

sub run_jsony {
  my ($self, $jsony) = @_;
  $self->run(decode_jsony($jsony));
}

1;

=head1 NAME

Wat - An operative (fexpr based) embedded lisp with delimited continuations

=cut

__DATA__
        ["begin",
         
         ["def", "compose",
          ["lambda", ["f", "g"], ["lambda", ["arg"], ["f", ["g", "arg"]]]]],

         ["def", "car", ["lambda", [["x", "#rest", "#ignore"]], "x"]],
         ["def", "cdr", ["lambda", [["#ignore", "#rest", "x"]], "x"]],
         ["def", "caar", ["compose", "car", "car"]],
         ["def", "cadr", ["compose", "car", "cdr"]],
         ["def", "cdar", ["compose", "cdr", "car"]],
         ["def", "cddr", ["compose", "cdr", "cdr"]],

         ["def", "define-macro",
          ["macro", [["name", "#rest", "params"], "#rest", "body"],
           ["list", "def", "name", ["list*", "macro", "params", "body"]]]],

         ["define-macro", ["define", "lhs", "#rest", "rhs"],
          ["if", ["cons?", "lhs"],
           ["list", "def", ["car", "lhs"], ["list*", "lambda", ["cdr", "lhs"], "rhs"]],
           ["list", "def", "lhs", ["car", "rhs"]]]],

         ["define", ["map-list", "f", "lst"],
           ["if", ["nil?", "lst"],
            [],
            ["cons", ["f", ["car", "lst"]], ["map-list", "f", ["cdr", "lst"]]]]],

         ["define-macro", ["let", "bindings", "#rest", "body"],
          ["cons",
           ["list*", "lambda", ["map-list", "car", "bindings"], "body"],
           ["map-list", "cadr", "bindings"]]],

         ["define-macro", ["let*", "bindings", "#rest", "body"],
          ["if", ["nil?", "bindings"],
           ["list*", "let", [], "body"],
           ["list", "let", ["list", ["car", "bindings"]],
            ["list*", "let*", ["cdr", "bindings"], "body"]]]],

         ["define-macro", ["where", "expr", "#rest", "bindings"],
          ["list", "let", "bindings", "expr"]],

         ["define-macro", ["where*", "expr", "#rest", "bindings"],
          ["list", "let*", "bindings", "expr"]],

         ["define", ["call-with-escape", "fun"],
          ["let", [["fresh", ["list", null]]],
           ["catch", ["fun", ["lambda", ["val"], ["throw", ["list", "fresh", "val"]]]],
            ["lambda", ["exc"],
             ["if", ["&&", ["cons?", "exc"], ["===", "fresh", ["car", "exc"]]],
              ["cadr", "exc"],
              ["throw", "exc"]]]]]],

         ["define-macro", ["let-escape", "name", "#rest", "body"],
          ["list", "call-with-escape", ["list*", "lambda", ["list", "name"], "body"]]],

         ["define", ["call-while", "test-fun", "body-fun"],
          ["let-escape", "return",
           ["loop",
            ["if", ["test-fun"],
             ["body-fun"],
             ["return", null]]]]],

         ["define-macro", ["while", "test", "#rest", "body"],
          ["list", "call-while",
           ["list", "lambda", [], "test"],
           ["list*", "lambda", [], "body"]]]

]
