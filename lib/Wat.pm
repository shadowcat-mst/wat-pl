package Wat;

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
use Data::Dumper::Concise;
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
  } elsif (
    blessed($cmb)
    and $o->$_isa('Wat::Cons') and $o->{car}->$_isa('Wat::Sym')
    and $cmb->can(my $method_name = $o->{car}{name})
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

sub Wat::_Catch::wat_combine {
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

sub elt {
  (reduce { $a->{cdr} } $_[0], ('cdr') x $_[1])->{car}
}

sub make_env {
  bless({ bindings => ($_[0]->{bindings}||{}) }, 'Wat::Env');
}
sub lookup {
  my ($e, $name) = @_;
  fail("unbound: ${name}") unless exists $e->{bindings}{$name};
  return $e->{bindings}{$name};
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
  $self->{car}->wat_match($e, $rhs->{car});
  $self->{cdr}->wat_match($e, $rhs->{cdr});
  return;
}
sub Wat::Nil::wat_match {
  my ($self, $e, $rhs) = @_;
  fail("NIL expected, but got: ${\($rhs||'undef')}") unless $rhs||'' eq NIL;
  return;
}
sub Wat::Ign::wat_match {}
sub Wat::Ign::wat_repr { '#ignore' }

## Utilities

sub fail {
  our @Wat_Stack;
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
    unless ($c->$_isa('Wat::Cons')) {
      push @ary, Sym('#rest'), $c;
      last;
    }
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
    if ($val eq '#ignore') {
      return IGN;
    } elsif (looks_like_number($val)) {
      return $val;
    } else {
      return Sym($val);
    }
  } elsif ($ref eq 'ARRAY') {
    return parse_array_value($val);
  } else {
    return $val;
  }
}

sub parse_array_value {
  my ($val) = @_;
  if (@$val >= 2 and $val->[-2] eq '#rest') {
    array_to_list(
      [ map parse_value($_), @{$val}[0..$#$val-2] ],
      parse_value($val->[-1])
    );
  } else {
    array_to_list([ map parse_value($_), @$val ]);
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
    [ def => wrap => nwrap(\&wrap) ],
    [ def => unwrap => nwrap(\&unwrap) ],
  ## Values
    [ def => cons => nwrap(\&Cons) ],
    [ def => 'cons?' => nwrap(sub { $_[0]->$_isa('Wat::Cons') }) ],
    [ def => 'nil?' => nwrap(sub { $_[0] eq NIL }) ],
    [ def => 'symbol?' => nwrap(sub { $_[0]->$_isa('Wat::Sym') }) ],
    [ def => 'symbol-name' => nwrap(sub { $_[0]->{name} }) ],
    [ def => 'if' => bless({}, 'Wat::If') ],

  ## Primitives

    [ def => 'quote' => [ '--vau' => [ 'x' ], [ '#ignore', 'x' ] ] ],
    [ def => 'string', [ '--vau', [ 'sym' ], '#ignore', [ 'symbol-name', 'sym' ] ] ],
    (map [ def => $_ => n_binop($_) ],
       qw(+ - * / % ** << >> x . < <= > >= == != <=> cmp lt le gt ge eq ne & |)
    ),
    (map [ def => $_ => n_unop($_) ], qw(- ! ~)),
  ]
}

sub new {
  my ($class) = @_;
  my $env = make_env;
  env_bind($env, Sym('def'), bless({}, 'Wat::Def'));
  env_bind($env, Sym('begin'), bless({}, 'Wat::Begin'));
  my $new = bless({ env => $env }, $class);
  $new->run(primitives);
  return $new;
}

sub run {
  local our @Wat_Stack;
  evaluate($_[0]->{env}, undef, undef, parse_value($_[1]));
}

1;
