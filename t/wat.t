use strictures 1;
use Test::More;
use Wat;

#::Dwarn(Wat::list(def => '--vau', bless({}, 'Wat::__Vau')));

my $wat = Wat->new;

#::Dwarn($wat->{env}{bindings}{'def'});

#::Dwarn($wat->run('+'));

#::Dwarn([ sort keys %{$wat->{env}{bindings}} ]);

is($wat->run([ '+', 1, 4 ]), '5', 'Simple addition exists');

is(
  $wat->run([ '.', [ 'string', 'Joe' ], [ 'string', 'Bob' ] ]),
  'JoeBob',
  'string concatenation'
);

is(
  $wat->run_jsony(q[let [[i 0]] [begin [while [< i 5] [set! i [+ i 1]]] i]]),
  5,
  'let + while + set!'
);

is(
  $wat->run_jsony(q[[
    let [[x 0] [inc [vau [s] env [eval [list set! s [list + s 1]] env]]]]
      [begin [inc x] [inc x] x]
  ]]),
  2,
  'vau for inc works'
);

is($wat->run('+')->(1, 2), 3, 'Applicative coderef overloading ok');

my $obj = bless({}, 'MyClass');

sub MyClass::test { "foo $_[1]" }

$wat->run([ def => myobj => $obj ]);

is(
  $wat->run([ myobj => test => [ string => 'bar' ] ]), 'foo bar',
  'method invocation ok'
);

done_testing;
