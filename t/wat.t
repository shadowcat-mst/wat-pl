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

done_testing;
