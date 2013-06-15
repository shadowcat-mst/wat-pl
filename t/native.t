use strictures 1;
use Test::More;
use Wat;

my $wat = Wat->new;

is_deeply(
  $wat->run_jsony(q[def myh [ hash :foo 1 :bar 2 ]]),
  { foo => 1, bar => 2 },
  'Hash construction ok'
);

is_deeply(
  $wat->run_jsony(q[def mya [ array 1 2 3 4 ]]),
  [ 1..4 ],
  'Array construction ok'
);

is_deeply(
  [ sort @{$wat->run_jsony(q[eval [cons array [keys myh]]])} ],
  [ 'bar', 'foo' ],
  'Hash keys ok'
);

is_deeply(
  $wat->run_jsony(q[eval [cons array [keys mya]]]),
  [ 0,1,2,3 ],
  'Array keys ok'
);

is_deeply(
  [ sort @{$wat->run_jsony(q[eval [cons array [values myh]]])} ],
  [ 1, 2 ],
  'Hash values ok'
);

is_deeply(
  $wat->run_jsony(q[eval [cons array [values mya]]]),
  [ 1 .. 4 ],
  'Array values ok'
);

is(
  $wat->run_jsony(q[push mya 5]),
  5,
  'push returns value'
);

is_deeply(
  $wat->run('mya'),
  [ 1 .. 5 ],
  'push modifies array'
);

is(
  $wat->run_jsony(q[unshift mya 0]),
  0,
  'unshift returns value'
);

is_deeply(
  $wat->run('mya'),
  [ 0 .. 5 ],
  'unshift modifies array'
);

is(
  $wat->run_jsony(q[pop mya]),
  5,
  'pop returns value'
);

is_deeply(
  $wat->run('mya'),
  [ 0 .. 4 ],
  'pop modifies array'
);

is(
  $wat->run_jsony(q[shift mya]),
  0,
  'shift returns value'
);

is_deeply(
  $wat->run('mya'),
  [ 1 .. 4 ],
  'shift modifies array'
);

is(
  $wat->run_jsony(q[fetch mya 2]),
  3,
  'fetch loads array value'
);

is(
  $wat->run_jsony(q[store mya 2 7]),
  7,
  'store returns value'
);

is_deeply(
  $wat->run('mya'),
  [ 1, 2, 7, 4 ],
  'store modifies array'
);

is(
  $wat->run_jsony(q[fetch myh :foo]),
  1,
  'fetch loads hash values'
);

is(
  $wat->run_jsony(q[store myh :baz 3]),
  3,
  'store returns value'
);

is_deeply(
  $wat->run('myh'),
  { foo => 1, bar => 2, baz => 3 },
  'store modifies hash'
);

is(
  $wat->run_jsony(q[exists-key myh :foo]),
  1,
  'exists on present key'
);

is(
  $wat->run_jsony(q[exists-key myh :lycanthrope]),
  '',
  'exists on absent key'
);

is(
  $wat->run_jsony(q[delete-key myh :bar]),
  2,
  'delete returns value'
);

is_deeply(
  $wat->run('myh'),
  { foo => 1, baz => 3 },
  'delete modifies hash'
);

done_testing;
