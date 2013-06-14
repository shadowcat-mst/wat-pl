use strictures 1;
use Test::More;
use Wat;
use IO::Async::Loop;

my $loop = IO::Async::Loop->new;

my $wat = Wat->new;

$wat->run([ def => loop => $loop ]);

$wat->run_jsony(q[begin
  [def x 0]
  [let [[f [loop delay-future [string after] 0.1]]]
    [f on-done [lambda [] [set! x 1] [loop stop]]]]
]);

is($wat->run('x'), 0, 'Value initialised to 0');

$loop->run;

is($wat->run('x'), 1, 'Value updated on future completion');

$wat->run_jsony(q[begin
  [def x 0]
  [let* [
      [default-prompt [quote default-prompt]]
      [sleep [lambda [ms]
        [take-subcont default-prompt k
          [[loop delay-future [string after] [/ ms 1000]]
            on-done [lambda [] [push-prompt default-prompt [push-subcont k 3]]]]
        ]
      ]]
    ]
    [push-prompt default-prompt
      [set! x [sleep 100]]
      [loop stop]
    ]
  ]
]);

is($wat->run('x'), 0, 'x initialised to 0');

$loop->run;

is($wat->run('x'), 3, 'x updated after continuation resume');

done_testing;
