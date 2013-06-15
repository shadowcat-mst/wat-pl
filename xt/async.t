use strictures 1;
use Test::More;
use Wat;

my $wat = Wat->new;

$wat->run_jsony(q[begin
  [def loop [new :IO::Async::Loop]]
  [def is [can :Test::More :is]]
  [def x 0]
  [let [[f [loop delay-future :after 0.1]]]
    [f on-done [lambda [] [set! x 1] [loop stop]]]]
  [is x 0 [string 'Value initialised to 0']]
  [loop run]
  [is x 1 [string 'Value updated on future completion']]
  [set! x 0]
  [let* [
      [default-prompt [quote default-prompt]]
      [sleep [lambda [ms]
        [take-subcont default-prompt k
          [[loop delay-future :after [/ ms 1000]]
            on-done [lambda [] [push-prompt default-prompt [push-subcont k 3]]]]
        ]
      ]]
    ]
    [push-prompt default-prompt
      [set! x [sleep 100]]
      [loop stop]
    ]
    [is x 0 [string 'x initialised to 0']]
    [loop run]
    [is x 3 [string 'x updated after continuation resume']]
  ]
]);

done_testing;
