package Wat::REPL;

use Data::Dumper::Concise;
use IO::All;
use Module::Runtime qw(use_module);
use Moo;

has wat_class => (is => 'ro', default => sub { 'Wat' });

has wat => (is => 'lazy', builder => sub {
  my ($self) = @_;
  use_module($self->wat_class)->new;
});

has readline => (is => 'lazy', builder => sub {
  use_module('Term::ReadLine')->new('Wat');
});

has init_file => (is => 'ro');

sub BUILD {
  my ($self) = @_;
  if (my $init_file = $self->init_file) {
    $self->wat->run_jsony(io->file($init_file)->all);
  }
}

sub loop {
  my ($self) = @_;
  my $rl = $self->readline;
  my $wat = $self->wat;
  LINE: while (defined(my $line = $rl->readline('> '))) {
    my ($res, $raw);
    eval { $res = Dumper(Wat::repr($raw = $wat->run_jsony($line))); 1 }
      or do { print "Run failed: $@\n"; next LINE };
    print $res;
    $wat->def('_', $raw);
  }
}

sub run_as_script {
  my ($class, @args) = @_;
  $class->new(init_file => $ARGV[0], @args)->loop;
}

1;
