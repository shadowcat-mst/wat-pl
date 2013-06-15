use strict;
use warnings FATAL => 'all';
use ExtUtils::MakeMaker;

(do 'maint/Makefile.PL.include' or die $@) unless -f 'META.yml';

WriteMakefile(
  NAME => 'Wat',
  VERSION_FROM => 'lib/Wat.pm',
  PREREQ_PM => {
    'Safe::Isa' => 0,
    'Moo' => 0,
    'Try::Tiny' => 0,
    'JSONY' => 0,
    'Data::Dumper::Concise' => 0,
    'strictures' => 0,
    'Module::Runtime' => 0,
  },
);
