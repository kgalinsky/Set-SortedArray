#!perl

use Test::More tests => 9;

require_ok('Set::SortedArray');

$a = Set::SortedArray->new(qw/ a b c     /);
$b = Set::SortedArray->new(qw/   b c d   /);
$c = Set::SortedArray->new(qw/     c d e /);

is( $a->union($b)->size, 4, 'union ab' );
is( $a->union( $b, $c )->size, 5, 'union abc' );

is( $a->intersection($b)->size, 2, 'intersection ab' );
is( $a->intersection( $b, $c )->size, 1, 'intersection abc' );

is($a->difference($b)->size, 1, 'a - b');
is($a->difference($c)->size, 2, 'a - c');

is($a->symmetric_difference($b)->size, 2, 'a % b');
is($a->symmetric_difference($c)->size, 4, 'a % c');