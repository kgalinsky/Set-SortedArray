package Set::SortedArray;

use strict;
use warnings;

=head1 NAME

Set::SortedArray - sets stored as sorted arrays for speed

=head1 VERSION

Version 0.0.1

=cut

use version; our $VERSION = qv('0.0.1');

=head1 SYNOPSIS

    use Set::SortedArray;
    my $s = Set::SortedArray->new( qw/ d b c a e /);
    my $t = Set::SortedArray->new_presorted( qw/ b c e f g / );

    print $s->as_string, "\n";
    print $s, "\n";

    $u = $s->union($t);
    $i = $s->intersection($t);
    $d = $s->difference($t);
    $e = $s->symmetric_difference($t);
    $a = $s->asymmetric_difference($t);
    $v = $s->unique($t);

    $u = $s + $t;   # union
    $i = $s * $t;   # intersection
    $d = $s - $t;   # difference
    $e = $s % $t;   # symmetric_difference
    $v = $s / $t;   # unique

    $eq = $s->is_equal($t);
    $dj = $s->is_disjoint($t);
    $ps = $s->is_proper_subset($t);
    $pS = $s->is_proper_superset($t);
    $is = $s->is_subset($t);
    $iS = $s->is_superset($t);

    $eq = $s == $t; # equal
    $dj = $s != $t; # disjoint
    $ps = $s < $t;  # is_proper_subset
    $pS = $s > $t;  # is_proper_superset
    $is = $s <= $t; # is_subset
    $iS = $s >= $t; # is_superset

    # amalgam of a few of the above
    $cmp = $s->compare($t);
    $cmp = $s <=> $t;

=head2 DESCRIPTION

Create a set that is stored as a sorted array. Similar to Set::Scalar, except
optimized for speed and memory.

=cut

use overload
  '""'  => \&_as_string,
  '+'   => \&union,
  '*'   => \&intersection,
  '-'   => \&difference,
  '%'   => \&symmetric_difference,
  '/'   => \&unique,
  '=='  => \&is_equal,
  '!='  => \&is_disjoint,
  '<'   => \&is_proper_subset,
  '>'   => \&is_proper_superset,
  '<='  => \&is_subset,
  '>='  => \&is_superset,
  '<=>' => \&compare;

=head1 CONSTRUCTORS

=head2 new

    $set = Set::SortedArray->new();
    $set = Set::SortedArray->new(@members);

=head2 new_presorted

    $set = Set::SortedArray->new_presorted(@members);

Quicker than new, but doesn't sort data.

=cut

sub new {
    my $class = shift;
    my $self = bless [ sort @_ ], $class;
    return $self;
}

sub new_presorted {
    my $class = shift;
    my $self = bless [@_], $class;
    return $self;
}

=head1 MODIFYING

TODO

=cut

=head1 DISPLAYING

=head2 as_string

    print $s->as_string, "\n";
    print $s, "\n";

=head2 as_string_callback

    Set::SortedArray->as_string_callback(sub { ... });

=cut

sub _as_string { shift->as_string(@_) }

sub as_string { return '(' . join( ' ', @{ $_[0]->_members } ) . ')' }

sub as_string_callback {
    my ( $class, $callback ) = @_;
    no strict 'refs';
    no warnings;
    *{"${class}::as_string"} = $callback;
}

=head1 QUERYING

=head2 members

=head2 size

=cut

sub members  { return @{ $_[0] } }
sub _members { return $_[0] }               # return arrayref of members
sub size     { return scalar @{ $_[0] } }

=head1 DERIVING

=head2 union

    $u = $s->union($t);
    $u = $s->union($t, $v);
    $u = $s + $t;
    $u = $s + $t + $v; # inefficient

=cut

sub union {
    pop unless ( UNIVERSAL::can( $_[-1], '_members' ) );

    my %members;
    foreach my $set ( map { $_->_members } @_ ) {
        foreach my $member (@$set) {
            $members{$member} ||= $member;
        }
    }

    my $union = bless [ sort values %members ], ref( $_[0] );
    return $union;
}

=head2 intersection

    $i = $s->intersection($t);
    $i = $s->intersection($t, $u);
    $i = $s * $t;
    $i = $s * $t * $u; # inefficient

=cut

sub intersection {
    pop unless ( UNIVERSAL::can( $_[-1], '_members' ) );

    my $total = @_;

    my %members;
    my %counts;

    foreach my $set ( map { $_->_members } @_ ) {
        foreach my $member (@$set) {
            $members{$member} ||= $member;
            $counts{$member}++;
        }
    }

    my $intersection =
      bless [ sort grep { $counts{$_} == $total } values %members ],
      ref $_[0];
    return $intersection;
}

=head2 difference

    $d = $s->difference($t);
    $d = $s - $t;

=cut

sub difference {
    my ( $this, $that ) = map { $_->_members } @_[ 0, 1 ];

    my $i = 0;
    my $j = 0;

    my $difference = [];
    while ( ( $i < @$this ) && ( $j < @$that ) ) {
        my $member_i = $this->[$i];
        my $member_j = $that->[$j];

        if ( $member_i eq $member_j ) { $i++; $j++ }
        elsif ( $member_i lt $member_j ) { push @$difference, $member_i; $i++ }
        else                             { $j++ }
    }

    push @$difference, @$this[ $i .. $#$this ];

    my $class = ref($this);
    bless $difference, $class;

    return $difference;
}

=head2 symmetric_difference

    $e = $s->symmetric_difference($t);
    $e = $s % $t;

=cut

sub symmetric_difference {
    my ( $this, $that ) = map { $_->_members } @_[ 0, 1 ];

    my $i = 0;
    my $j = 0;

    my $difference = [];
    while ( ( $i < @$this ) && ( $j < @$that ) ) {
        my $member_i = $this->[$i];
        my $member_j = $that->[$j];

        if ( $member_i eq $member_j ) { $i++; $j++ }
        elsif ( $member_i lt $member_j ) { push @$difference, $member_i; $i++ }
        else                             { push @$difference, $member_j; $j++ }
    }

    push @$difference, @$this[ $i .. $#$this ];
    push @$difference, @$that[ $j .. $#$that ];

    my $class = ref($this);
    bless $difference, $class;

    return $difference;
}

=head2 asymmetric_difference

    $a = $s->asymmetric_difference($t);

Returns [ $s - $t, $t - $s ], but more efficiently.

=cut

sub asymmetric_difference {
    my ( $this, $that ) = map { $_->_members } @_[ 0, 1 ];

    my $i = 0;
    my $j = 0;

    my ( $difference, $add ) = ( [], [] );
    while ( ( $i < @$this ) && ( $j < @$that ) ) {
        my $member_i = $this->[$i];
        my $member_j = $that->[$j];

        if ( $member_i eq $member_j ) { $i++; $j++ }
        elsif ( $member_i lt $member_j ) { push @$difference, $member_i; $i++ }
        else                             { push @$add,        $member_j; $j++ }
    }

    push @$difference, @$this[ $i .. $#$this ];
    push @$add,        @$that[ $j .. $#$that ];

    my $class = ref($this);
    bless $difference, $class;
    bless $add,        $class;

    return [ $difference, $add ];
}

=head2 unique

    $v = $s->unique($t);
    $v = $s / $t;

=cut

sub unique {
    pop unless ( UNIVERSAL::can( $_[-1], '_members' ) );

    my %members;
    my %counts;

    foreach my $set ( map { $_->_members } @_ ) {
        foreach my $member (@$set) {
            $counts{$member}++;
        }
    }

    my $unique =
      bless [ sort grep { $counts{$_} == 1 } values %members ],
      ref $_[0];
    return $unique;
}

=head1 COMPARING

=head2 is_equal

    $eq = $s->is_equal($t);
    $eq = $s == $t;

=cut

sub is_equal {
    my ( $this, $that ) = map { $_->_members } @_[ 0, 1 ];
    return unless ( @$this == @$that );
    return _is_equal( $this, $that );
}

sub _is_equal {
    my ( $this, $that ) = @_;
    for ( my $i = 0 ; $i < @$this ; $i++ ) {
        return unless ( $this->[$i] eq $that->[$i] );
    }
    return 1;
}

=head2 is_disjoint

    $dj = $s->is_disjoint($t);
    $dj = $s != $t;

=cut

sub is_disjoint {
    my ( $this, $that ) = map { $_->_members } @_[ 0, 1 ];

    my $i = 0;
    my $j = 0;

    while ( ( $i < @$this ) && ( $j < @$that ) ) {
        my $member_i = $this->[$i];
        my $member_j = $that->[$j];

        if ( $member_i eq $member_j ) { return }
        elsif ( $member_i lt $member_j ) { $i++ }
        else                             { $j++ }
    }

    return 1;
}

=head2 is_proper_subset

    $ps = $s->is_proper_subset($t);
    $ps = $s < $t;

=head2 is_proper_superset

    $pS = $s->is_proper_superset($t);
    $pS = $s > $t;

=head2 is_subset

    $is = $s->is_subset($t);
    $is = $s <= $t;

=head2 is_superset

    $iS = $s->is_superset($t);
    $iS = $s >= $t;

=cut

sub is_proper_subset {
    my ( $this, $that ) = map { $_->_members } @_[ 0, 1 ];
    return unless ( @$this < @$that );
    return _is_subset( $this, $that );
}

sub is_proper_superset {
    my ( $this, $that ) = map { $_->_members } @_[ 0, 1 ];
    return unless ( @$this > @$that );
    return _is_subset( $that, $this );
}

sub is_subset {
    my ( $this, $that ) = map { $_->_members } @_[ 0, 1 ];
    return unless ( @$this <= @$that );
    return _is_subset( $this, $that );
}

sub is_superset {
    my ( $this, $that ) = map { $_->_members } @_[ 0, 1 ];
    return unless ( @$this >= @$that );
    return _is_subset( $that, $this );
}

sub _is_subset {
    my ( $this, $that ) = @_;

    my $i = 0;
    my $j = 0;

    while ( ( $i < @$this ) && ( $j < @$that ) ) {
        my $member_i = $this->[$i];
        my $member_j = $that->[$j];

        if ( $member_i eq $member_j ) { $i++; $j++; }
        elsif ( $member_i gt $member_j ) { $j++ }
        else                             { return }
    }

    return $i == @$this;
}

=head2 compare

    $cmp = $s->compare($t);
    $cmp = $s <=> $t;

C<compare> returns:

    0  if $s == $t
    1  if $s > $t
    -1 if $s < $t
    () otherwise

=cut

sub compare {
    my ( $this, $that ) = map { $_->_members } @_[ 0, 1 ];

    if ( my $cmp = $#$this <=> $#$that ) {
        return $cmp == 1
          ? ( _is_subset( $that, $this ) ? 1 : () )
          : ( _is_subset( $this, $that ) ? -1 : () );
    }
    else { return _is_equal( $this, $that ) ? 0 : () }
}

=head1 AUTHOR

"Kevin Galinsky", C<kgalinsky plus cpan at gmail dot com>

=head1 BUGS

Please report any bugs or feature requests to C<bug-set-sortedarray at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Set-SortedArray>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Set::SortedArray

You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Set-SortedArray>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Set-SortedArray>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Set-SortedArray>

=item * Search CPAN

L<http://search.cpan.org/dist/Set-SortedArray/>

=back

=head1 ACKNOWLEDGEMENTS

=head1 LICENSE AND COPYRIGHT

Copyright 2011 "Kevin Galinsky".

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=cut

1;    # End of Set::SortedArray
