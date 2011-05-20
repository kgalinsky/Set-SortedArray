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

=head2 DESCRIPTION

=cut

use overload '""' => \&as_string, '+' => \&union, '*' => \&intersection;

=head1 CONSTRUCTORS

=head2 new

    $set = Set::SortedArray->new();
    $set = Set::SortedArray->new(@members);

=head2 new_presorted

    $set = Set::SortedArray->new_presorted(@_);

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

=cut

sub as_string { return join '', @{ $_[0] } }

=head1 QUERYING

=head2 members

=head2 size

=cut

sub members { return @{ $_[0] } }
sub size    { return scalar @{ $_[0] } }

=head1 DERIVING

=head2 union

=cut

sub union {
    my $self = shift;
    my %members;

    foreach my $set ( $self, @_ ) {
        foreach my $member (@$set) {
            $members{$member} ||= $member;
        }
    }

    my $union = bless [ sort values %members ], ref($self);
    return $union;
}

=head2 intersection

=cut

sub intersection {
    my $total = @_;
    my $self  = shift;

    my %members;
    my %counts;

    foreach my $set ( $self, @_ ) {
        foreach my $member (@$set) {
            $members{$member} ||= $member;
            $counts{$member}++;
        }
    }

    my $intersection =
      bless [ sort grep { $counts{$_} == $total } values %members ],
      ref $self;
    return $intersection;
}

=head2 difference

=cut

sub difference {
    my ( $self, $set ) = @_;

    my $i = 0;
    my $j = 0;

    my $difference = [];
    while ( ( $i < @$self ) && ( $j < @$set ) ) {
        my $member_i = $self->[$i];
        my $member_j = $set->[$j];

        if ( $member_i eq $member_j ) { $i++; $j++ }
        elsif ( $member_i lt $member_j ) { push @$difference, $member_i; $i++ }
        else                             { $j++ }
    }

    push @$difference, @$self[ $i .. $#$self ];

    my $class = ref($self);
    bless $difference, $class;

    return $difference;
}

=head2 symmetric_difference

=cut

sub symmetric_difference {
    my ( $self, $set ) = @_;

    my $i = 0;
    my $j = 0;

    my $difference = [];
    while ( ( $i < @$self ) && ( $j < @$set ) ) {
        my $member_i = $self->[$i];
        my $member_j = $set->[$j];

        if ( $member_i eq $member_j ) { $i++; $j++ }
        elsif ( $member_i lt $member_j ) { push @$difference, $member_i; $i++ }
        else                             { push @$difference, $member_j; $j++ }
    }

    push @$difference, @$self[ $i .. $#$self ];
    push @$difference, @$set[ $j .. $#$set ];

    my $class = ref($self);
    bless $difference, $class;

    return $difference;
}

=head2 asymmetric_difference

=cut

sub asymmetric_difference {
    my ( $self, $set ) = @_;

    my $i = 0;
    my $j = 0;

    my ( $difference, $add ) = ( [], [] );
    while ( ( $i < @$self ) && ( $j < @$set ) ) {
        my $member_i = $self->[$i];
        my $member_j = $set->[$j];

        if ( $member_i eq $member_j ) { $i++; $j++ }
        elsif ( $member_i lt $member_j ) { push @$difference, $member_i; $i++ }
        else                             { push @$add,        $member_j; $j++ }
    }

    push @$difference, @$self[ $i .. $#$self ];
    push @$add,        @$set[ $j .. $#$set ];

    my $class = ref($self);
    bless $difference, $class;
    bless $add,        $class;

    return [ $difference, $add ];
}

=head2 unique

=cut

=head1 AUTHOR

"Kevin Galinsky", C<< <"galinsky at broadinstitute.org"> >>

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
