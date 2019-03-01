gap> START_TEST( "restriction" );

gap> Q1 := LeftQuiver( "Q(2)[a:1->2]" );;
gap> Q2 := LeftQuiver( "Q(3)[b:1->2,c:2->3]" );;
gap> m := QuiverHomomorphism( Q1, Q2, [ Q2[2], Q2[3] ], [ Q2.c ] );;
gap> A1 := PathAlgebra( Rationals, Q1 );;
gap> A2 := PathAlgebra( Rationals, Q2 );;
gap> repA1 := CategoryOfQuiverRepresentations( A1 );;
gap> repA2 := CategoryOfQuiverRepresentations( A2 );;
gap> rest := RestrictionFunctor( m, repA2, repA1 );;
gap> R := QuiverRepresentation( A2, [ 1, 2, 2 ], [ [ 5, 7 ], [ 4, 5, 6, 7 ] ] );;
gap> restR := ApplyFunctor( rest, R );;
gap> VectorSpacesOfRepresentation( restR ) = VectorSpacesOfRepresentation( R ){ [ 2, 3 ] };
true
gap> MapForArrow( restR, Q1.a ) = MapForArrow( R, Q2.c );
true

gap> STOP_TEST( "restriction" );
