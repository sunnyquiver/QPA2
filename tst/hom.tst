gap> START_TEST( "hom" );

# test that Coefficients works in a Hom space of representations
gap> Q := LeftQuiver( "Q(3)[a:1->2,b:2->3]" );;
gap> A := PathAlgebra( Rationals, Q );;
gap> R := QuiverRepresentation( A, [ 1, 1, 1 ], [] );;
gap> hom := Hom( R, R );;
gap> B := CanonicalBasis( hom );;
gap> idR := IdentityMorphism( R );;
gap> idRcoeff := Coefficients( B, idR );;
gap> idR_ := LinearCombination( B, idRcoeff );;
gap> idR_ = idR;
true

gap> STOP_TEST( "hom" );
