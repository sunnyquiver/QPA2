gap> START_TEST( "hom" );

# test that basis for Hom space works when there is a loop
gap> Q := RightQuiver( "Q(1)[a:1->1]" );;
gap> KQ := PathAlgebra(Rationals, Q);;
gap> rels := [KQ.aa];;
gap> A := KQ/rels;;
gap> mat := [ [[0,0],[1,0]] ];;
gap> N:= QuiverRepresentation( A, [2], mat );;
gap> hom := Hom( N, N );;
gap> Dimension( hom );
2

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

gap> A := LeftNakayamaAlgebra( Rationals, [ 3, 2, 1 ] );;
gap> B := LeftNakayamaAlgebra( Rationals, [ 2, 1 ] );;
gap> P1 := IndecProjRepresentations( A )[ 1 ];;
gap> T := TensorProductOfAlgebras( A, B );;
gap> PP5 := IndecProjRepresentations( T )[ 5 ];;
gap> hom := Hom( 1, PP5, P1 );;
gap> IsHomRepresentation( hom );
true
gap> Source( hom ) = PP5;
true
gap> Range( hom ) = P1;
true

gap> A := LeftNakayamaAlgebra( Rationals, [3,2,1] );;
gap> B := LeftNakayamaAlgebra( Rationals, [2,1] );;
gap> P := IndecProjBimodules([A,B]);;
gap> S := SimpleLeftModules(A);;
gap> h11 := Hom(LEFT, P[1], S[1]);;

gap> STOP_TEST( "hom" );
