gap> START_TEST( "representation" );

# test layered representation
gap> A := LeftNakayamaAlgebra( Rationals, [ 3, 2, 1 ] );;
gap> Ar := AlgebraAsRepresentationEnv( A );;
gap> Arr1 := AsLayeredRepresentation( 1, Ar );;
gap> Arr2 := AsLayeredRepresentation( 2, Ar );;
gap> Ar_1 := AsFlatRepresentation( 1, Arr1 );;
gap> Ar_2 := AsFlatRepresentation( 2, Arr2 );;
gap> Ar_1 = Ar;
true
gap> Ar_2 = Ar;
true

gap> STOP_TEST( "representation" );
