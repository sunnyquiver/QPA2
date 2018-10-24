gap> START_TEST( "representation" );

# test Display for representations
gap> A := PathAlgebra( Rationals, LeftQuiver( "Q(3)[a:1->2,b:2->3]" ) );;
gap> R := QuiverRepresentation( A, [ 1, 0, 0 ], [] );;
gap> Display( R );
A representation over the algebra Rationals * Q given by the data:

[4m[32mFor vertex (1):[0m
An object in vector spaces over Rationals of dimension 1

[4m[32mFor vertex (2):[0m
An object in vector spaces over Rationals of dimension 0

[4m[32mFor vertex (3):[0m
An object in vector spaces over Rationals of dimension 0

[4m[32mFor arrow (a):[0m
Linear transformation given (in row-convention) by the 1x0 matrix 
[ [  ] ]

[4m[32mFor arrow (b):[0m
Linear transformation given (in row-convention) by the 0x0 matrix 
[  ]


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
