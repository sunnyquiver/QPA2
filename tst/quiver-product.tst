gap> START_TEST( "quiver-product" );

gap> L := List( [ "A3", "A4", "A5" ], LeftDynkinQuiver );;
gap> Q := QuiverProduct( L );;
gap> pr := Cartesian( List( L, PrimitivePaths ) );;
gap> ForAll( List( pr, paths -> ProductPathFactors( PathInProductQuiver( Q, paths ) ) = paths ), IdFunc );
true

gap> STOP_TEST( "quiver-product" );
