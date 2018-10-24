gap> START_TEST( "ideal" );

# test IsAdmissibleIdeal
gap> kQ := PathAlgebra( Rationals, LeftQuiver( "Q(2)[a:1->2,b:2->1]" ) );;
gap> I := QuiverAlgebraTwoSidedIdeal( kQ, [] );;
gap> IsAdmissibleIdeal( I );
false
gap> kQ := PathAlgebra( Rationals, LeftQuiver( "Q(2)[a:1->2,b:1->2]" ) );;
gap> I := QuiverAlgebraTwoSidedIdeal( kQ, [] );;
gap> IsAdmissibleIdeal( I );
true

gap> STOP_TEST( "ideal" );
