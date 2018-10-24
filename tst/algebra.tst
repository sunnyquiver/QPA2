gap> START_TEST( "algebra" );

gap> IsAdmissibleQuiverAlgebra( PathAlgebra( Rationals, LeftQuiver( "Q(2)[a:1->2,b:2->1]" ) ) );
false
gap> IsAdmissibleQuiverAlgebra( PathAlgebra( Rationals, LeftQuiver( "Q(2)[a:1->2,b:1->2]" ) ) );
true

gap> STOP_TEST( "algebra" );
