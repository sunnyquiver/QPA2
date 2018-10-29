gap> START_TEST( "algebra" );

gap> Q := LeftQuiver( "Q(3)[a:1->1,b:1->2,c:2->3,d:2->3]" );;
gap> kQ := PathAlgebra( Rationals, Q );;
gap> A := kQ / [ kQ.aaa ];;
gap> BasisPathsBetweenVertices( A, Q[ 1 ], Q[ 2 ] ) = [ Q.b, Q.ba, Q.baa ];
true

gap> IsAdmissibleQuiverAlgebra( PathAlgebra( Rationals, LeftQuiver( "Q(2)[a:1->2,b:2->1]" ) ) );
false
gap> IsAdmissibleQuiverAlgebra( PathAlgebra( Rationals, LeftQuiver( "Q(2)[a:1->2,b:1->2]" ) ) );
true

gap> STOP_TEST( "algebra" );
