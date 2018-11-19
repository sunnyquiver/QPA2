gap> START_TEST( "algebra" );

gap> Q := LeftQuiver( "Q(3)[a:1->2,b:2->3]" );;
gap> kQ := PathAlgebra( Rationals, Q );;
gap> e1 := 7 * kQ[1] + 8 * kQ[2] + 9 * kQ[3];;
gap> e2 := 10 * kQ.a + 100 * kQ.b;;
gap> e1 * e2 = 80 * kQ.a + 900 * kQ.b;
true
gap> e2 * e1 = 70 * kQ.a + 800 * kQ.b;
true
gap> ComposeElements( e1, e2 ) = e2 * e1;
true
gap> ComposeElements( e2, e1 ) = e1 * e2;
true

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
