gap> START_TEST( "quiver" );

gap> Q1 := LeftQuiver( "Q(3)[a:1->2,b:2->3]" );;
gap> Q2 := LeftQuiver( "Q(5)[a:1->2,b:2->3,c:3->4,d:4->5]" );;
gap> m := QuiverHomomorphism( Q1, Q2, [ Q2[ 3 ], Q2[ 4 ], Q2[ 5 ] ], [ Q2.c, Q2.d ] );;
gap> Q1.ba ^ m = Q2.dc;
true

gap> STOP_TEST( "quiver" );
