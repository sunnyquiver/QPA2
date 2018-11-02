gap> START_TEST( "quiver" );

gap> Q1 := LeftQuiver( "Q(3)[a:1->2,b:2->3]" );;
gap> Q2 := LeftQuiver( "Q(5)[a:1->2,b:2->3,c:3->4,d:4->5]" );;
gap> m := QuiverHomomorphism( Q1, Q2, [ Q2[ 3 ], Q2[ 4 ], Q2[ 5 ] ], [ Q2.c, Q2.d ] );;
gap> Q1.ba ^ m = Q2.dc;
true

gap> Q1 := LeftQuiver( "Q(3)[a:1->2,b:2->3]" );;
gap> Q2 := LeftQuiver( "Q(5)[a:1->2,b:2->3,c:3->4,d:4->5]" );;
gap> PQ := QuiverProduct( Q1, Q2 );;
gap> incs := ProductQuiverInclusions( PQ );;
gap> Length( incs );
2
gap> Length( incs[ 1 ] ) = NumberOfVertices( Q2 );
true
gap> Length( incs[ 2 ] ) = NumberOfVertices( Q1 );
true
gap> Q1.ba ^ incs[ 1 ][ 2 ] = PathInProductQuiver( PQ, [ Q1.ba, Q2[2] ] );
true

gap> STOP_TEST( "quiver" );
