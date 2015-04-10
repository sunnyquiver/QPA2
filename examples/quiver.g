#! @BeginChunk Example_Decompose
#! @BeginExample
Q := LeftQuiver( "Q", 3, [ [ 'a', 1, 2 ], [ 'b', 2, 3 ] ] );
#! Quiver( Q, 3, [ [ 'a', 1, 2 ], [ 'b', 2, 3 ] ] )
AsList( Q.ba );
#! [ (a), (b) ]
AsListLR( Q.ba );
#! [ (b), (a) ]
ArrowList( Q.ba );
#! [ (a), (b) ]
ArrowListLR( Q.ba );
#! [ (b), (a) ]
AsList( Q[ 1 ] );
#! [ (1) ]
ArrowList( Q[ 1 ] );
#! [  ]
#! @EndExample
#! @EndChunk
