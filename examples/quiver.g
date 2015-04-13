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

#! @BeginChunk Example_Subpath
#! @BeginExample
Q := LeftQuiver( "Q", 7, [ [ 'a', 1, 2 ], [ 'b', 2, 3 ], [ 'c', 3, 4 ],
                              [ 'd', 4, 5 ], [ 'e', 5, 6 ], [ 'f', 6, 7 ] ] );;
Subpath( Q.fedcba, 0, 4 );
#! (d*c*b*a)
Subpath( Q.fedcba, 1, 3 );
#! (c*b)
SubpathLR( Q.fedcba, 1, 3 );
#! (e*d)
#! @EndExample
#! @EndChunk

#! @BeginChunk Example_SubpathIndex
#! @BeginExample
Q := LeftQuiver( "Q", 3, [ [ 'a', 1, 2 ], [ 'b', 2, 3 ], [ 'c', 3, 1 ] ] );;
SubpathIndex( Q.acbacba, Q.ac );
#! 2
SubpathIndexLR( Q.acbacba, Q.ac );
#! 3
#! @EndExample
#! @EndChunk

#! @BeginChunk Example_ExtractSubpath
#! @BeginExample
Q := LeftQuiver( "Q", 3, [ [ 'a', 1, 2 ], [ 'b', 2, 3 ], [ 'c', 3, 1 ] ] );;
ExtractSubpath( Q.acbacba, Q.ac );
#! [ (b*a), (a*c*b) ]
#! @EndExample
#! @EndChunk

#! @BeginChunk Example_DividePaths
#! @BeginExample
Q := LeftQuiver( "Q", 3, [ [ 'a', 1, 2 ], [ 'b', 2, 3 ], [ 'c', 3, 1 ] ] );;
Q.acbacba / Q.ac;
#! [ (a*c*b), (b*a) ]
#! @EndExample
#! @EndChunk

#! @BeginChunk Example_PathOverlaps
#! @BeginExample
Q := LeftQuiver( "Q", 3, [ [ 'a', 1, 2 ], [ 'b', 2, 3 ], [ 'c', 3, 1 ] ] );;
PathOverlaps( Q.acbacba, Q.bacbac );
#! [ [ (a*c*b*a*c), (c*b*a*c) ], [ (a*c), (c) ] ]
#! @EndExample
#! @EndChunk
