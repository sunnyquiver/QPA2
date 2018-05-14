#! @BeginChunk Example_DynkinQuiver
#! @BeginExample
LeftDynkinQuiver( "A3" );
#! A3(3)[a:1->2,b:2->3]
LeftDynkinQuiver( "D4" ); 
#! D4(4)[a:1->3,b:2->3,c:3->4]
LeftDynkinQuiver( "E6" );
#! E6(6)[a:1->2,b:2->3,c:3->4,d:4->5,e:3->6]
LeftDynkinQuiver( "A~3" );
#! A~3(4)[a:1->2,b:2->3,c:3->4,d:4->1]
#! @EndExample
#! @EndChunk

#! @BeginChunk Example_DynkinQuiver_orientation
#! @BeginExample
LeftDynkinQuiver( "A5>><<" );
#! A5(5)[a:1->2,b:2->3,c:4->3,d:5->4]
#! @EndExample
#! @EndChunk

#! @BeginChunk Example_DynkinQuiver_separate_args
#! @BeginExample
LeftDynkinQuiver( "A", 5 );
#! A5(5)[a:1->2,b:2->3,c:3->4,d:4->5]
LeftDynkinQuiver( "A", 5, ">><<" );
#! A5(5)[a:1->2,b:2->3,c:4->3,d:5->4]
#! @EndExample
#! @EndChunk

#! @BeginChunk Example_DynkinGraph
#! @BeginExample
DynkinGraph( "A", 4 );
#! [ [ 1, 2 ], [ 2, 3 ], [ 3, 4 ] ]
#! @EndExample
#! @EndChunk
