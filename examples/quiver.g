#! @BeginChunk Example_LeftQuiver
#! @BeginExample
LeftQuiver( "Q", 2, [ [ 'a', 1, 2 ], [ 'b', 2, 1 ] ]);
#! Q(2)[a:1->2,b:2->1]
LeftQuiver( "Q[a]", 2, [ [ 1, 2 ], [ 2, 1 ] ]);
#! Q(2)[a:1->2,b:2->1]
LeftQuiver( "R[a]", ['u','v'], [ [ 'u', 'v' ], [ 'v', 'u' ] ]);
#! R(u,v)[a:u->v,b:v->u]
LeftQuiver( "R(u)[a]", 2, [ [ 'u', 'v' ], [ 'v', 'u' ] ]);
#! R(u,v)[a:u->v,b:v->u]
LeftQuiver( "Q(u1)[alpha_1]", 3, [ [ 2, 1 ], [ 2, 3 ] ] );
#! Q(u1,u2,u3)[alpha_1:u2->u1,alpha_2:u2->u3]
LeftQuiver( "Q(u1)[alpha_1]", 3, [ [ "u2", "u1" ], [ "u2", "u3" ] ] );
#! Q(u1,u2,u3)[alpha_1:u2->u1,alpha_2:u2->u3]
LeftQuiver( "Q(0)[a]", 2, [ [ 0, 0 ], [ 0, 1 ] ] );
#! Q(0,1)[a:0->0,b:0->1]
LeftQuiver( "A3", 3, [ [ 'a', 1, 2 ], [ 'b', 2, 3 ] ] );
#! A3(3)[a:1->2,b:2->3]
LeftQuiver( "A3(3)[a:1->2,b:2->3]" );
#! A3(3)[a:1->2,b:2->3]
LeftQuiver( "Q(u0..u3)[a:u0->u3,b:u1->u2,c:u2->u3]" );
#! Q(u0,u1,u2,u3)[a:u0->u3,b:u1->u2,c:u2->u3]
#! @EndExample
#! @EndChunk

#! @BeginChunk Example_RightQuiver
#! @BeginExample
RQ := RightQuiver( "Q(3)[a:1->2,b:2->3]" );
#! Q(3)[a:1->2,b:2->3]
RQ.a * RQ.b;
#! (a*b)
RQ.b * RQ.a;
#! fail
LQ := LeftQuiver( "Q(3)[a:1->2,b:2->3]" );
#! Q(3)[a:1->2,b:2->3]
LQ.b * LQ.a;
#! (b*a)
LQ.a * LQ.b;
#! fail
#! @EndExample
#! @EndChunk

#! @BeginChunk Example_Quiver
#! @BeginExample
LQ := Quiver( IsLeftQuiver, "Q(2)[a:1->2,b:1->2]" );
#! Q(2)[a:1->2,b:1->2]
RQ := Quiver( IsRightQuiver, "Q(2)[a:1->2,b:1->2]" );
#! Q(2)[a:1->2,b:1->2]
IsLeftQuiver( LQ );
#! true
IsRightQuiver( RQ );
#! true
#! @EndExample
#! @EndChunk

#! @BeginChunk Example_MakeLabelsFromPattern
#! @BeginExample
MakeLabelsFromPattern( "a", 5 );
#! "abcde"
MakeLabelsFromPattern( "F", 5 );
#! "FGHIJ"
MakeLabelsFromPattern( "0", 5 );
#! [ 0, 1, 2, 3, 4 ]
MakeLabelsFromPattern( "-2", 5 );
#! [ -2, -1, 0, 1, 2 ]
MakeLabelsFromPattern( "foo38", 5 );
#! [ "foo38", "foo39", "foo40", "foo41", "foo42" ]
MakeLabelsFromPattern( "spam3", 5, [,,"egg"] );
#! [ "spam3", "spam4", "egg", "spam6", "spam7" ]
LeftQuiver( "Q", MakeLabelsFromPattern( "u", 3 ),
               MakeLabelsFromPattern( "a", 4 ),
               [ 1, 1, 2, 3 ], [ 1, 2, 3, 2 ] );
Q(u,v,w)[a:u->u,b:u->v,c:v->w,d:w->v]
#! @EndExample
#! @EndChunk

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
