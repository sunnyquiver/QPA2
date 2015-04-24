#! @BeginChunk Example_PathAlgebra
#! @BeginExample
Q := LeftQuiver( "Q(3)[a:1->2,b:2->3,c:2->3]" );
#! Q(3)[a:1->2,b:2->3,c:2->3]
kQ := PathAlgebra( Rationals, Q );
#! Rationals * Q
kQ.c - 2/5 * kQ.ba + kQ[1];
#! -2/5*(b*a) + 1*(c:2->3) + 1*(1)
#! @EndExample
#! @EndChunk

#! @BeginChunk Example_QuotientOfPathAlgebra
#! @BeginExample
kQ := PathAlgebra( Rationals, LeftQuiver( "Q(3)[a:1->2,b:2->3,c:2->3]" ) );
#! Rationals * Q
A := kQ / [ kQ.ca - kQ.ba ];
#! ( Rationals * Q ) / [ 1*(c*a) + -1*(b*a) ]
(A.c + A.b) * A.a;
#! { 2*(b*a) }
kQ / TwoSidedIdealByGenerators( kQ, [ kQ.ca - kQ.ba ] );
#! ( Rationals * Q ) / [ 1*(c*a) + -1*(b*a) ]
#! @EndExample
#! @EndChunk
