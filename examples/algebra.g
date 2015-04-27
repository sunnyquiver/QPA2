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

#! @BeginChunk Example_PathAlgebraElement
#! @BeginExample
kQ := PathAlgebra( Rationals, LeftQuiver( "Q(3)[a:1->2,b:2->3,c:2->3]" ) );
#! Rationals * Q
PathAlgebraElement( kQ, [ 2, -1, 1/5 ], [ Q.a, Q.ba, Q[1] ] );
#! -1*(b*a) + 2*(a:1->2) + 1/5*(1)
PathAlgebraElement( kQ, [ 2, 3/2 ], [ Q.a, Q.a ] );
#! 7/2*(a:1->2)
#! @EndExample
#! @EndChunk

#! @BeginChunk Example_QuotientOfPathAlgebraElement
#! @BeginExample
kQ := PathAlgebra( Rationals, LeftQuiver( "Q(3)[a:1->2,b:2->3,c:2->3]" ) );
#! Rationals * Q
A := kQ / [ kQ.ca - kQ.ba ];
#! ( Rationals * Q ) / [ 1*(c*a) + -1*(b*a) ]
e := kQ.ca + kQ.ba;
#! 1*(c*a) + 1*(b*a)
QuotientOfPathAlgebraElement( A, e );
#! { 2*(b*a) }
#! @EndExample
#! @EndChunk

#! @BeginChunk Example_PathAsAlgebraElement
#! @BeginExample
Q := LeftQuiver( "Q(3)[a:1->2,b:2->3,c:2->3]" );;
kQ := PathAlgebra( Rationals, Q );;
A := kQ / [ kQ.ca - kQ.ba ];;
PathAsAlgebraElement( kQ, Q.ca );
#! 1*(c*a)
PathAsAlgebraElement( A, Q.ca );
#! { 1*(b*a) }
#! @EndExample
#! @EndChunk

#! @BeginChunk Example_AlgebraElementByLabel
#! @BeginExample
Q := LeftQuiver( "Q(3)[a:1->2,b:2->3,c:2->3]" );;
kQ := PathAlgebra( Rationals, Q );;
kQ[ 1 ] + kQ[ 2 ] + kQ[ 3 ];
#! 1*(3) + 1*(2) + 1*(1)
kQ[ 'b' ] * kQ[ 'a' ];
#! 1*(b*a)
#! @EndExample
#! @EndChunk

#! @BeginChunk Example_AlgebraElementFromString
#! @BeginExample
Q := LeftQuiver( "Q(3)[a:1->2,b:2->3,c:3->2,d:2->1]" );;
kQ := PathAlgebra( Rationals, Q );;
AlgebraElementFromString( kQ, "dcba" );
#! 1*(d*c*b*a)
kQ.dcba
#! 1*(d*c*b*a)
#! @EndExample
#! @EndChunk

#! @BeginChunk Example_DivideByList
#! @BeginExample
Q := LeftQuiver( "Q(6)[a:1->2,b:2->3,c:2->4,d:3->5,e:4->5,f:5->6]" );;
kQ := PathAlgebra( Rationals, Q );;
divisors := [ kQ.ec - kQ.db, kQ.fdb ];;
DivideByList( kQ.fdba, divisors );
#! [ [ [  ], [ [ 1*(6), 1*(a:1->2) ] ] ], 0 ]
DivideByList( kQ.feca + kQ.eca, divisors );
#! [ [ [ [ 1*(f:5->6), 1*(a:1->2) ], [ 1*(5), 1*(a:1->2) ] ], [ [ 1*(6), 1*(a:1->2) ] ] ], 1*(d*b*a) ]
#! @EndExample
#! @EndChunk

