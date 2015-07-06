LoadPackage( "QPA" );

# Q:=Quiver("Q", 1, [['z',1,1],['y',1,1],['x',1,1]]);
# A:=PathAlgebra(Rationals,Q);
# e:=A.zxxyx;
# f1:=A.xy-A.x;
# f2 := A.xx-A.xz;

Q1 := LeftQuiver( "Q1(3)[a:1->2,b:2->3]" );
Q1_ := LeftQuiver( "Q1[a]", 3, [[1,2], [2,3]] );
Q2 := LeftQuiver( "Q2", 1, [['a', 1, 1]] );
Q3 := LeftQuiver( "Q3", 3,
                  [ [ 'a', 1, 2 ],
                    [ 'b', 2, 3 ],
                    [ 'c', 3, 1 ],
                    [ 'd', 1, 1 ] ] );
kQ1 := PathAlgebra( Rationals, Q1 );
kQ2 := PathAlgebra( Rationals, Q2 );
kQ3 := PathAlgebra( Rationals, Q3 );

Q4 := LeftQuiver( "Q4", 7,
                  [ ['a', 2, 1],
                    ['b', 7, 1],
                    ['c', 4, 2],
                    ['d', 5, 4],
                    ['e', 3, 2],
                    ['f', 5, 3],
                    ['g', 6, 5],
                    ['h', 6, 7] ] );
kQ4 := PathAlgebra( Rationals, Q4 );
rels4 := [ kQ4.aefg - kQ4.bh,
           kQ4.ef - kQ4.cd ];
A4 := kQ4 / Ideal( kQ4, rels4 );

# Example from Benjamin Keller's PhD thesis, p. 130
Q5 := RightQuiver( "DCYC", 4,
                   [ [ 'a', 2, 3 ],
                     [ 'b', 3, 4 ],
                     [ 'c', 4, 2 ],
                     [ 'd', 2, 1 ],
                     [ 'e', 1, 3 ],
                     [ 'f', 3, 1 ],
                     [ 'g', 1, 4 ],
                     [ 'h', 4, 1 ],
                     [ 'i', 1, 2 ] ] );
kQ5 := PathAlgebra( Rationals, Q5 );
rels5 := [ kQ5.abc + kQ5.di, kQ5.id + kQ5.iaf, kQ5.f * kQ5.i + kQ5.fgc + kQ5.bhi,
           kQ5.cab + kQ5.hg, kQ5.ef + kQ5.ebh, kQ5.dg + kQ5.deb + kQ5.afg,
           kQ5.bca + kQ5.fe, kQ5.gh + kQ5.gcd, kQ5.he + kQ5.hia + kQ5.cde ];


Q6 := LeftQuiver( "Q", 6,
                 [ [ 'a', 1, 4 ],
                   [ 'b', 4, 5 ],
                   [ 'c', 1, 2 ],
                   [ 'd', 2, 5 ],
                   [ 'e', 2, 3 ],
                   [ 'f', 3, 6 ],
                   [ 'g', 5, 6 ] ] );
kQ6 := PathAlgebra( Rationals, Q6 );
rels6 := [ kQ6.dc - kQ6.ba, kQ6.gd - kQ6.fe ];


P1r := QuiverRepresentation( kQ1, [ 1, 1, 1 ],
                             [ [ [ 1 ] ],
                               [ [ 1 ] ] ] );
P2r := QuiverRepresentation( kQ1, [ 0, 1, 1 ],
                             [ [ [ 0 ] ],
                               [ [ 1 ] ] ] );
P3r := QuiverRepresentation( kQ1, [ 0, 0, 1 ],
                             [ [ [ 0 ] ],
                               [ [ 0 ] ] ] );
I1r := QuiverRepresentation( kQ1, [ 1, 0, 0 ],
                             [ [ [ 0 ] ],
                               [ [ 0 ] ] ] );
I2r := QuiverRepresentation( kQ1, [ 1, 1, 0 ],
                             [ [ [ 1 ] ],
                               [ [ 0 ] ] ] );
I3r := P1r;

f := QuiverRepresentationHomomorphism( P2r, P1r, [ [[0]], [[1]], [[1]] ] );

ZZr := ZeroRepresentation( kQ1 );

P1m := AsLeftModule( P1r );
P2m := AsLeftModule( P2r );
P3m := AsLeftModule( P3r );
I1m := AsLeftModule( I1r );
I2m := AsLeftModule( I2r );
I3m := AsLeftModule( I3r );

P1 := LeftQuiverModule( kQ1, [ 1, 1, 1 ], [ [ [ 1 ] ], [ [ 1 ] ] ] );
P2 := LeftQuiverModuleByArrows( kQ1, [ 0, 1, 1 ], [ Q1.b ], [ [ [ 1 ] ] ] );
P3 := LeftQuiverModuleByArrows( kQ1, [ 0, 0, 1 ], [ ], [ ] );
ZZ := LeftZeroModule( kQ1 );

QxQ_ := LeftQuiver( "QxQ",
                    [ [1,1],[1,2],[2,1],[2,2] ],
                    [
                      [[1,'a'],[1,1],[1,2]],
                      [[2,'a'],[2,1],[2,2]],
                      [['a',1],[1,1],[2,1]],
                      [['a',2],[1,2],[2,2]]
                     ] );

QxQ := LeftQuiver( ["Q","Q"],
                   VertexLabels( QxQ_ ),
                   ArrowLabels( QxQ_ ),
                   ArrowSourceIndices( QxQ_ ),
                   ArrowTargetIndices( QxQ_ ) );

IsProductQuiver( QxQ );
Q := ProductQuiverFactor( QxQ, 1 );
kQ := PathAlgebra( Rationals, Q );

# IsProductQuiver( QxQ ) -> true
