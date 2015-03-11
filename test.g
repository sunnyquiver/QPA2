Read( "lib/quiver.gd" );
Read( "lib/algebra.gd" );

Read( "lib/quiver.gi" );
Read( "lib/algebra.gi" );


# Q:=Quiver("Q", 1, [['z',1,1],['y',1,1],['x',1,1]]);
# A:=PathAlgebra(Rationals,Q);
# e:=A.zxxyx;
# f1:=A.xy-A.x;
# f2 := A.xx-A.xz;

Q := Quiver( "Q", 6,
             [ [ 'a', 1, 4 ],
               [ 'b', 4, 5 ],
               [ 'c', 1, 2 ],
               [ 'd', 2, 5 ],
               [ 'e', 2, 3 ],
               [ 'f', 3, 6 ],
               [ 'g', 5, 6 ] ] );
A := PathAlgebra( Rationals, Q );
rels := [ A.dc - A.ba, A.gd - A.fe ];
