#! @BeginChunk Example_EmptyMatrix
#! @BeginExample
M := MakeEmptyMatrix( 0, 3, Rationals );
#! <empty 0x3 matrix over Rationals>
M * [ 5, 8, 2 ];
#! [  ]
[] * M;
#! [ 0, 0, 0 ]
N := MakeEmptyMatrix( 3, 0, Rationals );
#! <empty 3x0 matrix over Rationals>
N * [];
#! [ 0, 0, 0 ]
[ 4, 3, 7 ] * N;
#! [  ]
M * N;
#! <empty 0x0 matrix over Rationals>
N * M;
#! [ [ 0, 0, 0 ], [ 0, 0, 0 ], [ 0, 0, 0 ] ]
P := MakeZeroMatrix( 2, 3, Rationals );
#! [ [ 0, 0, 0 ], [ 0, 0, 0 ] ]
P * N;
#! <empty 2x0 matrix over Rationals>
N + N;
#! <empty 3x0 matrix over Rationals>
N + N = N;
#! true
#! @EndExample
#! @EndChunk
