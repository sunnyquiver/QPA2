DeclareCategory( "IsQPAMatrix", IsMatrixObj );

# matobj2.gd:367:DeclareAttribute( "BaseDomain", IsMatrixObj );
# matobj2.gd:379:DeclareAttribute( "DimensionsMat", IsMatrixObj );   # returns [rows,cols]
# matobj2.gd:475:DeclareOperation( "MatElm", [IsMatrixObj,IsPosInt,IsPosInt] );

DeclareOperation( "MatrixByRows", [ IsRing, IsMatrix ] );
DeclareOperation( "MatrixByCols", [ IsRing, IsMatrix ] );
DeclareOperation( "MakeZeroMatrix", [ IsRing, IsInt, IsInt ] );
DeclareOperation( "IdentityMatrix", [ IsRing, IsInt ] );

DeclareProperty( "IsIdentityMatrix", IsQPAMatrix );
DeclareProperty( "IsZeroMatrix", IsQPAMatrix );

DeclareOperation( "RowsOfMatrix", [ IsQPAMatrix ] );
DeclareOperation( "ColsOfMatrix", [ IsQPAMatrix ] );
