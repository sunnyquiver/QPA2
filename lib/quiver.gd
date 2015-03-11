DeclareCategory( "IsPath", IsMultiplicativeElement );
#DeclareCategory( "IsPath", IsPathExpr );
DeclareCategory( "IsPrimitivePath", IsPath );  # vertex or arrow
DeclareCategory( "IsNontrivialPath", IsPath ); # not vertex
DeclareCategory( "IsVertex", IsPrimitivePath );
DeclareCategory( "IsArrow", IsNontrivialPath and IsPrimitivePath );
DeclareCategory( "IsCompositePath", IsNontrivialPath );
DeclareCategory( "IsQuiver", CategoryCollections( IsPath ) );

DeclareAttribute( "ContainingQuiver", IsPath );
DeclareAttribute( "Source", IsPath );
DeclareAttribute( "Target", IsPath );
DeclareAttribute( "Length", IsPath );
DeclareAttribute( "ArrowList", IsPath );
DeclareAttribute( "AsList", IsPath );
DeclareAttribute( "Label", IsArrow );
DeclareAttribute( "VertexNumber", IsVertex );
DeclareAttribute( "ArrowNumber", IsArrow );
DeclareOperation( "Composable", [ IsPath, IsPath ] );
#DeclareOperation( "\*", [ IsPath, IsPath ] );
DeclareOperation( "PathFromArrowList", [ IsList ] );
DeclareOperation( "Subpath", [ IsPath, IsInt, IsInt ] );

DeclareGlobalFunction( "Quiver" );
DeclareAttribute( "Vertices", IsQuiver );
DeclareAttribute( "Arrows", IsQuiver );
DeclareOperation( "Vertex", [ IsQuiver, IsInt ] );
DeclareOperation( "\[\]", [ IsQuiver, IsInt ] );
DeclareOperation( "Arrow", [ IsQuiver, IsObject ] );
DeclareOperation( "\^", [ IsQuiver, IsObject ] );
DeclareOperation( "PathFromString", [ IsQuiver, IsString ] );
#DeclareOperation( "\.", [ IsQuiver, IsPosInt ] );

DeclareOperation( "\/", [ IsPath, IsPath ] );
DeclareOperation( "PathOverlaps", [ IsPath, IsPath ] );
