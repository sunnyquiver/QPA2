#! @Chapter Quivers

#! @Section Categories for paths and quivers

#! @BeginGroup IsPath
#!
DeclareCategory( "IsPath", IsMultiplicativeElement );
#!
DeclareCategory( "IsLeftPath", IsPath );
#!
DeclareCategory( "IsRightPath", IsPath );
#! @EndGroup

#! @Description
#!  A **primitive path** is a path which can not be decomposed
#!  in a nontrivial way, that is, a vertex or an arrow.
DeclareCategory( "IsPrimitivePath", IsPath );  # vertex or arrow

#! @Description
#!  A **nontrivial** path is a path which has length at least one,
#!  that is, any path which is not a vertex.
DeclareCategory( "IsNontrivialPath", IsPath ); # not vertex

#! @Description
#!  A **composite path** is a path which can be decomposed in a
#!  nontrivial way,
#!  that is, a path which is a product of two or more arrows.
#!  Any path is either a primitive path or a composite path.
DeclareCategory( "IsCompositePath", IsNontrivialPath );

#!
DeclareCategory( "IsVertex", IsPrimitivePath );

#!
DeclareCategory( "IsArrow", IsNontrivialPath and IsPrimitivePath );

#! @BeginGroup IsQuiver
#!
DeclareCategory( "IsQuiver", CategoryCollections( IsPath ) );
#!
DeclareCategory( "IsLeftQuiver", IsQuiver );
#!
DeclareCategory( "IsRightQuiver", IsQuiver );
#! @EndGroup


#! @Section Operations for paths

#! @Arguments p
#! @Returns IsQuiver
#! @Description
#!   The quiver containing the path <A>p</A>.
DeclareAttribute( "QuiverOfPath", IsPath );

#! @BeginGroup PathEnds
#! @Arguments p
DeclareAttribute( "Source", IsPath );
#! @Arguments p
DeclareAttribute( "Target", IsPath );
#! @Arguments p
DeclareAttribute( "LeftEnd", IsPath );
#! @Arguments p
DeclareAttribute( "RightEnd", IsPath );
#! @EndGroup

#! @Arguments p
#! @Description
#!  The length of the path <A>p</A>.
#! @Returns integer
DeclareAttribute( "Length", IsPath );

#! @BeginGroup ArrowList
#! @Arguments p
#! @Description
#!  The path <A>p</A> decomposed as a list of arrows.
#! @Returns list of arrows
DeclareAttribute( "ArrowList", IsPath );
#! @Arguments p
DeclareAttribute( "ArrowListLR", IsPath );
#! @EndGroup

#! @BeginGroup DecomposePath
#! @Arguments p
#! @Description
#!  The path <A>p</A> decomposed as a list of primitive paths.
#! @Returns list of primitive paths
DeclareAttribute( "AsList", IsPath );
DeclareAttribute( "AsListLR", IsPath );
#! @EndGroup
#! @InsertChunk Example_Decompose

#! @Arguments a
#! @Description
#!  The label of the arrow <A>a</A>.
#!  This is normally a character (such as 'a' or 'b')
#!  or a string (such as "alpha"),
#!  but can be any object.
DeclareAttribute( "Label", IsArrow );

#! @Arguments v
#! @Description
#!  The number of the vertex <A>v</A>.
#!  In a quiver with $n$ vertices, the vertices are assigned
#!  numbers $1, \ldots, n$ when the quiver is constructed.
DeclareAttribute( "VertexNumber", IsVertex );

#! @Arguments a
#! @Description
#!  The number of the arrow <A>a</A>.
#!  In a quiver with $m$ arrows, the arrows are assigned
#!  numbers $1, \ldots, m$ when the quiver is constructed.
DeclareAttribute( "ArrowNumber", IsArrow );

#! @BeginGroup Composable
#! @Arguments p1, p2
#! @Returns <C>true</C> or <C>false</C>
#! @Description
#!  Checks if the paths <A>p1</A> and <A>p2</A> can be composed.
#!  If <C>Composable( p1, p2 )</C> returns <C>true</C>,
#!  then the operation <C>ComposePaths( p1, p2 )</C> succeeds.
#!  If <C>ComposableLR( p1, p2 )</C> returns <C>true</C>,
#!  then the operation <C>p1 * p2</C> succeeds.
DeclareOperation( "Composable", [ IsPath, IsPath ] );
DeclareOperation( "ComposableLR", [ IsPath, IsPath ] );
#! @EndGroup

#! @BeginGroup ComposePaths
#! @Arguments p_1, p_2, ..., p_n
DeclareGlobalFunction( "ComposePaths" );
#! @Arguments p1, p2
DeclareOperation( "ComposePaths2", [ IsPath, IsPath ] );
#! @Returns <C>IsPath</C> or <C>fail</C>
#! @Description
#!  Compose the paths <A>p_1</A>, <A>p_2</A>, ..., <A>p_n</A>, if possible.
#! @EndGroup

DeclareOperation( "PathFromArrowList", [ IsList ] );
DeclareOperation( "PathFromArrowListLR", [ IsList ] );
DeclareOperation( "PathFromArrowListNC", [ IsList ] );
DeclareOperation( "Subpath", [ IsPath, IsInt, IsInt ] );
DeclareOperation( "SubpathLR", [ IsPath, IsInt, IsInt ] );
DeclareOperation( "SubpathIndex", [ IsPath, IsPath ] );
DeclareOperation( "SubpathIndexLR", [ IsPath, IsPath ] );
DeclareOperation( "ExtractSubpath", [ IsPath, IsPath ] );
DeclareOperation( "\/", [ IsPath, IsPath ] );
DeclareOperation( "PathOverlaps", [ IsPath, IsPath ] );

DeclareOperation( "Quiver", [ IsFunction, IsString, IsPosInt, IsList ] );
DeclareOperation( "LeftQuiver", [ IsString, IsPosInt, IsList ] );
DeclareOperation( "RightQuiver", [ IsString, IsPosInt, IsList ] );
DeclareAttribute( "Vertices", IsQuiver );
DeclareAttribute( "Arrows", IsQuiver );
DeclareAttribute( "NumberOfVertices", IsQuiver );
DeclareAttribute( "NumberOfArrows", IsQuiver );
DeclareAttribute( "PrimitivePaths", IsQuiver );
DeclareOperation( "Vertex", [ IsQuiver, IsInt ] );
DeclareOperation( "\[\]", [ IsQuiver, IsInt ] );
DeclareOperation( "Arrow", [ IsQuiver, IsObject ] );
DeclareOperation( "\^", [ IsQuiver, IsObject ] );
DeclareOperation( "PathFromString", [ IsQuiver, IsString ] );
#DeclareOperation( "\.", [ IsQuiver, IsPosInt ] );
