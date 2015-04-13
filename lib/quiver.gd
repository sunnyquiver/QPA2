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
#! @Returns <C>IsPath</C> or <C>fail</C>
#! @Description
#!  Compose the paths <A>p_1</A>, <A>p_2</A>, ..., <A>p_n</A>, if possible.
DeclareGlobalFunction( "ComposePaths" );
#! @Arguments p1, p2
DeclareOperation( "ComposePaths2", [ IsPath, IsPath ] );
#! @EndGroup

#! @InsertChunk PathMultiplication

#! @BeginGroup PathFromArrowList
#! @Arguments list
#! @Description
#!  Combine the arrows in the list <A>list</A> to a path.
#!  This operation is used internally.
#!  For users of QPA, it is usually more convenient to use the
#!  more general function <C>ComposePaths</C>.
DeclareOperation( "PathFromArrowList", [ IsList ] );
#! @Arguments list
#!  The variant <C>PathFromArrowListLR</C> assumes the arrows
#!  to be given in the multiplication order of the quiver.
DeclareOperation( "PathFromArrowListLR", [ IsList ] );
#! @Arguments list
#!  The variant <C>PathFromArrowListNC</C> does not check that
#!  the arrows are composable.
DeclareOperation( "PathFromArrowListNC", [ IsList ] );
#! @EndGroup

#! @BeginGroup Subpath
#! @Description
#!  Extract a subpath of the path <A>p</A>.
#!  The integers <A>from</A> and <A>to</A> identify vertices
#!  that the path passes through, counted from 0 to <C>Length( <A>p</A> )</C>.
#!  The operation <C>Subpath</C> counts the vertices from the source
#!  of the path to the target; the operation <C>SubpathLR</C> counts
#!  the vertices from left to right in multiplication order.
#!  The resulting subpath is the part of <A>p</A> between vertices
#!  <A>from</A> and <A>to</A>.
#!  This is a path of length <C><A>to</A> - <A>from</A></C>.
#!  In particular, if <C><A>from</A> = <A>to</A></C>, then the result
#!  is a vertex.
#! @Returns IsPath
#! @Arguments p, from to
DeclareOperation( "Subpath", [ IsPath, IsInt, IsInt ] );
#! @Arguments p, from to
DeclareOperation( "SubpathLR", [ IsPath, IsInt, IsInt ] );
#! @EndGroup

#! @InsertChunk Example_Subpath

#! @BeginGroup SubpathIndex
#! @Description
#!  Finds the first position (if any) in the path <A>p</A>
#!  where <A>q</A> appears as a subpath.
#!
#!  If <A>q</A> is not a subpath of <A>p</A>, then <C>fail</C> is returned.
#!  If <A>q</A> is a subpath of <A>p</A>, then
#!  <C>SubpathIndex( <A>p</A>, <A>q</A> )</C> returns an integer <C>i</C> such that
#!  <C>Subpath( <A>p</A>, i, i + Length( <A>q</A> ) ) = <A>q</A></C>, and
#!  <C>SubpathIndexLR( <A>p</A>, <A>q</A> )</C> returns an integer <C>j</C> such that
#!  <C>SubpathLR( <A>p</A>, j, j + Length( <A>q</A> ) ) = <A>q</A></C>.
#!
#!  Both <C>SubpathIndex</C> and <C>SubpathIndexLR</C> search through the path <A>p</A>
#!  from source to target for the first occurence of <A>q</A>.
#!  This means that in a left-oriented quiver, the rightmost occurence is found,
#!  while in a right-oriented quiver, the leftmost occurence is found.
#! @Arguments p, q
#! @Returns integer or <C>fail</C>
DeclareOperation( "SubpathIndex", [ IsPath, IsPath ] );
DeclareOperation( "SubpathIndexLR", [ IsPath, IsPath ] );
#! @EndGroup

#! @InsertChunk Example_SubpathIndex

#! @Description
#!  Finds the paths that remain if we remove the subpath <A>q</A> from the path <A>p</A>.
#!
#!  If <A>q</A> is not a subpath of <A>p</A>, then <C>fail</C> is returned.
#!  If <A>q</A> is a subpath of <A>p</A>, then the result is a list
#!  <C>[ r1, r2 ]</C> of two paths such that
#!  <C>ComposePaths( r1, <A>q</A>, r2 ) = <A>p</A></C>.
#!
#!  If <A>q</A> occurs more than once as a subpath of <A>p</A>, then
#!  the first occurence (in source-to-target order) is used; that is, the
#!  same occurence that would be found by <C>SubpathIndex( <A>p</A>, <A>q</A> )</C>.
#! @Arguments p, q
#! @Returns list containing two paths or <C>fail</C>
DeclareOperation( "ExtractSubpath", [ IsPath, IsPath ] );

#! @InsertChunk Example_ExtractSubpath

#! @Description
#!  Divide the path <A>p</A> by the path <A>q</A>, if possible.
#!
#!  If <A>q</A> is not a subpath of <A>p</A>, then <C>fail</C> is returned.
#!  If <A>q</A> is a subpath of <A>p</A>, then the result is a list
#!  <C>[ r1, r2 ]</C> of two paths such that
#!  <C>r1 * <A>q</A> * r2 = <A>p</A></C>.
#!
#!  In a right-oriented quiver, this operation is exactly the same
#!  as <C>ExtractSubpath</C>.
#!  In a left-oriented quiver, this operation is the same
#!  as <C>ExtractSubpath</C> except that the paths <C>r1</C> and <C>r2</C>
#!  in the result occur in the opposite order.
#! @Arguments p, q
#! @Returns list containing two paths or <C>fail</C>
DeclareOperation( "\/", [ IsPath, IsPath ] );

#! @InsertChunk Example_DividePaths

#! @Description
#!  Finds overlaps between the paths <A>p</A> and <A>q</A>.
#!
#!  Returns a list of all pairs <C>[ b, c ]</C> of paths such that
#!  <C><A>p</A> * c = b * <A>q</A></C>.
#! @Arguments p, q
#! @Returns list of pairs of paths
DeclareOperation( "PathOverlaps", [ IsPath, IsPath ] );

#! @InsertChunk Example_PathOverlaps

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
