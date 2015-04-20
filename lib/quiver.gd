#! @Chapter Quivers

#! @Section Categories for paths and quivers

#! @BeginGroup IsPath
#! @Description
#!  Every path is in the category <C>IsPath</C>.
#!  Additionally, every path in a left-oriented quiver is in
#!  the category <C>IsLeftPath</C>, and
#!  every path in a right-oriented quiver is in
#!  the category <C>IsRightPath</C>.
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
#! @Description
#!  Every quiver is in the category <C>IsQuiver</C>.
#!  Additionally, a left-oriented quiver is in the category <C>IsLeftQuiver</C>,
#!  and a right-oriented quiver is in the category <C>IsRightQuiver</C>.
DeclareCategory( "IsQuiver", CategoryCollections( IsPath ) );
#!
DeclareCategory( "IsLeftQuiver", IsQuiver );
#!
DeclareCategory( "IsRightQuiver", IsQuiver );
#! @EndGroup


#! @Section Constructing quivers

#! @BeginGroup LeftQuiver
#! @Description
#!  Constructor for left-oriented quivers.
#! @Arguments label_with_patterns, num_vertices, arrows
DeclareOperation( "LeftQuiver", [ IsString, IsPosInt, IsDenseList ] );
#! @Arguments label_with_patterns, vertex_labels, arrows
DeclareOperation( "LeftQuiver", [ IsString, IsDenseList, IsDenseList ] );
#! @Arguments description
DeclareOperation( "LeftQuiver", [ IsString ] );
#! @Arguments label_with_patterns_list, num_vertices, vertex_labels, arrows
DeclareOperation( "LeftQuiver", [ IsDenseList, IsPosInt, IsList, IsDenseList ] );
#! @Arguments label, vertex_labels, arrow_labels, source_indices, target_indices
DeclareOperation( "LeftQuiver", [ IsString, IsDenseList, IsDenseList, IsDenseList, IsDenseList ] );
#! @EndGroup

#! @BeginGroup RightQuiver
#! @Description
#!  Constructor for right-oriented quivers.
#! @Arguments label_with_patterns, num_vertices, arrows
DeclareOperation( "RightQuiver", [ IsString, IsPosInt, IsDenseList ] );
#! @Arguments label_with_patterns, vertex_labels, arrows
DeclareOperation( "RightQuiver", [ IsString, IsDenseList, IsDenseList ] );
#! @Arguments description
DeclareOperation( "RightQuiver", [ IsString ] );
#! @Arguments label_with_patterns_list, num_vertices, vertex_labels, arrows
DeclareOperation( "RightQuiver", [ IsDenseList, IsPosInt, IsList, IsDenseList ] );
#! @Arguments label, vertex_labels, arrow_labels, source_indices, target_indices
DeclareOperation( "RightQuiver", [ IsString, IsDenseList, IsDenseList, IsDenseList, IsDenseList ] );
#! @EndGroup

#! @BeginGroup Quiver
#! @Description
#!  Constructor for quivers.
#!  <P/>
#!  These operations are exactly the same as <C>LeftQuiver</C>
#!  and <C>RightQuiver</C>, except that they have an additional
#!  first argument <A>quiver_cat</A>, which specifies whether the
#!  quiver should be left- or right-oriented.
#!  The value of this argument should be either <C>IsLeftQuiver</C>
#!  or <C>IsRightQuiver</C>.
#! @Arguments quiver_cat, label_with_patterns, num_vertices, arrows
DeclareOperation( "Quiver", [ IsFunction, IsString, IsPosInt, IsDenseList ] );
#! @Arguments quiver_cat, label_with_patterns, vertex_labels, arrows
DeclareOperation( "Quiver", [ IsFunction, IsString, IsDenseList, IsDenseList ] );
#! @Arguments quiver_cat, description
DeclareOperation( "Quiver", [ IsFunction, IsString ] );
#! @Arguments quiver_cat, label_with_patterns_list, num_vertices, vertex_labels, arrows
DeclareOperation( "Quiver", [ IsFunction, IsDenseList, IsPosInt, IsList, IsDenseList ] );
#! @Arguments quiver_cat, label, vertex_labels, arrow_labels, source_indices, target_indices
DeclareOperation( "Quiver", [ IsFunction, IsString, IsDenseList, IsDenseList, IsDenseList, IsDenseList ] );
#! @EndGroup Quiver

DeclareOperation( "DecomposeQuiverDescriptionString", [ IsString ] );
DeclareOperation( "ParseLabelPatternString", [ IsString ] );
DeclareOperation( "ApplyLabelPattern", [ IsDenseList, IsPosInt ] );
DeclareOperation( "MakeLabelsFromPatternObj", [ IsDenseList, IsPosInt, IsList ] );

#! @BeginGroup MakeLabelsFromPattern
#! @Description
#!  Make a list of <A>n</A> labels (for vertices or arrows)
#!  from the pattern <A>pattern</A>.
#!  The argument <A>fixed_labels</A> may be used to specify some
#!  labels that deviate from the pattern.
#! @Arguments pattern, n
DeclareOperation( "MakeLabelsFromPattern", [ IsString, IsPosInt ] );
#! @Arguments pattern, n, fixed_labels
DeclareOperation( "MakeLabelsFromPattern", [ IsString, IsPosInt, IsList ] );
#! @EndGroup MakeLabelsFromPattern

DeclareOperation( "ParseStringAsLabel", [ IsString ] );
DeclareOperation( "ParseQuiverLabelString", [ IsString ] );
DeclareOperation( "ParseVerticesDescriptionString", [ IsString ] );
DeclareOperation( "ParseArrowDescriptionString", [ IsString ] );
DeclareOperation( "ParseQuiverDescriptionString", [ IsString ] );

DeclareOperation( "SplitStringSubstring", [ IsString, IsString ] );


#! @Section Information about a quiver

#! @Description
#!  The label of the quiver <A>Q</A>.
#! @Arguments Q
#! @Returns <C>IsString</C>
DeclareAttribute( "Label", IsQuiver );


#! @Section Accessing paths in a quiver

#! @Description
#!  Returns the vertices of the quiver <A>Q</A> as a list.
#!  <P/>
#!  The ordering of the list corresponds to the ordering of the vertices
#!  in the quiver.
#!  That is, the vertex at position <C>i</C> in the list has number <C>i</C>
#!  (see <C>VertexNumber</C>) and is the vertex which is returned by
#!  <C>Vertex( <A>Q</A>, i )</C>.
#! @Arguments Q
#! @Returns list of <C>IsVertex</C>
DeclareAttribute( "Vertices", IsQuiver );

#! @Description
#!  Returns the arrows of the quiver <A>Q</A> as a list.
#!  <P/>
#!  The ordering of the list corresponds to the ordering of the arrows
#!  in the quiver.
#!  That is, the arrow at position <C>i</C> in the list has number <C>i</C>
#!  (see <C>ArrowNumber</C>) and is the arrow which is returned by
#!  <C>Arrow( <A>Q</A>, i )</C>.
#! @Arguments Q
#! @Returns list of <C>IsArrow</C>
DeclareAttribute( "Arrows", IsQuiver );

#! @Description
#!  The number of vertices in the quiver <A>Q</A>.
#! @Arguments Q
#! @Returns integer
DeclareAttribute( "NumberOfVertices", IsQuiver );

#! @Description
#!  The number of arrows in the quiver <A>Q</A>.
#! @Arguments Q
#! @Returns integer
DeclareAttribute( "NumberOfArrows", IsQuiver );

#! @Description
#!  Returns the sources of all arrows in the quiver,
#!  given as a list <C>L</C> of positive integers,
#!  such that <C>L[i]</C> is the index of the source vertex
#!  of the arrow with index <C>i</C>.
#! @Arguments Q
#! @Returns list of positive integers
DeclareAttribute( "ArrowSourceIndices", IsQuiver );

#! @Description
#!  Returns the targets of all arrows in the quiver,
#!  given as a list <C>L</C> of positive integers,
#!  such that <C>L[i]</C> is the index of the target vertex
#!  of the arrow with index <C>i</C>.
#! @Arguments Q
#! @Returns list of positive integers
DeclareAttribute( "ArrowTargetIndices", IsQuiver );

#! @Description
#!  Returns the primitive paths of the quiver <A>Q</A> as a list.
#!  <P/>
#!  This list contains the vertices first and then the arrows,
#!  all ordered in the usual way.  That is, we have
#!  <C>PrimitivePaths( <A>Q</A> ) =
#!     Concatenation( Vertices( <A>Q</A> ), Arrows( <A>Q</A> ) )</C>.
#! @Arguments Q
#! @Returns list of <C>IsPrimitivePath</C>
DeclareAttribute( "PrimitivePaths", IsQuiver );

#! @BeginGroup Vertex
#! @Description
#!  The vertex with number <A>i</A> in the quiver <A>Q</A>.
#! @Arguments Q, i
#! @Returns <C>IsVertex</C>
DeclareOperation( "Vertex", [ IsQuiver, IsPosInt ] );
#! @Arguments Q, i
DeclareOperation( "\[\]", [ IsQuiver, IsObject ] );
#! @EndGroup

#! @Description
#!  The arrow with number <A>i</A> in the quiver <A>Q</A>.
#! @Arguments Q, i
#! @Returns <C>IsArrow</C>
DeclareOperation( "Arrow", [ IsQuiver, IsPosInt ] );

#! @BeginGroup PrimitivePathByLabel
#! @Description
#!  Returns the primitive path (vertex or arrow) of the quiver <A>Q</A>
#!  which has <A>label</A> as label, if any.
#!  If no such path exists, then <C>fail</C> is returned.
#!  The operation <C><A>Q</A>^<A>label</A></C> is equivalent to
#!  <C>PrimitivePathByLabel( <A>Q</A>, <A>label</A> )</C>.
#! @Returns <C>IsPrimitivePath</C> or <C>fail</C>
#! @Arguments Q, label
DeclareOperation( "PrimitivePathByLabel", [ IsQuiver, IsObject ] );
#! @Arguments Q, label
DeclareOperation( "\^", [ IsQuiver, IsObject ] );
#! @EndGroup

#!
DeclareOperation( "PathFromString", [ IsQuiver, IsString ] );
#DeclareOperation( "\.", [ IsQuiver, IsPosInt ] );


#! @Section Information about a path

#! @Arguments p
#! @Returns IsQuiver
#! @Description
#!  The quiver containing the path <A>p</A>.
DeclareAttribute( "QuiverOfPath", IsPath );

#! @BeginGroup PathEnds
#! @Description
#!  The **source** of a path is the vertex where the path starts;
#!  the **target** of a path is the vertex where the path ends.
#!  The **left end** and **right end** of a path <A>p</A> are defined
#!  as the vertices with the property
#!  <C>LeftEnd( <A>p</A> ) * <A>p</A> * RightEnd( <A>p</A> ) = <A>p</A></C>.
#!  <P/>
#!  In a left-oriented quiver, the left end of a path is the target
#!  and the right end is the source.
#!  In a right-oriented quiver, the left end of a path is the source
#!  and the right end is the target.
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
#! @Returns nonnegative integer
DeclareAttribute( "Length", IsPath );

#! @Arguments p
#! @Description
#!  The label of the primitive path <A>p</A>.
#!  This is normally a character (such as 'a' or 'b')
#!  or a string (such as "alpha"),
#!  but can be any object.
DeclareAttribute( "Label", IsPrimitivePath );

#! @Arguments p
#! @Description
#!  The label of the primitive path <A>p</A>, as a string.
#! @Returns <C>IsString</C>
DeclareAttribute( "LabelAsString", IsPrimitivePath );

DeclareGlobalFunction( "QPA_LABEL_TO_STRING" );

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


#! @Section Composition of paths

#! @BeginGroup Composable
#! @Arguments p1, p2
#! @Returns <C>true</C> or <C>false</C>
#! @Description
#!  Checks if the paths <A>p1</A> and <A>p2</A> can be composed.
#!  <P/>
#!  If <C>Composable( p1, p2 )</C> returns <C>true</C>,
#!  then the operation <C>ComposePaths( p1, p2 )</C> succeeds.
#!  If <C>ComposableLR( p1, p2 )</C> returns <C>true</C>,
#!  then the operation <C>p1 * p2</C> succeeds.
DeclareOperation( "Composable", [ IsPath, IsPath ] );
DeclareOperation( "ComposableLR", [ IsPath, IsPath ] );
#! @EndGroup

#! @BeginGroup ComposePathsGroup
#! @Arguments p_1, p_2, ..., p_n
#! @Returns <C>IsPath</C> or <C>fail</C>
#! @Description
#!  Compose the paths <A>p_1</A>, <A>p_2</A>, ..., <A>p_n</A>, if possible.
#!  <P/>
#!  For the operation <C>ComposePaths</C>, the paths should be given in source-to-target order.
#!  For the operation <C>ComposePathsLR</C>, the paths should be given in multiplication order.
#!  <P/>
#!  The function <C>ComposePaths</C>, which takes an arbitrary number
#!  of arguments, is implemented by repeated calls to the
#!  two-argument operation <C>ComposePaths2</C>.
DeclareGlobalFunction( "ComposePaths" );
#! @Arguments p_1, p_2, ..., p_n
DeclareGlobalFunction( "ComposePathsLR" );
#! @Arguments p1, p2
DeclareOperation( "ComposePaths2", [ IsPath, IsPath ] );
#! @EndGroup

#! @InsertChunk PathMultiplication

DeclareOperation( "FoldLeft", [ IsList, IsFunction ] );

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


#! @Section Decomposition of paths

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


#! @Section Subpaths and path division

#! @BeginGroup Subpath
#! @Description
#!  Extract a subpath of the path <A>p</A>.
#!  <P/>
#!  The integers <A>from</A> and <A>to</A> identify vertices
#!  that the path passes through, counted from 0 to <C>Length( <A>p</A> )</C>.
#!  The operation <C>Subpath</C> counts the vertices from the source
#!  of the path to the target; the operation <C>SubpathLR</C> counts
#!  the vertices from left to right in multiplication order.
#!  The resulting subpath is the part of <A>p</A> between vertices
#!  <A>from</A> and <A>to</A>.
#!  <P/>
#!  This is a path of length <C><A>to</A> - <A>from</A></C>.
#!  In particular, if <C><A>from</A> = <A>to</A></C>, then the result
#!  is a vertex.
#! @Returns <C>IsPath</C>
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
#!  <P/>
#!  If <A>q</A> is not a subpath of <A>p</A>, then <C>fail</C> is returned.
#!  If <A>q</A> is a subpath of <A>p</A>, then
#!  <C>SubpathIndex( <A>p</A>, <A>q</A> )</C> returns an integer <C>i</C> such that
#!  <C>Subpath( <A>p</A>, i, i + Length( <A>q</A> ) ) = <A>q</A></C>, and
#!  <C>SubpathIndexLR( <A>p</A>, <A>q</A> )</C> returns an integer <C>j</C> such that
#!  <C>SubpathLR( <A>p</A>, j, j + Length( <A>q</A> ) ) = <A>q</A></C>.
#!  <P/>
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
#!  <P/>
#!  If <A>q</A> is not a subpath of <A>p</A>, then <C>fail</C> is returned.
#!  If <A>q</A> is a subpath of <A>p</A>, then the result is a list
#!  <C>[ r1, r2 ]</C> of two paths such that
#!  <C>ComposePaths( r1, <A>q</A>, r2 ) = <A>p</A></C>.
#!  <P/>
#!  If <A>q</A> occurs more than once as a subpath of <A>p</A>, then
#!  the first occurence (in source-to-target order) is used; that is, the
#!  same occurence that would be found by <C>SubpathIndex( <A>p</A>, <A>q</A> )</C>.
#! @Arguments p, q
#! @Returns list containing two paths or <C>fail</C>
DeclareOperation( "ExtractSubpath", [ IsPath, IsPath ] );

#! @InsertChunk Example_ExtractSubpath

#! @Description
#!  Divide the path <A>p</A> by the path <A>q</A>, if possible.
#!  <P/>
#!  If <A>q</A> is not a subpath of <A>p</A>, then <C>fail</C> is returned.
#!  If <A>q</A> is a subpath of <A>p</A>, then the result is a list
#!  <C>[ r1, r2 ]</C> of two paths such that
#!  <C>r1 * <A>q</A> * r2 = <A>p</A></C>.
#!  <P/>
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
#!  <P/>
#!  Returns a list of all pairs <C>[ b, c ]</C> of paths such that
#!  <C><A>p</A> * c = b * <A>q</A></C>.
#! @Arguments p, q
#! @Returns list of pairs of paths
DeclareOperation( "PathOverlaps", [ IsPath, IsPath ] );

#! @InsertChunk Example_PathOverlaps
