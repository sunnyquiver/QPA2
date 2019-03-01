#! @Chapter Quivers

#! @Section Categories for paths and quivers

#! @BeginGroup IsPathGroup
#! @Description
#!  Every path is in the category <C>IsPath</C>.
#!  Additionally, every path in a left-oriented quiver is in
#!  the category <C>IsLeftPath</C>, and
#!  every path in a right-oriented quiver is in
#!  the category <C>IsRightPath</C>.
DeclareCategory( "IsPath", IsMultiplicativeElement and IsObjectWithDirection );
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
DeclareCategory( "IsQuiverVertex", IsPrimitivePath );

#!
DeclareCategory( "IsArrow", IsNontrivialPath and IsPrimitivePath );

#! @Description
#!  Every quiver is in the category <C>IsQuiver</C>.
#!  Additionally, a left-oriented quiver is in the category
#!  <Ref Filt="IsLeftQuiver"/>,
#!  and a right-oriented quiver is in the category <C>IsRightQuiver</C>.
DeclareCategory( "IsQuiver", CategoryCollections( IsPath ) and IsObjectWithDirection );
#! @Description
#!  Category for left-oriented quivers.
DeclareCategory( "IsLeftQuiver", IsQuiver );
#! @Description
#!  Category for right-oriented quivers.
DeclareCategory( "IsRightQuiver", IsQuiver );


#! @Section Constructing quivers

#! @BeginGroup LeftQuiver
#! @Returns <Ref Filt="IsLeftQuiver"/>
#! @Description
#!  Constructor for left-oriented quivers.
#!  <P/>
#!  The three first forms of the constructor are the most useful ones for
#!  interactive use and for creating specific quivers in programs.
#!  The two last forms are mainly intended for use in programmatic generation
#!  of quivers, that is, for functions that create quivers based on some
#!  dynamic data.
#!  <P/>
#!  In the first form, the quiver is specified by a label (an arbitrary name for the quiver),
#!  the number of vertices, and a list of lists describing the arrows;
#!  for example <C>LeftQuiver( "Q", 2, [ [ 'a', 1, 2 ], [ 'b', 2, 1 ] ] )</C>.
#!  The first argument can optionally contain patterns for automatic labelling
#!  of vertices and arrows, for example:
#!  <C>LeftQuiver( "Q(u1)[a]", 2, [ [ "u1", "u2" ], [ "u2", "u1" ] ] )</C>.
#!  <P/>
#!  In the second form of the constructor, the vertices are specified by
#!  a list of labels instead of just a number.
#!  The number of vertices is determined by the length of this list.
#!  For example: <C>LeftQuiver( "Q", [ 'u', 'v' ], [ [ 'a', 'u', 'v' ], [ 'b', 'v', 'u' ] ] )</C>.
#!  <P/>
#!  In the third form, the quiver is described by just one string,
#!  for example: <C>LeftQuiver( "Q(u,v)[a:u->v,b:v->u]" )</C>.
#!  <P/>
#!  In the fourth form, the number of vertices and the labels for vertices are
#!  given as separate arguments.
#!  This makes it possible to assign labels to only some of the vertices,
#!  and let the rest be labelled automatically.
#!  Additionally, this form of the constructor takes the quiver label
#!  and the patterns for labels of vertices and arrows as a list of three strings,
#!  instead of a single string.
#!  <P/>
#!  In the last form of the constructor, the source and target vertices of each arrow
#!  must be specified by indices, and not by labels
#!  (all other forms of the constructor accept both labels and indices).
#!  This makes it possible to unambigously use indices even if
#!  the vertices have integers as labels.
#!  This form also allows the quiver label to be any object,
#!  not necessarily a string.
#!  <P/>
#!  The arguments to the <C>LeftQuiver</C> constructor have the following meanings:
#!  <List>
#!  <Mark><A>label_with_patterns</A></Mark>
#!  <Item>
#!  A string containing a label for the quiver (this is just an arbitrary
#!  name decided by the user or program who creates the quiver),
#!  optionally followed by patterns for labelling the vertices and arrows.
#!  The pattern for vertices, if present, is enclosed by parentheses: ();
#!  the pattern for arrows, if present, is enclosed by brackets: [].
#!  If both patterns are present, then the pattern for vertices should appear
#!  before the one for arrows.
#!  <P/>
#!  For example, the value can be <C>"Q"</C> to give the quiver the label <Q>Q</Q>
#!  and use the default pattern for vertices and no pattern for arrows;
#!  or <C>"Q(0)"</C> to give the vertices labels 0, 1, 2, ...;
#!  or <C>"Q[a]"</C> to give the arrows labels 'a', 'b', 'c', ...;
#!  or <C>"Q(u1)[alpha_1]"</C> to give the vertices labels "u1", "u2", "u3", ...,
#!  and the arrows labels "alpha_1", "alpha_2", "alpha_3", ....
#!  <P/>
#!  For vertices, the default pattern is <C>"1"</C>, which means that the
#!  vertices are labelled with positive integers 1, 2, 3, ....
#!  For arrows, there is no default pattern;
#!  if the arrow pattern is not specified, then every arrow must be assigned a label explicitly.
#!  <P/>
#!  Writing an empty string as a pattern is equivalent to not specifying the pattern
#!  (for example, <C>"Q()[]"</C> is equivalent to <C>"Q"</C>).
#!  </Item>
#!  <Mark><A>num_vertices</A></Mark>
#!  <Item>
#!  The number of vertices in the quiver.
#!  </Item>
#!  <Mark><A>arrows</A></Mark>
#!  <Item>
#!  A list of arrow descriptions.
#!  Each arrow is described by either a list
#!  <C>[ label, source, target ]</C>
#!  or a list
#!  <C>[ source, target ]</C>,
#!  where <C>label</C> is the label of the arrow (an arbitrary object),
#!  source is the label or index of the source vertex,
#!  and target is the label or index of the target vertex.
#!  If the second form (without label) is used for some arrows,
#!  then a pattern for labelling the arrows must be specified in
#!  the argument <A>label_with_patterns</A> (or <A>label_with_patterns_list</A>).
#!  </Item>
#!  <Mark><A>vertex_labels</A></Mark>
#!  <Item>
#!  A list of labels for the vertices, given in the order of the vertices.
#!  Each label is an arbitrary object.
#!  All the labels should be distinct, and they should also be different from
#!  all arrow labels.
#!  <P/>
#!  When the argument <A>num_vertices</A> is not used, the number of vertices
#!  is determined by the length of the list <A>vertex_labels</A>.
#!  In this case, the list must be dense.
#!  <P/>
#!  When used together with the argument <A>num_vertices</A>, the list <A>vertex_labels</A>
#!  must not be longer than <A>num_vertices</A>, but it may be shorter, and it may
#!  contain holes.
#!  In this case, the vertices which are not present in the list are assigned
#!  labels automatically according to the vertex label pattern specified
#!  (if no pattern is specified, then they are labelled 1, 2, 3, ...).
#!  </Item>
#!  <Mark><A>description</A></Mark>
#!  <Item>
#!  A string containing all the information needed for creating the quiver.
#!  The string contains first the label for the quiver,
#!  then a specification of the vertices, enclosed in parentheses,
#!  then a specification of the arrows, enclosed in brackets.
#!  <P/>
#!  The specification of the vertices has one of the following forms:
#!  <List>
#!  <Item>
#!  A positive integer, which is the number of vertices.
#!  With this form, the vertices are labelled in the default way, with integers 1, 2, 3, ... .
#!  </Item>
#!  <Item>
#!  A label for each vertex, separated by commas.
#!  </Item>
#!  <Item>
#!  Two patterns of the same form, separated by <Q>..</Q>, specifying a range.
#!  For example, the vertex specification <C>"u1..u4"</C> produces
#!  four vertices labelled <C>"u1"</C>, <C>"u2"</C>, <C>"u3"</C>, <C>"u4"</C>.
#!  The vertex specification <C>"g..k"</C> produces five vertices labelled
#!  <C>'g'</C>, <C>'h'</C>, <C>'i'</C>, <C>'j'</C>, <C>'k'</C>.
#!  </Item>
#!  </List>
#!  The specification of the arrows contains descriptions of the form
#!  <C>"label:source->target"</C> for each arrow, separated by commas.
#!  <P/>
#!  As an example of a complete quiver description string, the string
#!  <C>"Q(3)[a:1->2,b:2->3]"</C>
#!  describes a quiver named <Q>Q</Q>,
#!  with three vertices labelled 1, 2, 3,
#!  and two arrows 'a' and 'b'.
#!  </Item>
#!  <Mark><A>label_with_patterns_list</A></Mark>
#!  <Item>
#!  Contains the same information as the argument <A>label_with_patterns</A>,
#!  but as a list <C>[ label, vertex_pattern, arrow_pattern ]</C>
#!  of three strings.
#!  Each of the strings <C>vertex_pattern</C> and <C>arrow_pattern</C> may be
#!  the empty string.
#!  For the vertex pattern, the empty string is equivalent to the string "1",
#!  meaning that the vertices are labelled with positive integers 1, 2, 3, ....
#!  For the arrow pattern, the empty string means that no pattern is used,
#!  and thus every arrow needs to be labelled explicitly.
#!  </Item>
#!  <Mark><A>label</A></Mark>
#!  <Item>
#!  The label for the quiver.
#!  </Item>
#!  <Mark><A>arrow_labels</A></Mark>
#!  <Item>
#!  A dense list containing labels for all the arrows.
#!  The number of arrows is determined by the length of this list,
#!  which must be the same as the lengths of the lists
#!  <A>source_indices</A> and <A>target_indices</A>.
#!  </Item>
#!  <Mark><A>source_indices</A></Mark>
#!  <Item>
#!  A dense list containing the source vertex of each arrow,
#!  given as a positive integer (the index of the vertex).
#!  </Item>
#!  <Mark><A>target_indices</A></Mark>
#!  <Item>
#!  A dense list containing the target vertex of each arrow,
#!  given as a positive integer (the index of the vertex).
#!  </Item>
#!  </List>
#! @Arguments label_with_patterns, num_vertices, arrows
DeclareOperation( "LeftQuiver", [ IsString, IsPosInt, IsDenseList ] );
#! @Arguments label_with_patterns, vertex_labels, arrows
DeclareOperation( "LeftQuiver", [ IsString, IsDenseList, IsDenseList ] );
#! @Arguments description
DeclareOperation( "LeftQuiver", [ IsString ] );
#! @Arguments label_with_patterns_list, num_vertices, vertex_labels, arrows
DeclareOperation( "LeftQuiver", [ IsDenseList, IsPosInt, IsList, IsDenseList ] );
#! @Arguments label, vertex_labels, arrow_labels, source_indices, target_indices
DeclareOperation( "LeftQuiver", [ IsObject, IsDenseList, IsDenseList, IsDenseList, IsDenseList ] );
#! @EndGroup


#! @InsertChunk Example_LeftQuiver

#! @BeginGroup RightQuiver
#! @Returns <Ref Filt="IsLeftQuiver"/>
#! @Description
#!  Constructor for right-oriented quivers.
#!  <P/>
#!  This works exactly like <Ref Oper="LeftQuiver"/>, except that the quiver
#!  is right-oriented (that is, the convention for order in multiplication of
#!  paths is the opposite of that used for left-oriented quivers).
#! @Arguments label_with_patterns, num_vertices, arrows
DeclareOperation( "RightQuiver", [ IsString, IsPosInt, IsDenseList ] );
#! @Arguments label_with_patterns, vertex_labels, arrows
DeclareOperation( "RightQuiver", [ IsString, IsDenseList, IsDenseList ] );
#! @Arguments description
DeclareOperation( "RightQuiver", [ IsString ] );
#! @Arguments label_with_patterns_list, num_vertices, vertex_labels, arrows
DeclareOperation( "RightQuiver", [ IsDenseList, IsPosInt, IsList, IsDenseList ] );
#! @Arguments label, vertex_labels, arrow_labels, source_indices, target_indices
DeclareOperation( "RightQuiver", [ IsObject, IsDenseList, IsDenseList, IsDenseList, IsDenseList ] );
#! @EndGroup

#! @InsertChunk Example_RightQuiver

#! @BeginGroup Quiver
#! @Returns <Ref Filt="IsQuiver"/>
#! @Description
#!  Constructor for quivers.
#!  <P/>
#!  These operations are exactly the same as <Ref Oper="LeftQuiver"/>
#!  and <Ref Oper="RightQuiver"/>, except that they have an additional
#!  first argument <A>quiver_cat</A>, which specifies whether the
#!  quiver should be left- or right-oriented.
#!  The value of this argument should be either <Ref Filt="IsLeftQuiver"/>
#!  or <Ref Filt="IsRightQuiver"/>.  The last option allows the user to 
#!  define a quiver via an adjacency matrix. 
#! @Arguments quiver_cat, label_with_patterns, num_vertices, arrows
DeclareOperation( "Quiver", [ IsDirection, IsString, IsPosInt, IsDenseList ] );
#! @Arguments quiver_cat, label_with_patterns, vertex_labels, arrows
DeclareOperation( "Quiver", [ IsDirection, IsString, IsDenseList, IsDenseList ] );
#! @Arguments quiver_cat, description
DeclareOperation( "Quiver", [ IsDirection, IsString ] );
#! @Arguments quiver_cat, label_with_patterns_list, num_vertices, vertex_labels, arrows
DeclareOperation( "Quiver", [ IsDirection, IsDenseList, IsPosInt, IsList, IsDenseList ] );
#! @Arguments quiver_cat, label, vertex_labels, arrow_labels, source_indices, target_indices
DeclareOperation( "Quiver", [ IsDirection, IsObject, IsDenseList, IsDenseList, IsDenseList, IsDenseList ] );
#! @Arguments quiver_cat, label, matrix
DeclareOperation( "Quiver", [ IsDirection, IsObject, IsMatrix ] );
#! @EndGroup Quiver

#! @InsertChunk Example_Quiver

DeclareOperation( "DecomposeQuiverDescriptionString", [ IsString ] );
DeclareOperation( "ParseLabelPatternString", [ IsString ] );
DeclareOperation( "ApplyLabelPattern", [ IsDenseList, IsPosInt ] );
DeclareOperation( "MakeLabelsFromPatternObj", [ IsDenseList, IsPosInt, IsList ] );

#! @BeginGroup MakeLabelsFromPattern
#! @Returns list
#! @Description
#!  Make a list of <A>n</A> labels (for vertices or arrows)
#!  from the pattern <A>pattern</A>.
#!  The argument <A>fixed_labels</A> may be used to specify some
#!  labels that deviate from the pattern.
#!  <P/>
#!  This is useful in combination with the quiver constructors
#!  that have the arguments
#!  <A>vertex_labels</A>, <A>arrow_labels</A>, <A>source_indices</A>, and <A>target_indices</A>.
#!  This operation gives a convenient way to produce the lists
#!  <A>vertex_labels</A> and <A>arrow_labels</A>.
#! @Arguments pattern, n
DeclareOperation( "MakeLabelsFromPattern", [ IsString, IsPosInt ] );
#! @Arguments pattern, n, fixed_labels
DeclareOperation( "MakeLabelsFromPattern", [ IsString, IsPosInt, IsList ] );
#! @EndGroup MakeLabelsFromPattern

#! @InsertChunk Example_MakeLabelsFromPattern

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
DeclareAttribute( "Label", IsQuiver );

#! @Arguments Q
#! @Description
#!  The label of the quiver <A>Q</A>, as a string.
#! @Returns <C>IsString</C>
DeclareAttribute( "LabelAsString", IsQuiver );

#! @Arguments Q
#! @Returns category
#! @Description
#!  Returns the category <A>Q</A> belongs to;
#!  either <Ref Filt="IsLeftQuiver"/> or <Ref Filt="IsRightQuiver"/>.
DeclareAttribute( "QuiverCategory", IsQuiver );


#! @Section Accessing paths in a quiver

#! @Description
#!  Returns the vertices of the quiver <A>Q</A> as a list.
#!  <P/>
#!  The ordering of the list corresponds to the ordering of the vertices
#!  in the quiver.
#!  That is, the vertex at position <C>i</C> in the list has index <C>i</C>
#!  (see <Ref Attr="VertexIndex" Label="for IsQuiverVertex"/>) and is the vertex which is returned by
#!  <C>Vertex( <A>Q</A>, i )</C>.
#! @Arguments Q
#! @Returns list of <Ref Filt="IsQuiverVertex"/>
DeclareOperation( "Vertices", [ IsQuiver ] );
DeclareAttribute( "VerticesAttr", IsQuiver );

#! @Description
#!  Returns the arrows of the quiver <A>Q</A> as a list.
#!  <P/>
#!  The ordering of the list corresponds to the ordering of the arrows
#!  in the quiver.
#!  That is, the arrow at position <C>i</C> in the list has index <C>i</C>
#!  (see <C>ArrowIndex</C>) and is the arrow which is returned by
#!  <C>Arrow( <A>Q</A>, i )</C>.
#! @Arguments Q
#! @Returns list of <Ref Filt="IsArrow"/>
DeclareAttribute( "Arrows", IsQuiver );

#! @Description
#!  Returns the labels of the vertices of the quiver <A>Q</A> as a list.
#!  <P/>
#!  The ordering of the list corresponds to the ordering of the vertices
#!  in the quiver.
#! @Arguments Q
#! @Returns list
DeclareAttribute( "VertexLabels", IsQuiver );

#! @Description
#!  Returns the arrows of the quiver <A>Q</A> as a list.
#!  <P/>
#!  The ordering of the list corresponds to the ordering of the arrows
#!  in the quiver.
#! @Arguments Q
#! @Returns list
DeclareAttribute( "ArrowLabels", IsQuiver );

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
#! @Returns list of <Ref Filt="IsPrimitivePath"/>
DeclareAttribute( "PrimitivePaths", IsQuiver );

#! @Description
#!  The vertex with index <A>i</A> in the quiver <A>Q</A>.
#! @Arguments Q, i
#! @Returns <C>IsQuiverVertex</C>
DeclareOperation( "Vertex", [ IsQuiver, IsPosInt ] );

#! @Description
#!  The arrow with index <A>i</A> in the quiver <A>Q</A>.
#! @Arguments Q, i
#! @Returns <Ref Filt="IsArrow"/>
DeclareOperation( "Arrow", [ IsQuiver, IsPosInt ] );

#! @BeginGroup PrimitivePathByLabel
#! @Description
#!  Returns the primitive path (vertex or arrow) of the quiver <A>Q</A>
#!  which has <A>label</A> as label, if any.
#!  If no such path exists, then <C>fail</C> is returned.
#!  The operation <C><A>Q</A>[ <A>label</A> ]</C> is equivalent to
#!  <C>PrimitivePathByLabel( <A>Q</A>, <A>label</A> )</C>.
#! @Returns <Ref Filt="IsPrimitivePath"/> or <C>fail</C>
#! @Arguments Q, label
DeclareOperation( "PrimitivePathByLabel", [ IsQuiver, IsObject ] );
#! @Arguments Q, label
DeclareOperation( "\[\]", [ IsQuiver, IsObject ] );
#! @EndGroup

#!
DeclareOperation( "PathFromString", [ IsQuiver, IsString ] );
#DeclareOperation( "\.", [ IsQuiver, IsPosInt ] );


#! @Arguments Q
#! @Returns list of vertices
#! @Description
#!  Returns a list of all the vertices that are sources in the quiver,
#!  in the sense that no arrow ends in them.
DeclareAttribute( "SourceVertices", IsQuiver );

#! @Arguments Q
#! @Returns list of vertices
#! @Description
#!  Returns a list of all the vertices that are sinks in the quiver,
#!  in the sense that no arrow starts in them.
DeclareAttribute( "SinkVertices", IsQuiver );


#! @Section Information about a path

#! @Arguments p
#! @Returns <Ref Filt="IsQuiver"/>
#! @Description
#!  The quiver containing the path <A>p</A>.
DeclareAttribute( "QuiverOfPath", IsPath );

#! @BeginGroup PathEnds
#! @Returns <Ref Filt="IsQuiverVertex"/>
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

DeclareAttribute( "ArrowString", IsArrow );

DeclareGlobalFunction( "QPA_LABEL_TO_STRING" );

#! @Arguments v
#! @Returns positive integer
#! @Description
#!  The index of the vertex <A>v</A>.
#!  In a quiver with $n$ vertices, the vertices are assigned
#!  indices $1, \ldots, n$ when the quiver is constructed.
DeclareAttribute( "VertexIndex", IsQuiverVertex );

#! @Arguments a
#! @Returns positive integer
#! @Description
#!  The index of the arrow <A>a</A>.
#!  In a quiver with $m$ arrows, the arrows are assigned
#!  indices $1, \ldots, m$ when the quiver is constructed.
DeclareAttribute( "ArrowIndex", IsArrow );

#! @Arguments v
#! @Returns list of arrows
#! @Description
#!  A list containing all arrows starting in the vertex <A>v</A>.
DeclareAttribute( "OutgoingArrows", IsQuiverVertex );

#! @Arguments v
#! @Returns list of arrows
#! @Description
#!  A list containing all arrows ending in the vertex <A>v</A>.
DeclareAttribute( "IncomingArrows", IsQuiverVertex );

#! @Arguments v
#! @Returns IsInt
#! @Description
#!  The outdegree of the vertex <A>v</A>; that is, the number of arrows
#!  starting in <A>v</A>.
DeclareAttribute( "Outdegree", IsQuiverVertex );

#! @Arguments v
#! @Returns IsInt
#! @Description
#!  The indegree of the vertex <A>v</A>; that is, the number of arrows
#!  ending in <A>v</A>.
DeclareAttribute( "Indegree", IsQuiverVertex );

#! @Arguments v
#! @Returns IsInt
#! @Description
#!  The degree of the vertex <A>v</A>; that is, the number of arrows
#!  starting or ending in <A>v</A>.
DeclareAttribute( "DegreeOfVertex", IsQuiverVertex );

#! @Arguments v
#! @Returns list of vertices
#! @Description
#!  Returns a list containing the neighbors of the vertex <A>v</A>;
#!  that is, all vertices <C>w</C> such that there is an arrow from
#!  <A>v</A> to <C>w</C> or from <C>w</C> to <A>v</A>.
DeclareAttribute( "Neighbors", IsQuiverVertex );


#! @Section Composition of paths

#! @BeginGroup ComposePathsGroup
#! @Arguments p_1, p_2, ..., p_n
#! @Returns <Ref Filt="IsPath"/> or <C>fail</C>
#! @Description
#!  Compose the paths <A>p_1</A>, <A>p_2</A>, ..., <A>p_n</A>, if possible.
#!  <P/>
#!  For the operation <C>ComposePaths</C>, the paths should be given in source-to-target order.
#!  For the operation <C>ComposePathsLR</C>, the paths should be given in multiplication order.
#!  <P/>
#!  The function <C>ComposePaths</C>, which takes an arbitrary number
#!  of arguments, is implemented by repeated calls to the
#!  two-argument operation <C>ComposePaths2</C>.
#!  <P/>
#!  The functions <C>ComposePaths</C> and <C>ComposePathsLR</C> can also
#!  be called with a single argument, which is a list of paths.
DeclareGlobalFunction( "ComposePaths" );
#! @Arguments p_1, p_2, ..., p_n
DeclareGlobalFunction( "ComposePathsLR" );
#! @Arguments p1, p2
DeclareOperation( "ComposePaths2", [ IsPath, IsPath ] );
#! @EndGroup

#! @InsertChunk PathMultiplication

#! @Arguments p1, p2
#! @Returns <C>true</C> or <C>false</C>
#! @Description
#!  Checks if the paths <A>p1</A> and <A>p2</A> can be composed
#!  in source-to-target order;
#!  that is, if the target of <A>p1</A> equals the source of <A>p2</A>.
#!  <P/>
#!  If this operation returns <C>true</C>,
#!  then calling <C>Compose( <A>p1</A>, <A>p2</A> )</C> will produce a path.
#!  If this operation returns <C>false</C>,
#!  then <C>Compose( <A>p1</A>, <A>p2</A> )</C> will return <C>fail</C>.
DeclareOperation( "Composable", [ IsPath, IsPath ] );

#! @Arguments p1, p2
#! @Returns <C>true</C> or <C>false</C>
#! @Description
#!  Checks if the paths <A>p1</A> and <A>p2</A> can be composed
#!  in multiplication order.
#!  <P/>
#!  If this operation returns <C>true</C>,
#!  then <C><A>p1</A> * <A>p2</A></C> will produce a path.
#!  If this operation returns <C>false</C>,
#!  then <C><A>p1</A> * <A>p2</A></C> will return <C>fail</C>.
DeclareOperation( "ComposableLR", [ IsPath, IsPath ] );

#! @BeginGroup PathFromArrowList
#! @Arguments list
#! @Returns <Ref Filt="IsPath"/>
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
#! @Returns list of <Ref Filt="IsArrow"/>
#! @Arguments p
#! @Description
#!  The path <A>p</A> decomposed as a list of arrows.
#! @Returns list of arrows
DeclareAttribute( "ArrowList", IsPath );
#! @Arguments p
DeclareAttribute( "ArrowListLR", IsPath );
#! @EndGroup

#! @BeginGroup DecomposePath
#! @Returns list of <Ref Filt="IsPrimitivePath"/>
#! @Arguments p
#! @Description
#!  The path <A>p</A> decomposed as a list of primitive paths.
#! @Returns list of primitive paths
DeclareAttribute( "AsList", IsPath );
DeclareAttribute( "AsListLR", IsPath );
#! @EndGroup
#! @InsertChunk Example_Decompose


#! @Section Path manipulation

#! @Arguments p, f
DeclareOperation( "TranslatePath", [ IsPath, IsFunction ] );


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
#! @Returns <Ref Filt="IsPath"/>
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
#! @Returns nonnegative integer or <C>fail</C>
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


#! @Section Quiver homomorphisms

#! @Description
#!  GAP category for quiver homomorphisms.
#! @Label
DeclareCategory( "IsQuiverHomomorphism",
                 IsMapping and IsSPGeneralMapping );

#! @Description
#!  Create a quiver homomorphism.
#!
#!  The result is a homomorphism from the quiver <A>Q1</A>
#!  to the quiver <A>Q2</A>
#!  where the lists <A>vertex_images</A> and <A>arrow_images</A>
#!  determine what each vertex and arrow is sent to.
#!  The homomorphism sends vertex number <C>i</C> in <A>Q1</A>
#!  to <C>vertex_images[ i ]</C>
#!  and arrow number <C>j</C> in <A>Q1</A>
#!  to <C>arrow_images[ j ]</C>.
#! @Arguments Q1, Q2, vertex_images, arrow_images
#! @Returns <Ref Filt="IsQuiverHomomorphism"/>
#! @Label
DeclareOperation( "QuiverHomomorphism",
                  [ IsQuiver, IsQuiver, IsDenseList, IsDenseList ] );

#! @Description
#!  Create a quiver homomorphism.
#!
#!  This operation is like <Ref Oper="QuiverHomomorphism"/>
#!  except that no checking of the arguments is performed.
#! @Arguments Q1, Q2, vertex_images, arrow_images
#! @Returns <Ref Filt="IsQuiverHomomorphism"/>
DeclareOperation( "QuiverHomomorphismNC",
                  [ IsQuiver, IsQuiver, IsDenseList, IsDenseList ] );

#! @Description
#!  The images of all vertices, for a quiver homomorphism <A>m</A>.
#! @Arguments m
#! @Returns list of vertices
DeclareAttribute( "VertexImages", IsQuiverHomomorphism );

#! @Description
#!  The images of all arrows, for a quiver homomorphism <A>m</A>.
#! @Arguments m
#! @Returns list of arrows
DeclareAttribute( "ArrowImages", IsQuiverHomomorphism );

#! @Description
#!  The source of a quiver homomorphism.
#! @Arguments m
#! @Returns <Ref Filt="IsQuiver"/>
#DeclareAttribute( "Source", IsQuiverHomomorphism );

#! @Description
#!  The range of a quiver homomorphism.
#! @Arguments m
#! @Returns <Ref Filt="IsQuiver"/>
#DeclareAttribute( "Range", IsQuiverHomomorphism );

#! @Description
#!  Apply the quiver homomorphism <A>m</A> to the path <A>p</A>.
#! @Arguments m, p
#! @Returns <Ref Filt="IsPath"/>
#DeclareGlobalFunction( "Image" );


#! @Section Quiver constructions

#! @Arguments Q, label
#! @Returns <Ref Filt="IsQuiver"/>
#! @Description
#!  Returns a new quiver which is equal to the quiver <A>Q</A>
#!  but has the object <A>label</A> as its label.
DeclareOperation( "RenameQuiver", [ IsObject, IsQuiver ] );

#! @Arguments Q
#! @Returns <Ref Filt="IsQuiver"/>
#! @Description
#!  Returns the opposite quiver of <A>Q</A>.
DeclareAttribute( "OppositeQuiver", IsQuiver );

#! @Description
#!  Returns the string <A>s</A> with the suffix <A>suffix</A> either added
#!  or removed, based on whether it is already present in <A>s</A>.
#! @Arguments s, suffix
#! @Returns string
DeclareOperation( "ToggleSuffix", [ IsString, IsString ] );

#! @Arguments p
#! @Returns <Ref Filt="IsPath"/>
#! @Description
#!  Returns the path corresponding to <A>p</A> in the opposite quiver.
DeclareAttribute( "OppositePath", IsPath );

#! @Arguments Q, R
#! @Returns <Ref Filt="IsQuiver"/>
#! @Description
#!  Returns the product quiver of <A>Q</A> and <A>R</A>.
DeclareOperation( "QuiverProduct", [ IsQuiver, IsQuiver ] );

#! @Arguments L
#! @Returns <Ref Filt="IsQuiver"/>
#! @Description
#!  Returns the product quiver of the quivers in the list <A>L</A>.
DeclareOperation( "QuiverProduct", [ IsDenseList ] );

#! @Description
#!  Checks whether <A>Q</A> is a product quiver.
#! @Returns IsBool
#! @Arguments Q
DeclareProperty( "IsProductQuiver", IsQuiver );

#! @Description
#!  Factor a product quiver.
#!  Given a product quiver <A>Q</A>, returns the quivers
#!  that <A>Q</A> is a product of.
#! @Returns list of quivers
#! @Arguments Q
DeclareAttribute( "ProductQuiverFactors", IsQuiver );

#! @Description
#!  Returns the <A>i</A>-th factor of the product quiver <A>Q</A>.
#! @Returns IsQuiver
#! @Arguments Q, i
DeclareOperation( "ProductQuiverFactor", [ IsProductQuiver, IsPosInt ] );

#! @Description
#!  Factor a product quiver, with left/right translation.
#!
#!  Given a product quiver <A>Q</A> which is the product of exactly
#!  two quivers, this function returns a list containing one of these
#!  quivers and the opposite of the other.
#! @Returns list of quivers
#! @Arguments Q
DeclareAttribute( "ProductQuiverFactorsLeftRight", IsQuiver );

#! @Description
#!  Project a path from a product quiver to one of the factors.
#!  The argument <A>i</A> is a positive integer, and <A>p</A> is a
#!  path in a product quiver.
#!  The result is the projection of <A>p</A> to the <A>i</A>-th factor
#!  of the product quiver.
#! @Returns IsPath
#! @Arguments i, p
DeclareOperation( "ProjectPathFromProductQuiver", [ IsPosInt, IsPath ] );

#! @Description
#!  Project a path from a product quiver to all factors.
#! @Returns list of paths
#! @Arguments p
DeclareAttribute( "ProductPathFactors", IsPath );

#! @Description
#!  Project a path from a product quiver to both factors,
#!  with left/right translation.
#! @Returns list of paths
#! @Arguments p
DeclareAttribute( "ProductPathFactorsLeftRight", IsPath );

#! @Description
#!  Include a path into a product quiver.
#! @Returns IsPath
#! @Arguments PQ, n, vertices, p
DeclareOperation( "IncludePathInProductQuiver", [ IsProductQuiver, IsPosInt, IsList, IsPath ] );

#! @Description
#!  Create a path in a product quiver.
#!  The argument <A>Q</A> is a product quiver,
#!  and <A>paths</A> is a list of paths from each factor of <A>Q</A>.
#!  The result is a path in <A>Q</A> with <A>paths</A> as its projections
#!  to each factor.
#! @Arguments Q, paths
DeclareOperation( "PathInProductQuiver", [ IsProductQuiver, IsDenseList ] );

#! @Description
#!  Create a path in a product quiver.
#!  The argument <A>Q</A> is a product quiver,
#!  and <A>paths</A> is a list of paths from each factor of <A>Q</A>.
#!  The result is a path in <A>Q</A> with <A>paths</A> as its projections
#!  to each factor.
#!
#!  The argument <A>permutation</A> is a permutation which describes
#!  in which order the given paths should appear in the resulting path.
#! @Arguments Q, paths, permutation
DeclareOperation( "PathInProductQuiver", [ IsProductQuiver, IsDenseList, IsPerm ] );

DeclareOperation( "ProductQuiverInclusion", [ IsProductQuiver, IsPosInt, IsPosInt ] );

DeclareAttribute( "ProductQuiverInclusions", IsProductQuiver );

#! @Description
#!  Gives the vertex index of a certain vertex in a product quiver.
#!  The argument <A>quivers</A> is a list of quivers,
#!  and the argument <A>vertex_indices</A> is a list of positive integers.
#!  The result is the index of the vertex in <C>QuiverProduct( quivers )</C>
#!  which is made by combining the vertices with the given vertex indices
#!  from each quiver.
#! @Returns positive integer
#! @Arguments quivers, vertex_indices
DeclareOperation( "ProductQuiverVertexIndex", [ IsDenseList, IsDenseList ] );

#! @Description
#!  Gives the arrow index of a certain arrow in a product quiver.
#!  The argument <A>quivers</A> is a list of quivers,
#!  <A>i</A> is a positive integer, and
#!  <A>path_indices</A> is a list of positive integers.
#!  Each element in <A>path_indices</A> should be the index of a vertex
#!  in the corresponding quiver, except <C>path_indices[i]</C>, which
#!  is the index of an arrow.
#!  The result is the index of the arrow in <C>QuiverProduct( quivers )</C>
#!  which is made by combining the given arrow and vertices.
#! @Returns positive integer
#! @Arguments quivers, i, path_indices
DeclareOperation( "ProductQuiverArrowIndex", [ IsDenseList, IsPosInt, IsDenseList ] );


DeclareOperation( "\^", [ IsQuiver, IsSide ] );
DeclareOperation( "\^", [ IsPath, IsSide ] );


#! @Section Path iterators

#! @Arguments Q
#! @Returns iterator
#! @Description
#!  Produces an iterator which iterates over the paths of the quiver.
#!  The paths appear in order from smallest to largest
#!  (as given by the <C>\&lt;</C> operator):
#!  first the vertices, then the arrows, and so on.
#!  Use the functions <C>NextIterator</C> and <C>IsDoneIterator</C>
#!  to access elements of the iterator
#!  (for more general information about iterators, see the GAP reference manual).
DeclareOperation( "PathIterator", [ IsQuiver ] );

#! @Arguments Q, f
#! @Returns iterator
#! @Description
#!  Produces an iterator which iterates over paths of the quiver <A>Q</A>
#!  satisfying the predicate function <A>f</A>.
#!  It is assumed that the predicate function has the property that
#!  if it returns true for a given path, then it also returns true for
#!  every prefix of that path (when the paths are written in source-to-target order).
DeclareOperation( "FilteredPathIterator", [ IsQuiver, IsFunction ] );

#! @Arguments Q
#! @Returns list of paths
#! @Description
#!  Produces a list of all paths in the quiver.
#!  This works only for acyclic quivers.
#!  Raises an error if called with a quiver which is not acyclic.
DeclareOperation( "PathList", [ IsQuiver ] );


#! @Section Subquivers

#! @Arguments Q, vertices, arrows
#! @Returns <Ref Filt="IsQuiver"/>
#! @Description
#!  Construct a subquiver of the quiver <A>Q</A> consisting of
#!  the vertices in the list <A>vertices</A> and
#!  the arrows in the list <A>arrows</A>.
DeclareOperation( "Subquiver", [ IsQuiver, IsDenseList, IsDenseList ] );

#! @Arguments Q, arrows
#! @Returns <Ref Filt="IsQuiver"/>
#! @Description
#!  Construct a subquiver of the quiver <A>Q</A> consisting of
#!  the arrows in the list <A>arrows</A> and all vertices adjacent to
#!  these arrows.
DeclareOperation( "Subquiver", [ IsQuiver, IsDenseList ] );

#! @Arguments Q, vertices
#! @Returns <Ref Filt="IsQuiver"/>
#! @Description
#!  Returns the subquiver of <A>Q</A> containing only the vertices
#!  in the list <A>vertices</A>, and all arrows between these vertices.
DeclareOperation( "FullSubquiver", [ IsQuiver, IsDenseList ] );


#! @Section Connected components

#! @Arguments Q
#! @Returns IsBool
#! @Description
#!  Check if the quiver <A>Q</A> is connected.
DeclareProperty( "IsConnected", IsQuiver );

#! @Arguments Q
#! @Returns list of quivers
#! @Description
#!  Returns a list of the connected components of the quiver <A>Q</A>.
DeclareOperation( "ConnectedComponents", [ IsQuiver ] );
DeclareAttribute( "ConnectedComponentsAttr", IsQuiver );

#! @Section Quiver invariants

#! @Description
#!  Returns the adjacency matrix of the quiver <A>Q</A>.
#! @Returns <C>IsMatrix</C>
#! @Arguments Q
DeclareAttribute( "AdjacencyMatrixOfQuiver", IsQuiver );

#! @Description
#!  This function returns the Dynkin type of the quiver <A>Q</A>  if it is a 
#!  Dynkin quiver or extended Dynkin quiver, i.e. the underlying graph is a 
#!  Dynkin diagram or extended Dynkin.  If the quiver is not connected, then
#!  the function returns <C>false</C>.
#! @Returns <C>IsString</C>
#! @Arguments Q
DeclareAttribute( "DynkinType", IsQuiver );

#! @Description
#!  Returns true if the quiver <A>Q</A> is acyclic, that is, without oriented cycles.
#! @Returns <C>IsBool</C>
#! @Arguments Q
DeclareProperty( "IsAcyclicQuiver", IsQuiver );

#! @Description
#!  Returns true if the quiver <A>Q</A> is Dynkin and the property has been set. 
#! @Returns <C>IsBool</C>
#! @Arguments Q
DeclareProperty( "IsDynkinQuiver", IsQuiver );

#! @Description
#!  Returns true if the quiver <A>Q</A> is extended Dynkin and the property has been set. 
#! @Returns <C>IsBool</C>
#! @Arguments Q
DeclareProperty( "IsExtendedDynkinQuiver", IsQuiver );

#! @Description
#!  Functions tests if every vertex in quiver <A>Q</A> is a source (respectively a target) 
#!  of at most 2 arrows.  Note that a path algebra of one loop IS NOT special biserial, but
#!  one loop IS special biserial quiver.
#! @Returns <C>IsBool</C>
#! @Arguments Q
DeclareProperty( "IsSpecialBiserialQuiver", IsQuiver );

#! @Description
#!  Returns true if the quiver <A>Q</A> is a tree. 
#! @Returns <C>IsBool</C>
#! @Arguments Q
DeclareProperty( "IsTreeQuiver", IsQuiver );

#! @Description
#!  Returns true if the underlying graph of the quiver <A>Q</A> has no cycles. 
#! @Returns <C>IsBool</C>
#! @Arguments Q
DeclareProperty( "IsUnorientedAcyclicQuiver", IsQuiver );

#! @Description
#!  Returns the separated quiver of the quiver <A>Q</A>.
#! @Returns <C>IsQuiver</C>
#! @Arguments Q
DeclareOperation( "SeparatedQuiver", [ IsQuiver ] );

#