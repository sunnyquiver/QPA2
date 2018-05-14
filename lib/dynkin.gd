#! @Chapter Special quivers, algebras and modules

#! @Section Dynkin quivers

#! QPA knows how to construct Dynkin quivers of type A, D and E,
#! and extended Dynkin quivers (also known as Euclidean quivers)
#! of type A~, D~ and E~.
#!
#! A Dynkin quiver (with a certain default orientation of the arrows)
#! can be created like this:
#! @InsertChunk Example_DynkinQuiver
#!
#! The orientation can be specified by including '&lt;' and '&gt;' characters,
#! one for each arrow:
#! @InsertChunk Example_DynkinQuiver_orientation
#!
#! It is also possible to give the type, size and (optional) orientation
#! as separate arguments to the constructor:
#! @InsertChunk Example_DynkinQuiver_separate_args
#!
#! These examples use the constructor LeftDynkinQuiver,
#! which produces a left quiver.
#! The constructor RightDynkinQuiver works in exactly the same way,
#! and produces a right quiver.
#! The constructor DynkinQuiver takes an additionial first argument
#! <A>dir</A> which decides whether to produce a left or a right quiver.
#!
#! The vertices and arrows of the Dynkin quivers are numbered like this:
#! <Alt Not="LaTeX">
#! @BeginLogSession
#!           a1    a2      a(n-1)
#! A_n  :  1 --> 2 --> ... --> (n)
#!
#!           a1    a2              a(n)
#! A~_n :  1 --> 2 --> ... --> (n) --> (n+1) -.
#!         ^                                   ) a(n+1)
#!         `----------------------------------´
#!
#!           a1
#!         1 -->   a3                a(n-1)
#! D_n  :        3 --> ... --> (n-1) --> (n)
#!         2 -->
#!           a2
#!
#!           a1                      a(n-1)
#!         1 -->   a3                --> (n)
#! D~_n :        3 --> ... --> (n-1)
#!         2 -->                     --> (n+1)
#!           a2                      a(n)
#!
#!           a1    a2    a3    a4      a(n-2)
#! E_n  :  1 --> 2 --> 3 --> 4 --> ... --> (n-1)
#!                     |
#!                     | a(n-1)
#!                     v
#!                    (n)
#!
#!           a     b     c     d
#! E~_6 :  1 --> 2 --> 3 --> 4 --> 5
#!                     |
#!                     | e
#!                     v
#!                     6
#!                     |
#!                     | f
#!                     v
#!                     7
#!
#!           a     b     c     d     e     f
#! E~_7 :  1 --> 2 --> 3 --> 4 --> 5 --> 6 --> 7
#!                           |
#!                           | g
#!                           v
#!                           8
#!
#!           a     b     c     d     e     f     g
#! E~_8 :  1 --> 2 --> 3 --> 4 --> 5 --> 6 --> 7 --> 8
#!                     |
#!                     | h
#!                     v
#!                     9
#! @EndLogSession
#! </Alt>


#! @BeginGroup DynkinQuiver
#! @Returns <Ref Filt="IsQuiver"/>
#! @Description
#!  Create a Dynkin quiver.
#!  <P/>
#!  The quiver is either described by a single string, or by separate arguments
#!  for type, size and (optionally) orientation of arrows.
#!  <P/>
#!  The possible arguments are:
#!  <List>
#!  <Mark><A>dir</A></Mark>
#!  <Item>
#!  A direction constant: either <C>LEFT</C> or <C>RIGHT</C>,
#!  giving either a left quiver or a right quiver.
#!  (The variants <Ref Oper="LeftDynkinQuiver"/> and
#!  <Ref Oper="RightDynkinQuiver"/> do not take this argument.)
#!  </Item>
#!  <Mark><A>desc</A></Mark>
#!  <Item>
#!  Description of the dynkin quiver.
#!  A string containing first the Dynkin type (either 'A', 'D' or 'E'),
#!  then either a '~' or nothing (for extended or normal Dynkin quiver),
#!  then a number.
#!  After the number, the description string may contain '&lt;' and '&gt;'
#!  characters describing the orientation of arrows, in the same format as
#!  the <A>orientation</A> argument.
#!  </Item>
#!  <Mark><A>type</A></Mark>
#!  <Item>
#!  Dynkin type: Either "A", "D", "E", "A~", "D~" or "E~".
#!  </Item>
#!  <Mark><A>n</A></Mark>
#!  <Item>
#!  Size of the quiver.  For non-extended Dynkin quivers (type "A", "D" or "E"),
#!  this is exactly the number or vertices in the quiver.
#!  For extended Dynkin quivers (type "A~", "D~" or "E~"),
#!  it is one less than the number of vertices.
#!  </Item>
#!  <Mark><A>orientation</A></Mark>
#!  <Item>
#!  A string describing the orientation of the arrows in the quiver.
#!  </Item>
#!  </List>
#! @Arguments dir, desc
DeclareOperation( "DynkinQuiver", [ IsDirection, IsString ] );
#! @Arguments dir, type, n
DeclareOperation( "DynkinQuiver", [ IsDirection, IsString, IsPosInt ] );
#! @Arguments dir, type, n, orientation
DeclareOperation( "DynkinQuiver", [ IsDirection, IsString, IsPosInt, IsString ] );
#! @EndGroup

#! @BeginGroup LeftDynkinQuiver
#! @Returns <Ref Filt="IsLeftQuiver"/>
#! @Description
#!  Create a left Dynkin quiver.
#!  This is exactly like the DynkinQuiver constructor, except that
#!  it does not take a direction argument, and always creates a left quiver.
#! @Arguments desc
DeclareOperation( "LeftDynkinQuiver", [ IsString ] );
#! @Arguments type, n
DeclareOperation( "LeftDynkinQuiver", [ IsString, IsPosInt ] );
#! @Arguments type, n, orientation
DeclareOperation( "LeftDynkinQuiver", [ IsString, IsPosInt, IsString ] );
#! @EndGroup

#! @BeginGroup RightDynkinQuiver
#! @Returns <Ref Filt="IsRightQuiver"/>
#! @Description
#!  Create a right Dynkin quiver.
#!  This is exactly like the DynkinQuiver constructor, except that
#!  it does not take a direction argument, and always creates a right quiver.
#! @Arguments desc
DeclareOperation( "RightDynkinQuiver", [ IsString ] );
#! @Arguments type, n
DeclareOperation( "RightDynkinQuiver", [ IsString, IsPosInt ] );
#! @Arguments type, n, orientation
DeclareOperation( "RightDynkinQuiver", [ IsString, IsPosInt, IsString ] );
#! @EndGroup

#! @BeginGroup DynkinGraph
#! @Returns list of lists
#! @Description
#!  Create a Dynkin diagram as a list of lists.
#!  <P/>
#!  The arguments are interpreted in the same way as those of
#!  <Ref Oper="DynkinQuiver"/>.
#!  The result is a list consisting of lists <C>[ source, target ]</C>
#!  for each arrow of the Dynkin diagram.
#!  The vertices are implicitly numbered by positive integers <M>1, 2, 3, \ldots</M>.
#!  <P/>
#!  This operation is used as a helper function by <Ref Oper="DynkinQuiver"/>.
#! @Arguments type, n
DeclareOperation( "DynkinGraph", [ IsString, IsPosInt ] );
#! @Arguments type, n, orientation
DeclareOperation( "DynkinGraph", [ IsString, IsPosInt, IsString ] );
#! @EndGroup

#! @InsertChunk Example_DynkinGraph

#! @Returns list
#! @Description
#!  Parse a string describing a Dynkin quiver.
#!  The argument <A>desc</A> is a string in the same format
#!  as the <A>desc</A> argument to <Ref Oper="DynkinQuiver"/>.
#!  The result is a list <C>[ type, n, orientation ]</C>.
#!  <P/>
#!  This operation is used as a helper function by <Ref Oper="DynkinQuiver"/>.
#! @Arguments desc
DeclareOperation( "ParseDynkinQuiverString", [ IsString ] );

#
