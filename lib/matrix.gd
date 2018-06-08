#! @Chapter Vector spaces

#! @Section Matrices

#! @Description
#!  Category for QPA's matrices.
DeclareCategory( "IsQPAMatrix", IsMatrixObj );

#! @Arguments M
#! @Returns ring
#! @Description
#!  The ring that <A>M</A> is a matrix over.  In other words, every
#!  element of <A>M</A> lies in this ring.
# matobj2.gd:367:DeclareAttribute( "BaseDomain", IsMatrixObj );

#! @Arguments M
#! @Returns list of length two
#! @Description
#!  Returns a list <C>[height,width]</C> containing the height and
#!  width of the matrix <A>M</A>.
# matobj2.gd:379:DeclareAttribute( "DimensionsMat", IsMatrixObj );   # returns [rows,cols]

#! @Arguments M, i, j
#! @Returns ring element
#! @Description
#!  The element at position (<A>i</A>,<A>j</A>) in the matrix <A>M</A>.
# matobj2.gd:475:DeclareOperation( "MatElm", [IsMatrixObj,IsPosInt,IsPosInt] );

#! @Arguments R, L
#! @Returns <Ref Filt="IsQPAMatrix"/>
#! @Description
#!  Construct a QPA matrix over the ring <A>R</A> containing the
#!  elements in <A>L</A>, which should be a list of lists of ring elements.
#!  Each sublist of <A>L</A> is taken as a row of the resulting matrix <C>M</C>;
#!  that is, the element <C>L[i][j]</C> will be <C>MatElm(M,i,j)</C>.
DeclareOperation( "MatrixByRows", [ IsRing, IsMatrix ] );
#DeclareOperation( "MatrixByRows", [ IsRing, IsDenseList, IsMatrix ] );
DeclareOperation( "MatrixByRows", [ IsRing, IsDenseList, IsDenseList ] );

#! @Arguments R, L
#! @Returns <Ref Filt="IsQPAMatrix"/>
#! @Description
#!  Construct a QPA matrix over the ring <A>R</A> containing the
#!  elements in <A>L</A>, which should be a list of lists of ring elements.
#!  Each sublist of <A>L</A> is taken as a column of the resulting matrix <C>M</C>;
#!  that is, the element <C>L[i][j]</C> will be <C>MatElm(M,j,i)</C>.
DeclareOperation( "MatrixByCols", [ IsRing, IsMatrix ] );
#DeclareOperation( "MatrixByCols", [ IsRing, IsDenseList, IsMatrix ] );
DeclareOperation( "MatrixByCols", [ IsRing, IsDenseList, IsDenseList ] );

#! @Arguments R, m, n
#! @Returns <Ref Filt="IsQPAMatrix"/>
#! @Description
#!  Construct an $<A>m</A> \times <A>n</A>$ zero matrix over the ring <A>R</A>.
DeclareOperation( "MakeZeroMatrix", [ IsRing, IsInt, IsInt ] );

#! @Arguments R, m, n
#! @Returns <Ref Filt="IsQPAMatrix"/>
#! @Description
#!  Construct an $<A>m</A> \times <A>n</A>$ identity matrix over the ring <A>R</A>.
DeclareOperation( "IdentityMatrix", [ IsRing, IsInt ] );

#! @Arguments M
#! @Description
#!  Returns <C>true</C> if <A>M</A> is an identity matrix, <C>false</C> otherwise.
DeclareProperty( "IsIdentityMatrix", IsQPAMatrix );

#! @Arguments M
#! @Description
#!  Returns <C>true</C> if <A>M</A> is a zero matrix, <C>false</C> otherwise.
DeclareProperty( "IsZeroMatrix", IsQPAMatrix );

#! @Arguments M
#! @Returns list of lists of ring elements
#! @Description
#!  Returns the rows of the matrix <A>M</A>, as a list of lists.
DeclareOperation( "RowsOfMatrix", [ IsQPAMatrix ] );

#! @Arguments M
#! @Returns list of lists of ring elements
#! @Description
#!  Returns the columns of the matrix <A>M</A>, as a list of lists.
DeclareOperation( "ColsOfMatrix", [ IsQPAMatrix ] );

#! @Arguments M
#! @Description
#!  Returns a matrix, which rows are a basis for the nullspace of the matrix
#!  <A>M</A>. 
#! @Returns IsQPAMatrix
DeclareAttribute( "NullspaceMat", IsQPAMatrix );

