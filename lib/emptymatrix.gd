#! @Chapter Utilities

#! @Section Empty matrices

#! @Description
#!  Category for empty matrices
DeclareCategory( "IsEmptyMatrix", IsMatrix );

#! @Description
#!  Create an empty <A>m</A> by <A>n</A> matrix over the ring <A>R</A>.
#!  The arguments <A>m</A> and <A>n</A> are nonnegative integers,
#!  and at least one of them must be zero.
#! @Returns <Ref Filt="IsEmptyMatrix"/>
#! @Arguments m, n, R
DeclareOperation( "MakeEmptyMatrix", [ IsInt, IsInt, IsRing ] );

#! @Description
#!  Returns the base ring of the empty matrix <A>M</A>.
#! @Returns <Ref BookName="Reference" Filt="IsRing"/>
#! @Arguments M
DeclareAttribute( "RingOfEmptyMatrix", IsEmptyMatrix );

#! @Description
#!  Create an <A>m</A> by <A>n</A> zero matrix over the ring <A>R</A>.
#!  The arguments <A>m</A> and <A>n</A> are nonnegative integers.
#!  If at least one of them is zero, then an empty matrix is constructed.
#!  Otherwise, a normal GAP matrix filled with zeros is constructed,
#!  as by <Ref BookName="Reference" Oper="NullMat"/>.
#! @Returns <Ref Filt="IsEmptyMatrix"/>
#! @Arguments m, n, R
DeclareOperation( "MakeZeroMatrix", [ IsInt, IsInt, IsRing ] );

DeclareOperation( "IdentityMatrix", [ IsInt, IsRing ] );

DeclareOperation( "\*", [ IsEmptyMatrix, IsList and IsEmpty ] );
DeclareOperation( "\*", [ IsList and IsEmpty, IsEmptyMatrix ] );

#! @InsertChunk Example_EmptyMatrix

#
