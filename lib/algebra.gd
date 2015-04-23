#! @Chapter Path algebras

#! @Section Quiver algebras

#! We use the term **quiver algebra** for an algebra that is
#! either a path algebra or a quotient of a path algebra by
#! some ideal.
#! This section describes operations that are defined for
#! quiver algebras.

#! @Description
#!  The category for elements of quiver algebras.
DeclareCategory( "IsQuiverAlgebraElement", IsRingElementWithOne );

#! @Description
#!  The category for quiver algebras.
DeclareCategory( "IsQuiverAlgebra", IsAlgebraWithOne and CategoryCollections( IsQuiverAlgebraElement ) );

#! @Arguments A
#! @Returns <Ref Filt="IsQuiver"/>
#! @Description
#!  Returns the quiver Q of the quiver algebra <A>A</A> = kQ/I.
DeclareAttribute( "QuiverOfAlgebra", IsQuiverAlgebra );

#! @Arguments A
#! @Returns list of <Ref Filt="IsPathAlgebraElement"/>
#! @Description
#!  Returns a list of relations for the quiver algebra <A>A</A>.
#!  That is, if $<A>A</A> = kQ/I$, this operation returns a list
#!  of generators for the ideal $I$.
#!  If <A>A</A> is a path algebra, this operation returns the empty list.
DeclareAttribute( "RelationsOfAlgebra", IsQuiverAlgebra );

# field of a QuiverAlgebra: LeftActingDomain

#! @Arguments A
#! @Returns <Ref Filt="IsPathAlgebra"/>
#! @Description
#!  Given a quiver algebra <A>A</A> = kQ/I, this operation returns
#!  the path algebra kQ.
DeclareAttribute( "PathAlgebra", IsQuiverAlgebra );

#! @BeginGroup AlgebraElementByLabel
#! @Description
#!  Returns the primitive path (vertex or arrow) with label <A>label</A>,
#!  as an element of the quiver algebra <A>A</A>.
#!  If no such path exists, then <C>fail</C> is returned.
#!  The operation <C><A>A</A>[ <A>label</A> ]</C> is equivalent to
#!  <C>AlgebraElementByLabel( <A>A</A>, <A>label</A> )</C>.
#! @Returns <Ref Filt="IsQuiverAlgebraElement"/> or <C>fail</C>
#! @Arguments A, label
DeclareOperation( "AlgebraElementByLabel", [ IsQuiverAlgebra, IsObject ] );
#! @Arguments A, label
DeclareOperation( "\[\]", [ IsQuiverAlgebra, IsObject ] );
#! @EndGroup

#! @Arguments A, string
#! @Description
#!  Returns the path described by the string <A>string</A>
#!  (see <Ref Oper="PathFromString"/>)
#!  as an element of the quiver algebra <A>A</A>.
#!  If no such path exists, then <C>fail</C> is returned.
#!  <P/>
#!  This operation can also be called by writing <C><A>A</A>.str</C>,
#!  where <C>str</C> is an unquoted string literal.
DeclareOperation( "AlgebraElementFromString", [ IsQuiverAlgebra, IsString ] );

# Elements of quiver algebras
DeclareAttribute( "Coefficients", IsQuiverAlgebraElement );
DeclareAttribute( "Paths", IsQuiverAlgebraElement );
DeclareAttribute( "AlgebraOfElement", IsQuiverAlgebraElement );
DeclareProperty( "IsUniform", IsQuiverAlgebraElement );

# Path algebras
DeclareCategory( "IsPathAlgebra", IsQuiverAlgebra );
#DeclareGlobalFunction( "PathAlgebra" );

# Elements of path algebras
DeclareCategory( "IsPathAlgebraElement", IsQuiverAlgebraElement );
DeclareGlobalFunction( "PathAlgebraElement" );
DeclareGlobalFunction( "PathAlgebraElementNC" );
DeclareOperation( "PathAsAlgebraElement", [ IsQuiverAlgebra, IsPath ] );
DeclareAttribute( "LeadingPath", IsPathAlgebraElement );
DeclareOperation( "LeadingCoefficient", [ IsPathAlgebraElement ] );
DeclareAttribute( "LeadingTerm", IsPathAlgebraElement );
DeclareAttribute( "NonLeadingTerms", IsPathAlgebraElement );
DeclareOperation( "DivideByList", [ IsPathAlgebraElement, IsList ] );
DeclareOperation( "Reduce", [ IsPathAlgebraElement, IsList ] );
DeclareOperation( "OverlapRelation",
                  [ IsPathAlgebraElement, IsPathAlgebraElement,
                    IsPath, IsPath ] );
DeclareOperation( "OverlapRelations",
                  [ IsPathAlgebraElement, IsPathAlgebraElement ] );
DeclareOperation( "TipReduce", [ IsCollection ] );
DeclareOperation( "ComputeGroebnerBasis", [ IsList ] );

# Ideals
DeclareCategory( "IsPathIdeal", IsRing );
DeclareAttribute( "GroebnerBasis", IsPathIdeal );

# Quotients of path algebras
DeclareCategory( "IsQuotientOfPathAlgebra", IsQuiverAlgebra );
DeclareOperation( "QuotientOfPathAlgebra", [ IsPathAlgebra, IsObject ] );
DeclareOperation( "\/", [ IsPathAlgebra, IsObject ] );
DeclareAttribute( "IdealOfQuotient", IsQuotientOfPathAlgebra );

# Elements of quotients
DeclareCategory( "IsQuotientOfPathAlgebraElement", IsQuiverAlgebraElement );
DeclareGlobalFunction( "QuotientOfPathAlgebraElement" );
DeclareAttribute( "Representative", IsQuotientOfPathAlgebraElement );
