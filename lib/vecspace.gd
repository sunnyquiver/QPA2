#! @Chapter Vector spaces

#! @Section Vector spaces

#! @Description
#!  Category for (CAP) categories of vector spaces.
DeclareCategory( "IsVectorSpaceCategory", IsCapCategory );

#! @Description
#!  Category for vectors.  Subcategory of the builtin GAP category <C>IsVector</C>.
DeclareCategory( "IsQPAVector", IsVector );

#! @Description
#!  Category for row vectors.
DeclareCategory( "IsQPARowVector", IsQPAVector );

#! @Description
#!  Category for column vectors.
DeclareCategory( "IsQPAColVector", IsQPAVector );

DeclareCategory( "IsEmptyVector", IsQPARowVector and IsQPAColVector );

#! @Description
#!  Category for vector spaces.
#!  Subcategory of the builtin GAP category <C>IsVector</C>,
#!  and of the category <C>IsCapCategoryObject</C>.
DeclareCategory( "IsQPAVectorSpace", IsVectorSpace and IsCapCategoryObject );

#! @Description
#!  Category for row vector spaces.
DeclareCategory( "IsRowVectorSpace", IsQPAVectorSpace );

#! @Description
#!  Category for column vector spaces.
DeclareCategory( "IsColVectorSpace", IsQPAVectorSpace );

#! @Description
#!  Category for zero vector spaces.
DeclareCategory( "IsZeroVectorSpace", IsRowVectorSpace and IsColVectorSpace );

#! @Description
#!  Category for linear transformations.
DeclareCategory( "IsLinearTransformation",
                 IsCapCategoryMorphism and IsVectorSpaceHomomorphism
                 and IsMapping );

#! @Arguments k
#! @Description
#!  Returns the category of vector spaces over the field <A>k</A>.
DeclareAttribute( "CategoryOfVectorSpaces", IsField );

DeclareOperation( "MakeQPAVector", [ IsString, IsField, IsDenseList ] );

#! @Arguments k, L
#! @Returns <Ref Filt="IsQPARowVector"/>
#! @Description
#!  Produces a row vector over the field <A>k</A> containing the elements
#!  in the list <A>L</A> (these elements must lie in the field <A>k</A>).
DeclareOperation( "RowVector", [ IsField, IsDenseList ] );

#! @Arguments k, L
#! @Returns <Ref Filt="IsQPAColVector"/>
#! @Description
#!  Produces a column vector over the field <A>k</A> containing the elements
#!  in the list <A>L</A> (these elements must lie in the field <A>k</A>).
DeclareOperation( "ColVector", [ IsField, IsDenseList ] );

#! @Arguments V, coeffs
#! @Returns <Ref Filt="IsQPAVector"/>
#! @Description
#!  Produces a vector in the vector space <A>V</A> with coefficients <A>coeffs</A>
#!  with respect to the canonical basis of <A>V</A>.
#!  <P/>
#!  The argument <A>coeffs</A> can be either a list,
#!  a row vector (<Ref Filt="IsQPARowVector"/>)
#!  or a column vector (<Ref Filt="IsQPAColVector"/>).
DeclareOperation( "Vector", [ IsQPAVectorSpace, IsList ] );

#DeclareOperation( "Vector", [ IsQPAVectorSpace, IsList, IsBasis ] );
DeclareOperation( "Vector", [ IsQPAVectorSpace, IsQPARowVector ] );
#DeclareOperation( "Vector", [ IsQPAVectorSpace, IsQPARowVector, IsBasis ] );
DeclareOperation( "Vector", [ IsQPAVectorSpace, IsQPAColVector ] );
#DeclareOperation( "Vector", [ IsQPAVectorSpace, IsQPAColVector, IsBasis ] );

#! @Arguments v
#! @Returns list of field elements
#! @Description
#!  Returns the coefficients of the vector <A>v</A> (with respect to the
#!  canonical basis of the vector space it belongs to) as a list.
DeclareAttribute( "AsList", IsQPAVector );

#! @Arguments v
#! @Returns <Ref Filt="IsQPARowVector"/>
#! @Description
#!  Returns the coefficients of the vector <A>v</A> (with respect to the
#!  canonical basis of the vector space it belongs to) as a row vector.
DeclareOperation( "AsRowVector", [ IsQPAVector ] );

#! @Arguments v
#! @Returns <Ref Filt="IsQPAColVector"/>
#! @Description
#!  Returns the coefficients of the vector <A>v</A> (with respect to the
#!  canonical basis of the vector space it belongs to) as a column vector.
DeclareOperation( "AsColVector", [ IsQPAVector ] );

#! @Arguments v
#! @Returns nonnegative integer
#! @Description
#!  Returns the dimension of the vector space to which the vector <A>v</A> belongs.
#!  If the vector is a row or column vector, this number is the length of the vector.
DeclareAttribute( "Length", IsQPAVector );

#! @Arguments v, i
#! @Returns field element
#! @Description
#!  Returns the <A>i</A>-th element of
#!  <C>AsList( <A>v</A> )</C>.
#!  If <A>v</A> is a row or column vector, this is the same as the
#!  <A>i</A>-th element of <A>v</A>.
DeclareOperation( "\[\]", [ IsQPAVector, IsPosInt ] );

#! @Arguments v
#! @Returns <Ref Filt="IsQPAVectorSpace"/>
#! @Description
#!  Returns the vector space containing the vector <A>v</A>.
DeclareAttribute( "SpaceContainingVector", IsQPAVector );

#! @Arguments k, d
#! @Returns <Ref Filt="IsQPAVectorSpace"/>
#! @Description
#!  Constructs the row vector space of dimension <A>d</A>
#!  over the field <A>k</A>.
DeclareOperation( "RowVectorSpace", [ IsField, IsInt ] );

#! @Arguments k, d
#! @Returns <Ref Filt="IsQPAVectorSpace"/>
#! @Description
#!  Constructs the column vector space of dimension <A>d</A>
#!  over the field <A>k</A>.
DeclareOperation( "ColVectorSpace", [ IsField, IsInt ] );

DeclareOperation( "MakeQPAVectorSpace", [ IsString, IsField, IsInt ] );
DeclareOperation( "MakeQPAVectorSpace", [ IsQPAVectorSpace, IsInt ] );

#! @Arguments k
#! @Returns <Ref Filt="IsZeroVectorSpace"/>
#! @Description
#!  Returns the zero vector space over the field <A>k</A>.
#!  The zero vector space contains a single element, the empty vector.
#!  The zero vector space is regarded as both a row space and a column space.
DeclareAttribute( "ZeroVectorSpace", IsField );

#! @Arguments k
#! @Returns <Ref Filt="IsEmptyVector"/>
#! @Description
#!  Returns the empty vector over the field <A>k</A>.
#!  This is the only element of the zero vector space.
#!  The empty vector is regarded as both a row vector and a column vector.
DeclareAttribute( "EmptyVector", IsField );


DeclareOperation( "MakeLinearTransformation",
                  [ IsQPAVectorSpace, IsQPAVectorSpace, IsQPAMatrix, IsQPAMatrix ] );

DeclareOperation( "LinearTransformationByLeftMatrix",
                  [ IsQPAVectorSpace, IsQPAVectorSpace, IsQPAMatrix ] );

DeclareOperation( "LinearTransformationByRightMatrix",
                  [ IsQPAVectorSpace, IsQPAVectorSpace, IsQPAMatrix ] );

#! @Arguments M
#! @Returns <Ref Filt="IsLinearTransformation"/>
#! @Description
#!  Returns the linear transformation between row vector spaces
#!  represented by the matrix <A>M</A>, which must be an
#!  <Ref Filt="IsQPAMatrix"/> over a field $k$.
#!  If <A>M</A> is an $m \times n$ matrix, the result is a linear
#!  transformation from the $m$-dimensional row space to the
#!  $n$-dimensional row space over $k$.
DeclareOperation( "LinearTransformationOfRowSpaces", [ IsQPAMatrix ] );

#! @Arguments M
#! @Returns <Ref Filt="IsLinearTransformation"/>
#! @Description
#!  Returns the linear transformation between row vector spaces
#!  represented by the matrix <A>M</A>, which must be an
#!  <Ref Filt="IsQPAMatrix"/> over a field $k$.
#!  If <A>M</A> is an $m \times n$ matrix, the result is a linear
#!  transformation from the $n$-dimensional row space to the
#!  $m$-dimensional row space over $k$.
DeclareOperation( "LinearTransformationOfColSpaces", [ IsQPAMatrix ] );

DeclareAttribute( "MatrixOfLinearTransformation", IsLinearTransformation );
DeclareAttribute( "LeftMatrixOfLinearTransformation", IsLinearTransformation );
DeclareAttribute( "RightMatrixOfLinearTransformation", IsLinearTransformation );
DeclareOperation( "LeftMatrixOfLinearTransformation", [ IsLinearTransformation, IsBasis, IsBasis ] );
DeclareOperation( "RightMatrixOfLinearTransformation", [ IsLinearTransformation, IsBasis, IsBasis ] );

DeclareOperation( "StackMatricesHorizontally", [ IsDenseList ] );
DeclareOperation( "StackMatricesHorizontally", [ IsQPAMatrix, IsQPAMatrix ] );
DeclareOperation( "StackMatricesVertically", [ IsDenseList ] );
DeclareOperation( "StackMatricesVertically", [ IsQPAMatrix, IsQPAMatrix ] );

DeclareOperation( "SubspaceInclusion", [ IsQPAVectorSpace, IsHomogeneousList ] );
#! @Arguments M, v
#! @Returns a row vector w
#! @Description
#!  Returns a row vector such that  w*<A>M</A> = <A>v</A>, whenever possible or fail if there
#!  is no solution.  Signals an error message if the vector <A>v</A> and matrix <A>M</A> are
#!  not compatible, that is, number of columns in the matrix <A>M</A> differ from the number 
#!  of enteries in the vector <A>v</A>. 
DeclareOperation( "SolutionMat", [ IsQPAMatrix, IsQPARowVector ] );