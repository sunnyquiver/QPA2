DeclareCategory( "IsVectorSpaceCategory", IsCapCategory );

DeclareCategory( "IsQPAVector", IsVector );
DeclareCategory( "IsQPARowVector", IsQPAVector );
DeclareCategory( "IsQPAColVector", IsQPAVector );

DeclareCategory( "IsQPAVectorSpace", IsVectorSpace and IsCapCategoryObject );
DeclareCategory( "IsRowVectorSpace", IsQPAVectorSpace );
DeclareCategory( "IsColVectorSpace", IsQPAVectorSpace );
DeclareCategory( "IsZeroVectorSpace", IsQPAVectorSpace );

DeclareCategory( "IsLinearTransformation",
                 IsCapCategoryMorphism and IsVectorSpaceHomomorphism
                 and IsMapping );

DeclareAttribute( "CategoryOfVectorSpaces", IsField );

DeclareOperation( "MakeQPAVector", [ IsString, IsField, IsDenseList ] );
DeclareOperation( "RowVector", [ IsField, IsDenseList ] );
DeclareOperation( "ColVector", [ IsField, IsDenseList ] );
DeclareOperation( "Vector", [ IsQPAVectorSpace, IsList ] );
#DeclareOperation( "Vector", [ IsQPAVectorSpace, IsList, IsBasis ] );
DeclareOperation( "Vector", [ IsQPAVectorSpace, IsQPARowVector ] );
#DeclareOperation( "Vector", [ IsQPAVectorSpace, IsQPARowVector, IsBasis ] );
DeclareOperation( "Vector", [ IsQPAVectorSpace, IsQPAColVector ] );
#DeclareOperation( "Vector", [ IsQPAVectorSpace, IsQPAColVector, IsBasis ] );
DeclareOperation( "AsRowVector", [ IsQPAVector ] );
DeclareOperation( "AsColVector", [ IsQPAVector ] );
DeclareAttribute( "AsList", IsQPAVector );

DeclareAttribute( "Length", IsQPAVector );
DeclareOperation( "\[\]", [ IsQPAVector, IsPosInt ] );
DeclareAttribute( "SpaceContainingVector", IsQPAVector );

DeclareOperation( "MakeQPAVectorSpace", [ IsString, IsField, IsInt ] );
DeclareOperation( "MakeQPAVectorSpace", [ IsQPAVectorSpace, IsInt ] );
DeclareAttribute( "ZeroVectorSpace", IsField );
DeclareAttribute( "ZeroVector", IsField );
DeclareOperation( "RowVectorSpace", [ IsField, IsInt ] );
DeclareOperation( "ColVectorSpace", [ IsField, IsInt ] );
DeclareOperation( "MakeLinearTransformation",
                  [ IsQPAVectorSpace, IsQPAVectorSpace, IsQPAMatrix, IsQPAMatrix ] );
DeclareOperation( "LinearTransformationByLeftMatrix",
                  [ IsQPAVectorSpace, IsQPAVectorSpace, IsQPAMatrix ] );
DeclareOperation( "LinearTransformationByRightMatrix",
                  [ IsQPAVectorSpace, IsQPAVectorSpace, IsQPAMatrix ] );
DeclareOperation( "LinearTransformationOfRowSpaces", [ IsQPAMatrix ] );
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