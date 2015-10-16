DeclareCategory( "IsVectorSpaceCategory", IsCapCategory );

DeclareOperation( "MakeQPAVectorSpaceCategory", [ IsString, IsField ] );
DeclareAttribute( "CategoryOfRowSpaces", IsField );
DeclareAttribute( "CategoryOfColSpaces", IsField );

DeclareCategory( "IsQPAVectorSpace", IsVectorSpace and IsCapCategoryObject );
DeclareCategory( "IsRowVectorSpace", IsQPAVectorSpace );
DeclareCategory( "IsColVectorSpace", IsQPAVectorSpace );

DeclareCategory( "IsQPAVector", IsVector );
DeclareCategory( "IsColVector", IsVector );

DeclareCategory( "IsLinearTransformation",
                 IsMatrixObj and IsCapCategoryMorphism and IsVectorSpaceHomomorphism
                 and IsMapping );
DeclareCategory( "IsLinearTransformationOfRowSpaces", IsLinearTransformation );
DeclareCategory( "IsLinearTransformationOfColSpaces", IsLinearTransformation );

DeclareOperation( "MakeQPAVector", [ IsString, IsField, IsDenseList ] );
DeclareOperation( "RowVector", [ IsField, IsDenseList ] );
DeclareOperation( "ColVector", [ IsField, IsDenseList ] );
DeclareOperation( "MakeQPAVectorSpace", [ IsString, IsField, IsInt ] );
DeclareOperation( "RowVectorSpace", [ IsField, IsInt ] );
DeclareOperation( "ColVectorSpace", [ IsField, IsInt ] );
DeclareOperation( "MakeLinearTransformation", [ IsString, IsField, IsMatrix ] );
DeclareOperation( "LinearTransformationOfRowSpaces", [ IsField, IsMatrix ] );
DeclareOperation( "LinearTransformationOfColSpaces", [ IsField, IsMatrix ] );

DeclareAttribute( "VectorSpaceConstructor", IsVectorSpaceCategory );
DeclareAttribute( "LinearTransformationConstructor", IsVectorSpaceCategory );
DeclareOperation( "VectorSpaceInCategory", [ IsVectorSpaceCategory, IsInt ] );
DeclareOperation( "LinearTransformationInCategory", [ IsVectorSpaceCategory, IsMatrix ] );

DeclareAttribute( "Length", IsQPAVector );
DeclareOperation( "\[\]", [ IsQPAVector, IsPosInt ] );
DeclareAttribute( "SpaceContainingVector", IsQPAVector );
DeclareAttribute( "MatrixOfLinearTransformation", IsLinearTransformation );

DeclareOperation( "StackMatricesHorizontally", [ IsDenseList ] );
DeclareOperation( "StackMatricesHorizontally", [ IsMatrix, IsMatrix ] );
DeclareOperation( "StackMatricesVertically", [ IsDenseList ] );
DeclareOperation( "StackMatricesVertically", [ IsMatrix, IsMatrix ] );
