DeclareOperation( "MakeQPAVectorSpaceCategory", [ IsString, IsField ] );
DeclareAttribute( "RowSpaceCategory", IsField );
DeclareAttribute( "ColSpaceCategory", IsField );

DeclareCategory( "IsQPAVectorSpace", IsVectorSpace and IsCapCategoryObject );
DeclareCategory( "IsRowVectorSpace", IsQPAVectorSpace );
DeclareCategory( "IsColVectorSpace", IsQPAVectorSpace );

DeclareCategory( "IsQPAVector", IsVector );
DeclareCategory( "IsColVector", IsVector );

DeclareCategory( "IsQPAMatrix",
                 IsMatrixObj and IsCapCategoryMorphism and IsVectorSpaceHomomorphism
                 and IsMapping );
DeclareCategory( "IsRowMatrix", IsQPAMatrix );
DeclareCategory( "IsColMatrix", IsQPAMatrix );

DeclareOperation( "MakeQPAVector", [ IsString, IsField, IsDenseList ] );
DeclareOperation( "RowVector", [ IsField, IsDenseList ] );
DeclareOperation( "ColVector", [ IsField, IsDenseList ] );
DeclareOperation( "MakeQPAVectorSpace", [ IsString, IsField, IsInt ] );
DeclareOperation( "RowVectorSpace", [ IsField, IsInt ] );
DeclareOperation( "ColVectorSpace", [ IsField, IsInt ] );
DeclareOperation( "MakeQPAMatrix", [ IsString, IsField, IsMatrix ] );
DeclareOperation( "RowMatrix", [ IsField, IsMatrix ] );
DeclareOperation( "ColMatrix", [ IsField, IsMatrix ] );

DeclareOperation( "\[\]", [ IsQPAVector, IsPosInt ] );
DeclareAttribute( "SpaceContainingVector", IsQPAVector );

DeclareOperation( "StackMatricesHorizontally", [ IsDenseList ] );
DeclareOperation( "StackMatricesHorizontally", [ IsMatrix, IsMatrix ] );
DeclareOperation( "StackMatricesVertically", [ IsDenseList ] );
DeclareOperation( "StackMatricesVertically", [ IsMatrix, IsMatrix ] );
