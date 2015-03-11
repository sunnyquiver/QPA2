# Quiver algebras
DeclareCategory( "IsQuiverAlgebraElement", IsRingElementWithOne );
DeclareCategory( "IsQuiverAlgebra", IsAlgebraWithOne and CategoryCollections( IsQuiverAlgebraElement ) );
DeclareAttribute( "QuiverOfAlgebra", IsQuiverAlgebra );
DeclareAttribute( "RelationsOfAlgebra", IsQuiverAlgebra );
# field of a QuiverAlgebra: LeftActingDomain
DeclareAttribute( "PathAlgebra", IsQuiverAlgebra );
DeclareOperation( "\[\]", [ IsQuiverAlgebra, IsInt ] );

# Elements of quiver algebras
DeclareAttribute( "Coefficients", IsQuiverAlgebraElement );
DeclareAttribute( "Paths", IsQuiverAlgebraElement );
DeclareAttribute( "AlgebraOfElement", IsQuiverAlgebraElement );

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
DeclareOperation( "OverlapRelation",
                  [ IsPathAlgebraElement, IsPathAlgebraElement,
                    IsPath, IsPath ] );
DeclareOperation( "OverlapRelations",
                  [ IsPathAlgebraElement, IsPathAlgebraElement ] );
DeclareOperation( "ComputeGroebnerBasis", [ IsList ] );

# Quotients of path algebras
DeclareCategory( "IsQuotientOfPathAlgebra", IsQuiverAlgebra );
DeclareGlobalFunction( "QuotientOfPathAlgebra" );

# Elements of quotients
DeclareCategory( "IsQuotientOfPathAlgebraElement", IsQuiverAlgebraElement );
DeclareGlobalFunction( "QuotientOfPathAlgebraElement" );
DeclareAttribute( "Representative", IsQuotientOfPathAlgebraElement );
