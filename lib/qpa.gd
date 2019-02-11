DeclareCategory( "IsQPAElement", IsRingElementWithOne );

DeclareAttribute( "Coefficients", IsQPAElement );
DeclareAttribute( "Paths", IsQPAElement );
DeclareGlobalFunction( "QPAElement" );

DeclareCategory( "IsQPA", IsAlgebraWithOne and CategoryCollections( IsQPAElement ) );
DeclareCategory( "IsPathAlgebra", IsQPA );

DeclareAttribute( "QuiverOfAlgebra", IsQPA );
DeclareAttribute( "RelationsOfAlgebra", IsQPA );
# field of a QPA: LeftActingDomain
DeclareAttribute( "PathAlgebra", IsQPA );

DeclareGlobalFunction( "QPA" );
#DeclareGlobalFunction( "PathAlgebra" );

DeclareOperation( "DivideByList", [ IsQPAElement, IsList ] );

#