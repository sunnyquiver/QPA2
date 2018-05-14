DeclareProperty( "IsAdmissibleIdeal", IsPathIdeal );

DeclareCategory( "IsQuiverAlgebraIdeal", IsRing and CategoryCollections( IsQuiverAlgebraElement ) );
DeclareCategory( "IsQuiverAlgebraLeftIdeal", IsQuiverAlgebraIdeal );
DeclareCategory( "IsQuiverAlgebraRightIdeal", IsQuiverAlgebraIdeal );
DeclareCategory( "IsQuiverAlgebraTwoSidedIdeal", IsQuiverAlgebraIdeal );

DeclareOperation( "QuiverAlgebraIdeal", [ IsSide, IsQuiverAlgebra, IsDenseList ] );
DeclareOperation( "LeftIdealByGenerators", [ IsQuiverAlgebra, IsDenseList ] );
DeclareOperation( "RightIdealByGenerators", [ IsQuiverAlgebra, IsDenseList ] );
DeclareOperation( "TwoSidedIdealByGenerators", [ IsQuiverAlgebra, IsDenseList ] );

DeclareAttribute( "Generators", IsQuiverAlgebraIdeal );
DeclareAttribute( "IdealAsModule", IsQuiverAlgebraIdeal );
DeclareAttribute( "IdealAsSubmoduleOfAlgebra", IsQuiverAlgebraIdeal );

DeclareOperation( "IdealElementAsModuleElement", [ IsQuiverAlgebraElement, IsQuiverAlgebraIdeal ] );
DeclareOperation( "ModuleElementAsIdealElement", [ IsQuiverModuleElement, IsQuiverAlgebraIdeal ] );
