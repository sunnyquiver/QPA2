# algebras considered as representations/modules

DeclareOperation( "QuiverRepresentationFromBasis",
                  [ IsQuiverAlgebra, IsDenseList, IsDenseList, IsFunction ] );
DeclareOperation( "QuiverRepresentationFromBasisNC",
                  [ IsQuiverAlgebra, IsDenseList, IsDenseList, IsFunction ] );

DeclareAttribute( "AlgebraAsRepresentation", IsQuiverAlgebra );
DeclareAttribute( "AlgebraAsRepresentationEnv", IsQuiverAlgebra );

DeclareAttribute( "BasisPermutationToRepresentation", IsQuiverAlgebra );
DeclareAttribute( "BasisPermutationFromRepresentation", IsQuiverAlgebra );
DeclareAttribute( "BasisPermutationToRepresentationEnv", IsQuiverAlgebra );
DeclareAttribute( "BasisPermutationFromRepresentationEnv", IsQuiverAlgebra );

DeclareAttribute( "AsRepresentationElement", IsQuiverAlgebraElement );
DeclareAttribute( "AsRepresentationElementEnv", IsQuiverAlgebraElement );

DeclareAttribute( "AsAlgebraElement", IsQuiverRepresentationElement );
DeclareAttribute( "AsAlgebraElementEnv", IsQuiverRepresentationElement );

DeclareAttribute( "AlgebraAsLeftModule", IsQuiverAlgebra );
DeclareAttribute( "AlgebraAsRightModule", IsQuiverAlgebra );
DeclareAttribute( "AlgebraAsBimodule", IsQuiverAlgebra );
DeclareOperation( "AlgebraAsModule", [ IsSide, IsQuiverAlgebra ] );

DeclareOperation( "AsModuleElement", [ IsSide, IsQuiverAlgebraElement ] );
DeclareAttribute( "AsLeftModuleElement", IsQuiverAlgebraElement );
DeclareAttribute( "AsRightModuleElement", IsQuiverAlgebraElement );
DeclareAttribute( "AsBimoduleElement", IsQuiverAlgebraElement );

DeclareAttribute( "AsAlgebraElement", IsQuiverModuleElement );
