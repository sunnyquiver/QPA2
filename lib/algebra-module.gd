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

DeclareAttribute( "AsAlgebraElementAttr", IsQuiverRepresentationElement );
DeclareOperation( "AsAlgebraElement", [ IsQuiverRepresentationElement ] );
DeclareAttribute( "AsAlgebraElementEnv", IsQuiverRepresentationElement );

#! @Description
#!  Returns the algebra <Arg>A</Arg> as a left module over itself, when the algebra is
#!  finite dimensional quiver algebra.
#! @Returns <Ref Filt="IsLeftQuiverModule"/>
#! @Arguments A 
DeclareAttribute( "AlgebraAsLeftModule", IsQuiverAlgebra );

#! @Description
#!  Returns the algebra <Arg>A</Arg> as a right module over itself, when the algebra is
#!  finite dimensional quiver algebra.
#! @Returns <Ref Filt="IsRightQuiverModule"/>
#! @Arguments A 
DeclareAttribute( "AlgebraAsRightModule", IsQuiverAlgebra );

#! @Description
#!  Returns the algebra <Arg>A</Arg> as a bimodule over itself, when the algebra is
#!  finite dimensional quiver algebra.
#! @Returns <Ref Filt="IsQuiverBimodule"/>
#! @Arguments A 
DeclareAttribute( "AlgebraAsBimodule", IsQuiverAlgebra );

#! @Description
#!  Returns the algebra <Arg>A</Arg> as a left module, right module or bimodule 
#!  over itself depending on the side argument <Arg>side</Arg>, when the algebra 
#!  is finite dimensional quiver algebra.
#! @Returns <Ref Filt="IsQuiverBimodule"/>
#! @Arguments side, A 
DeclareOperation( "AlgebraAsModule", [ IsSide, IsQuiverAlgebra ] );

#! @Description
#!  Returns the dual of the algebra <Arg>A</Arg> as a left module over itself, 
#!  when the algebra is finite dimensional quiver algebra.
#! @Returns <Ref Filt="IsLeftQuiverModule"/>
#! @Arguments A 
DeclareAttribute( "DualOfAlgebraAsLeftModule", IsQuiverAlgebra );

#! @Description
#!  Returns the dual of the algebra <Arg>A</Arg> as a right module over itself, 
#!  when the algebra is finite dimensional quiver algebra.
#! @Returns <Ref Filt="IsRightQuiverModule"/>
#! @Arguments A 
DeclareAttribute( "DualOfAlgebraAsRightModule", IsQuiverAlgebra );

#! @Description
#!  Returns the dual of the algebra <Arg>A</Arg> as a bimodule over itself, 
#!  when the algebra is finite dimensional quiver algebra.
#! @Returns <Ref Filt="IsQuiverBimodule"/>
#! @Arguments A 
DeclareAttribute( "DualOfAlgebraAsBimodule", IsQuiverAlgebra );

#! @Description
#!  Returns the dual of the algebra <Arg>A</Arg> as a left module, right module 
#!  or bimodule over itself depending on the side argument <Arg>side</Arg>, 
#!  when the algebra is finite dimensional quiver algebra.
#! @Returns <Ref Filt="IsQuiverBimodule"/>
#! @Arguments side, A
DeclareOperation( "DualOfAlgebraAsModule", [ IsSide, IsQuiverAlgebra ] );

DeclareOperation( "AsModuleElement", [ IsSide, IsQuiverAlgebraElement ] );
DeclareAttribute( "AsLeftModuleElement", IsQuiverAlgebraElement );
DeclareAttribute( "AsRightModuleElement", IsQuiverAlgebraElement );
DeclareAttribute( "AsBimoduleElement", IsQuiverAlgebraElement );

DeclareAttribute( "AsAlgebraElementAttr", IsQuiverModuleElement );
DeclareOperation( "AsAlgebraElement", [ IsQuiverModuleElement ] );

#