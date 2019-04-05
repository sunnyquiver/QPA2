#! @Chapter Functors

#! @Section Transpose

#! @Description
#!  This function computes the transpose of a module <Arg>M</Arg>.  
#! @Returns <Ref Filt="IsQuiverModule"/>
#! @Arguments M
DeclareAttribute( "TransposeOfModule", IsQuiverModule );

#! @Description
#!  This function computes the transpoe of a homomorphism <Arg>f</Arg> between two modules. 
#! @Returns <Ref Filt="IsQuiverModuleHomomorphism"/>
#! @Arguments f
DeclareAttribute( "TransposeOfModuleHomomorphism", IsQuiverModuleHomomorphism );

#! @Description
#!  This function defines the transpose functor for a category of modules <Arg>C</Arg>. 
#! @Returns <C>IsCapFunctor</C>
#! @Arguments C
DeclareOperation( "TransposeFunctor", [ IsQuiverModuleCategory ] );

#