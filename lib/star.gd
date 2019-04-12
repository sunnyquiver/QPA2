#! @Chapter Functors

#! @Section The star functor

#! @Description
#!  This function computes the star of a module <Arg>M</Arg> for an algebra <M>A</M>, 
#!  that is, <M>\Hom_A( M, A )</M>.
#! @Returns <Ref Filt="IsQuiverModule"/>
#! @Arguments M
DeclareAttribute( "StarOfModule", IsQuiverModule );

#! @Description
#!  This function computes the star of a homomorphism between two modules <Arg>f</Arg> for an algebra <M>A</M>, 
#!  that is, <M>\Hom_A( f, A )</M>.
#! @Returns <Ref Filt="IsQuiverModuleHomomorphism"/>
#! @Arguments f
DeclareAttribute( "StarOfModuleHomomorphism", IsQuiverModuleHomomorphism );

#! @Description
#!  This function defines the star functor for a category of modules <Arg>C</Arg> 
#!  for an algebra <M>A</M>, that is, <M>\Hom_A( -, A )\colon \mathcal{C}\to \mathcal{C}^\op</M>.
#! @Returns <C>IsCapFunctor</C>
#! @Arguments C
DeclareOperation( "StarFunctor", [ IsQuiverModuleCategory ] );

#