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

#! @Section The Nakayama functor

#! @Description
#!  This function computes the Nakayama functor from a quiver module category <Arg>C</Arg>.  
#! @Returns <Ref Filt="IsQuiverModuleCategory"/>
#! @Arguments C
DeclareOperation( "NakayamaFunctor", [ IsQuiverModuleCategory ] );

#! @Description
#!  This function computes the Nakayama functor applied to a module <Arg>M</Arg>.  
#! @Returns <Ref Filt="IsQuiverModule"/>
#! @Arguments M
DeclareAttribute( "NakayamaFunctorOfModule", IsQuiverModule );

#! @Description
#!  This function computes the Nakayama functor applied to a module homomorphism 
#!  <Arg>f</Arg>.  
#! @Returns <Ref Filt="IsQuiverModuleHomomorphism"/>
#! @Arguments f
DeclareAttribute( "NakayamaFunctorOfModuleHomomorphism", IsQuiverModuleHomomorphism );


#