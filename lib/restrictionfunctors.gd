#! @Chapter Functors

#! @Section Restriction functors

#! @Description
#!  Returns the restriction functor from the category <A>C</A> to the category <A>D</A>
#!  when <A>f</A> is an algebra homomorphism from the algebra defining the category <A>D</A>
#!  to the algebra defining the category <A>C</A>.
#! @Returns IsCapFunctor 
#! @Arguments f, C, D
DeclareOperation( "RestrictionFunctor", [ IsQuiverAlgebraHomomorphism, IsCapCategory, IsCapCategory ] );

#! @Description
#!  Returns the restriction functor from the category <A>C</A> to the category <A>D</A>
#!  when <A>m</A> is a quiver homomorphism from the quiver defining the category <A>D</A>
#!  to the quiver defining the category <A>C</A>.
#! @Returns IsCapFunctor 
#! @Arguments m, C, D
DeclareOperation( "RestrictionFunctor", [ IsQuiverHomomorphism,
                                          IsQuiverRepresentationCategory, IsQuiverRepresentationCategory ] );

#!
DeclareOperation( "RestrictQuiverRepresentation",
                  [ IsQuiverRepresentation, IsQuiverAlgebraHomomorphism ] );

#!
DeclareOperation( "RestrictQuiverRepresentationElement",
                  [ IsQuiverRepresentationElement, IsQuiverAlgebraHomomorphism ] );


#! @Description
#!  Returns the restriction functor which forgets the right module structure. If the 
#!  argument is a category of right modules, then it returns the restriction functor
#!  to the category vector spaces. 
#! @Returns IsCapFunctor 
#! @Arguments C
DeclareAttribute( "RestrictionToLeftFunctor", IsQuiverModuleCategory );

#! @Description
#!  Returns the restriction functor which forgets the left module structure. If the 
#!  argument is a category of left modules, then it returns the restriction functor
#!  to the category vector spaces. 
#! @Returns IsCapFunctor 
#! @Arguments C
DeclareAttribute( "RestrictionToRightFunctor", IsQuiverModuleCategory );

#! @Description
#!  Returns the module where the right module structure is forgotten. If the 
#!  argument is a right module, then it returns the restriction to the underlying
#!  vector space. 
#! @Returns IsCapFunctor 
#! @Arguments C
DeclareAttribute( "RestrictionToLeft", IsQuiverModule );

#! @Description
#!  Returns the homomorphism where the right module structure is forgotten. If the 
#!  argument is a homomorphism of right modules, then it returns the restriction 
#!  to the underlying vector spaces. 
#! @Returns IsCapFunctor 
#! @Arguments C
DeclareAttribute( "RestrictionToLeft", IsQuiverModuleHomomorphism );

#! @Description
#!  Returns the module where the left module structure is forgotten. If the 
#!  argument is a left module, then it returns the restriction to the underlying
#!  vector space. 
#! @Returns IsCapFunctor 
#! @Arguments C
DeclareAttribute( "RestrictionToRight", IsQuiverModule );

#! @Description
#!  Returns the homomorphism where the left module structure is forgotten. If the 
#!  argument is a homomorphism of left module, then it returns the restriction 
#!  to the underlying vector space. 
#! @Returns IsCapFunctor 
#! @Arguments C
DeclareAttribute( "RestrictionToRight", IsQuiverModuleHomomorphism );

#! @Description
#!  Returns the functor from the category of left modules <A>C</A>, given by left
#!  <M>A</M>-modules, to the category of bimodules over <M>A</M>\otimes <M>k</M>, 
#!  where <M>A</M> is a <M>k</M>-algebra.
#! @Returns IsCapFunctor 
#! @Arguments C
DeclareAttribute( "LeftModuleToBimoduleFunctor", IsLeftQuiverModuleCategory );

#! @Description
#!  Returns the bimodule naturally associated to a left module <A>M</A>.  If <Arg>M</Arg> 
#!  is a left <M>A</M>-module, then the result is a bimodule over <M>A</M>\otimes <M>k</M>, 
#!  where <M>A</M> is a <M>k</M>-algebra.
#! @Returns IsQuiverBimodule 
#! @Arguments M
DeclareAttribute( "LeftModuleToBimodule", IsLeftQuiverModule );

#! @Description
#!  Returns the functor from the category of right modules <A>C</A>, given by right
#!  <M>A</M>-modules, to the category of bimodules over <M>k</M>\otimes <M>A</M>, 
#!  where <M>A</M> is a <M>k</M>-algebra.
#! @Returns IsCapFunctor 
#! @Arguments C
DeclareAttribute( "RightModuleToBimoduleFunctor", IsRightQuiverModuleCategory );

#! @Description
#!  Returns the bimodule naturally associated to a right module <A>M</A>.  If <Arg>M</Arg> 
#!  is a right <M>A</M>-module, then the result is a bimodule over <M>k</M>\otimes <M>A</M>, 
#!  where <M>A</M> is a <M>k</M>-algebra.
#! @Returns IsQuiverBimodule 
#! @Arguments M
DeclareAttribute( "RightModuleToBimodule", IsRightQuiverModule );

#! @Description
#!  Returns the functor from the quiver representation category <A>C</A> to the category
#!  of vector spaces.  
#! @Returns IsCapFunctor 
#! @Arguments C
DeclareAttribute( "AsVectorSpaceFunctor", IsQuiverRepresentationCategory ); 

#! @Description
#!  Returns the functor from the quiver module category <A>C</A> to the category of vector
#!  spaces.  
#! @Returns IsCapFunctor 
#! @Arguments C
DeclareAttribute( "AsVectorSpaceFunctor", IsQuiverModuleCategory ); 

DeclareAttribute( "TensorFlipRestrictionFunctor", IsQuiverRepresentationCategory );

DeclareAttribute( "TensorFlipRestriction", IsQuiverRepresentation );

#