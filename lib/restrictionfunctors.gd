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
#!  Returns the restriction functor from the bimodule category <A>C</A>, given by 
#!  <M>A</M>-<M>B</M>-bimodules, to the category of left modules over <M>A</M>. 
#! @Returns IsCapFunctor 
#! @Arguments C
DeclareAttribute( "RestrictionToLeftFunctor", IsQuiverBimoduleCategory );

#! @Description
#!  Returns the restriction functor from the bimodule category <A>C</A>, given by 
#!  <M>A</M>-<M>B</M>-bimodules, to the category of right modules over <M>B</M>. 
#! @Returns IsCapFunctor 
#! @Arguments C
DeclareAttribute( "RestrictionToRightFunctor", IsQuiverBimoduleCategory );

#! @Description
#!  Returns the functor from the category of left modules <A>C</A>, given by left
#!  <M>A</M>-modules, to the category of bimodules over <M>A</M>\otimes <M>k</M>, 
#!  where <M>A</M> is a <M>k</M>-algebra.
#! @Returns IsCapFunctor 
#! @Arguments C
DeclareAttribute( "LeftModuleToBimoduleFunctor", IsLeftQuiverModuleCategory );

#! @Description
#!  Returns the functor from the category of right modules <A>C</A>, given by right
#!  <M>A</M>-modules, to the category of bimodules over <M>k</M>\otimes <M>A</M>, 
#!  where <M>A</M> is a <M>k</M>-algebra.
#! @Returns IsCapFunctor 
#! @Arguments C
DeclareAttribute( "RightModuleToBimoduleFunctor", IsRightQuiverModuleCategory );