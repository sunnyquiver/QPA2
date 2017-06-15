#! @Chapter Functors

#! @Section Restriction functors

#! @Description
#!  Returns the restriction functor from the category <A>C</A> to the category <A>D</A>
#!  when <A>f</A> is an algebra homomorphism from the algebra defining the category <A>D</A>
#!  to the algebra defining the category <A>C</A>.
#! @Returns IsCapFunctor 
#! @Arguments f, C, D
DeclareOperation( "RestrictionFunctor", [ IsQuiverAlgebraHomomorphism, IsCapCategory, IsCapCategory ] );