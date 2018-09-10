#! @Chapter Dualities

#! @Section Duality for vector spaces

#! @Description
#!  Returns duality functor on a category <A>C</A> of vector spaces over a given field.
#! @Returns IsCapFunctor 
#! @Arguments C
DeclareOperation( "DualFunctor", [ IsVectorSpaceCategory ] );

#! @Description
#!  Returns duality functor from a category <A>C</A> of quiver representations over an algebra to
#!  the category of quiver representations over the opposite algebra.
#! @Returns IsCapFunctor 
#! @Arguments C
DeclareOperation( "DualFunctor", [ IsQuiverRepresentationCategory ] );

#! @Description
#!  Returns duality functor from a category <A>C</A> of quiver modules over an algebra to
#!  the category of quiver modules over the opposite algebra.
#! @Returns IsCapFunctor 
#! @Arguments C
DeclareOperation( "DualFunctor", [ IsQuiverModuleCategory ] );

