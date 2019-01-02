#! @Chapter Complexity of algebras and modules

#! @Section Complexity of algebras and modules/representations
#! Most of the operations described in this section are given for both modules 
#! and represenations of quiver algebras, where the description of them
#! only are given for modules.  The description of the operations for 
#! representations is similar.

#! @BeginGroup
#! @Description
#!  Returns for a given module  <A>M</A> an estimate of the 
#!  complexity of the module by approximating the complexity by 
#!  considering the limit  <M>\lim_{m --> \infty} \log \dim(P(M)(m))/\log m</M>, 
#!  where <M>P(M)(m)</M> is the m-th projective in a minimal projective 
#!  resolution of  <A>M</A>  at stage  <M>m</M>.  This limit is estimated by 
#!  <M>\log \dim(P(M)(n))/\log n</M>.
#! @Returns IsFloat
#! @Arguments M, n
DeclareOperation( "ComplexityOfModule", [ IsQuiverModule, IsPosInt ] );
#! @Arguments R, n
DeclareOperation( "ComplexityOfRepresentation", [ IsQuiverRepresentation, IsPosInt ] ); 
#! @EndGroup

#! @Description
#!  Returns for a given algebra  <A>A</A> an estimate of the 
#!  maximal complexity of the simple modules by approximating the 
#!  complexity of each simple module <M>S</M>.
#! @Returns IsFloat
#! @Arguments A, n
DeclareOperation( "ComplexityOfAlgebra", [ IsQuiverAlgebra, IsPosInt ] );

#