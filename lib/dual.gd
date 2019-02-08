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


#! @Chapter Operations on objects and morphisms

#! @Section Dual

#! @BeginGroup
#! @Description
#!  This function computes the dual <M>D(M)</M> of the module <A>M</A>, that is, 
#!  it computes <M>\Hom_k(M, k)</M>. If <A>M</A> is a module over <M>A</M>, then <M>D(M)</M> is a
#!  module over the opposite algebra of <M>A</M>.
#! @Returns <Ref Filt="IsQuiverModule"/> or <Ref Filt="IsQuiverRepresentation"/> 
#! @Arguments M
DeclareAttribute( "DualOfModule", IsQuiverModule ); 
#! @Arguments R
DeclareAttribute( "DualOfRepresentation", IsQuiverRepresentation ); 
#! @EndGroup

#! @Description
#!  This function computes the dual <M>D(f)</M> of the a homomorphism  <A>f</A>, that is, 
#!  it computes <M>\Hom_k(f, k)</M>. 
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/> 
#! @Arguments f
DeclareAttribute( "DualOfRepresentationHomomorphism", IsQuiverRepresentationHomomorphism );

#