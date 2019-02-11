#! @Chapter Stable categories

#! @Section Stable categories modulo projectives

#! @Description
#!  GAP category for stable category modulo projectives.
#! @Label
DeclareCategory( "IsStableCategoryModuloProjectives", IsFieldCategory );

#! @Description
#!  GAP category for objects in a stable category modulo projectives.
#! @Label
DeclareCategory( "IsStableCategoryModuloProjectivesObject", IsFieldCategoryObject );

#! @Description
#!  GAP category for morphisms in a stable category modulo projectives.
#! @Label
DeclareCategory( "IsStableCategoryModuloProjectivesMorphism", IsFieldCategoryMorphism );

#! @Description
#!  GAP category for hom spaces in a stable category modulo projectives.
DeclareCategory( "IsStableHomSpaceModuloProjectives", IsHomSpace );

#! The space of morphisms in a stable module category modulo projectives is 
#! given by <M>\operatorname{Hom}( M, N )</M> modulo the space <M>\mathcal{P}(M,N)</M>
#! of all morphisms factoring through a projective module. 

#! @Description
#!  Returns the subspace <M>\mathcal{P}(M,N)</M> of all morphisms factoring through
#!  a projective module for a given hom space in a stable module category modulo 
#!  projectives.
#! @Returns IsHomSpace
#! @Arguments hom
DeclareAttribute( "StableHomIdealInclusion", IsStableHomSpaceModuloProjectives );

#! @Description
#!  Returns the projection from <M>\operatorname{Hom}( M, N )</M> to the quotient
#!  by the subspace <M>\mathcal{P}(M,N)</M> consisting of all morphisms factoring 
#!  through a projective module for a given hom space in a stable module category 
#!  modulo projectives.
#! @Returns IsHomSpace
#! @Arguments hom
DeclareAttribute( "StableHomProjection", IsStableHomSpaceModuloProjectives );


#! @Description
#!  Given <M>\underline{\operatorname{Hom}}(M,N)</M> in a stable category, 
#!  returns the corresponding hom space <M>\operatorname{Hom}(M,N)</M> in the 
#!  original category. 
#! @Returns IsHomSpace
#! @Arguments hom
DeclareAttribute( "OriginalHomSpace", IsStableHomSpaceModuloProjectives );

#! @Description
#!  Returns the representation <A>R</A> as an object in the stable
#!  category <A>C</A> modulo projectives.  
#! @Returns IsStableCategoryModuloProjectivesObject
#! @Arguments R, C
DeclareOperation( "AsStableCategoryObject", [ IsQuiverRepresentation, IsStableCategoryModuloProjectives ] );

#! @Description
#!  Given an object <A>X</A> in a stable category modulo projectives, returns 
#!  the corresponding object in the original category.  
#! @Returns IsQuiverRepresentation
#! @Arguments X
DeclareAttribute( "OriginalObject", IsStableCategoryModuloProjectivesObject );

#! @Description
#!  Returns the homomorphism <A>f</A> as a morphism in the stable
#!  category <A>C</A> modulo projectives.  
#! @Returns IsStableCategoryModuloProjectivesObject
#! @Arguments f, C
DeclareOperation( "AsStableCategoryMorphism", [ IsQuiverRepresentationHomomorphism, IsStableCategoryModuloProjectives ] );
#! @Description
#!  Given a morphism <A>f</A> in a stable category modulo projectives, returns 
#!  the corresponding homomorphism in the original category.  
#! @Returns IsQuiverRepresentationHomomorphism
#! @Arguments X
DeclareAttribute( "OriginalMorphism", IsStableCategoryModuloProjectivesMorphism );

#! @Description
#!  Returns the stable category modulo projectives of the category <A>C</A>.
#! @Returns IsStableCategoryModuloProjectives
#! @Arguments C
DeclareAttribute( "StableCategoryModuloProjectives", IsQuiverRepresentationCategory );

#