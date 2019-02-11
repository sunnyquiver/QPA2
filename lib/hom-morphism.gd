#! @Chapter Hom spaces

#! @Section Morphisms of hom spaces

#! @Description
#!  Compute Hom of a morphism and an object.
#!
#!  Given a morphism <M>f \colon X_1 \to X_2</M> and an object <M>Y</M>
#!  in a <M>k</M>-category, this function returns the linear transformation
#!  <Display>
#!  f^* = \operatorname{Hom}(f, Y) \colon
#!           \operatorname{Hom}(X_2, Y) \to \operatorname{Hom}(X_1, Y)
#!  </Display>
#!  given by composition with <M>f</M>.
#! @Returns IsLinearTransformation
#! @Arguments f, Y
#! @Label for morphism and object
DeclareOperation( "Hom", [ IsFieldCategoryMorphism, IsFieldCategoryObject ] );

#!
DeclareOperation( "Hom", [ IsFieldCategoryObject, IsFieldCategoryMorphism ] );

#!
DeclareOperation( "Hom", [ IsFieldCategoryMorphism, IsFieldCategoryMorphism ] );

#