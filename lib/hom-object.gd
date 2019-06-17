#! @Chapter Hom spaces

#! @Section Hom objects

#!
DeclareCategory( "IsHomObject", IsFieldCategoryObject );

#! @Description
#!  Returns the source of all morphisms in a given hom object.
#!  That is, if the argument <A>hom</A> is <M>\operatorname{Hom}(X,Y)</M>,
#!  then the result is the object <M>X</M>.
#! @Returns IsFieldCategoryObject
#! @Arguments hom
DeclareAttribute( "Source", IsHomObject );

#! @Description
#!  Returns the range of all morphisms in a given hom object.
#!  That is, if the argument <A>hom</A> is <M>\operatorname{Hom}(X,Y)</M>,
#!  then the result is the object <M>Y</M>.
#! @Returns IsFieldCategoryObject
#! @Arguments hom
DeclareAttribute( "Range", IsHomObject );

#! @Description
#!  For a given hom object <M>\operatorname{Hom}_{\mathscr{C}}(X, Y)</M>,
#!  returns the category <M>\mathscr{C}</M>.
#! @Returns IsFieldCategory
#! @Arguments hom
DeclareAttribute( "HomCategory", IsHomObject );

#! @Description
#!  Returns the morphism in the hom object <A>hom</A> corresponding to the
#!  function <A>f</A>.
#!  If <A>f</A> does not determine a well-defined morphism in <A>hom</A>,
#!  the operation returns <C>fail</C>.
#! @Arguments hom, f
#! @Returns IsFieldCategoryMorphism
DeclareOperation( "MorphismByFunction",
                  [ IsHomObject, IsFunction ] );

#! @Description
#!  Returns the morphism in the hom object <A>hom</A> corresponding to the
#!  linear transformation <A>T</A>.
#!  If <A>T</A> does not determine a well-defined morphism in <A>hom</A>,
#!  the operation returns <C>fail</C>.
#! @Arguments hom, T
#! @Returns IsFieldCategoryMorphism
DeclareOperation( "MorphismByLinearTransformation",
                  [ IsHomObject, IsLinearTransformation ] );

