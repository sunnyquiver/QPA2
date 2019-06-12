# field-cat2.gd:
# Contains a definition which belongs in field-cat.gd but must be read
# after vecspace.gd has been read.

#! @Chapter Hom spaces

#! @Section Field categories

#! @Description
#!  Returns the morphism from <A>X</A> to <A>Y</A> corresponding to the
#!  linear transformation <A>f</A>,
#!  where <A>X</A> and <A>Y</A> are objects in a field category.
#!  If <A>f</A> does not determine a well-defined morphism
#!  from <A>X</A> to <A>Y</A>,
#!  the operation returns <C>fail</C>.
#! @Arguments X, Y, f
#! @Returns IsFieldCategoryMorphism
DeclareOperation( "MorphismByLinearTransformation",
                  [ IsFieldCategoryObject, IsFieldCategoryObject, IsLinearTransformation ] );

