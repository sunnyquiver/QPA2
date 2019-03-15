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

