#! @Chapter Representation type of algebras

#! @Section Tests for finite representation type

#! @Description
#!  Returns <C>true</C> if <A>A</A> is of finite representation type.
#!  Returns false if <A>A</A> is of infinite representation type.
#!  Returns fail if we can not determine the representation type
#!  (i.e. it imposible from theoretical/algorithmic point of view
#!  or a suitable criterion has not been implemented yet).
#! @Returns <C>IsBool</C>
#! @Arguments A
DeclareProperty( "IsFiniteTypeAlgebra", IsQuiverAlgebra );

#