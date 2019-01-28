#! @Chapter Operations on objects and morphisms

#! @Section Minimal morphisms

#! @Description
#!  This function returns true is the homomorphism  <A>f</A>  is left 
#!  minimal. 
#! @Returns <C>true</C> or <C>false</C>
#! @Arguments f
DeclareProperty( "IsLeftMinimal", IsQuiverRepresentationHomomorphism );
 
#! @Description
#!  This function returns true is the homomorphism  <A>f</A>  is right 
#!  minimal. 
#! @Returns <C>true</C> or <C>false</C>
#! @Arguments f
DeclareProperty( "IsRightMinimal", IsQuiverRepresentationHomomorphism );
