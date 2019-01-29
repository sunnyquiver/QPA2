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

#! 
DeclareOperation( "MoreRightMinimalVersion", [ IsQuiverRepresentationHomomorphism ]);

#!
DeclareOperation( "MoreLeftMinimalVersion", [ IsQuiverRepresentationHomomorphism ]);

#! @Description
#!  This function computes the minimal right <M>\add R</M>-approximation of the
##  representation  <C>C = Range(f)</C>.  
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/>
#! @Arguments f
DeclareAttribute( "RightMinimalVersion", IsQuiverRepresentationHomomorphism );

#! @Description
#!  This function computes the minimal left <M>\add R</M>-approximation of the
##  representation  <C>C = Source(f)</C>.  
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/>
#! @Arguments f
DeclareAttribute( "LeftMinimalVersion", IsQuiverRepresentationHomomorphism );