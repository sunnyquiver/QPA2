#! @Chapter Categorical properties of objects and morphisms

#! @Section Categorical tests

#! @Description
#!  This function returns true if the object  <A>M</A>  is in the additive
#!  closure of the object  <A>N</A>, an error message if  <A>M</A>  and  <A>N</A>  are 
#!  not objects in the same category and false otherwise.
#! @Returns <C>true</C> or <C>false</C>
#! @Arguments M, N
DeclareOperation( "IsInAdditiveClosure", [ IsFieldCategoryObject, IsFieldCategoryObject ] ); 

#! @BeginGroup
#! @Description
#!  This function returns true if the modules  <A>M</A>  and  <A>N</A>  are 
#!  isomorphic, an error message if  <A>M</A>  and  <A>N</A>  are not modules over 
#!  the same algebra and false otherwise.
#! @Returns <C>true</C> or <C>false</C>
#! @Arguments M, N
DeclareOperation( "IsomorphicModules", [ IsQuiverModule, IsQuiverModule ] ); 
#! @Arguments R1, R2
DeclareOperation( "IsomorphicRepresentations", [ IsQuiverRepresentation, IsQuiverRepresentation ] );
#! @EndGroup

#