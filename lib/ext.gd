#! @Chapter Extensions

#! @Section Extensions for field category objects

#! @Description
#!  Category for the space of extensions. 
DeclareCategory( "IsExtSpace", IsQPAVectorSpace );

#! @Description
#!  Given an positive integer <A>n</A> and two objects in a field category  
#!  <A>R1</A> and <A>R2</A>, the function computes a (minimal) projective 
#!  resolution 
#!  ..... --> P_2 -- d_2 --> P_1 -- d_1 --> P_0 -- d_0 --> R1 --> 0
#!  of the object  <A>R1</A>, this function returns the vector space of 
#!  extensions of degree <A>n</A>, which starts in <A>R2</A> and ends in 
#!  <A>R1</A>. The vector space of extensions are endoved with with several 
#!  attributes,  <C>EndTerm</C>, <C>StartTerm</C>, <C>ExtDegree</C>, 
#!  <C>AsCycleMap</C> and <C>ExtensionFromCycle</C>.
#! @Returns <Ref Filt="IsExtSpace"/>
DeclareOperation( "Ext", [ IsInt, IsFieldCategoryObject, IsFieldCategoryObject ] );

#! @Description
#!  Category for extensions in a field category. 
DeclareCategory( "IsExtension", IsQPAVector );

#! @Description
#!  Returns the end term of an extension.
#! @Returns <Ref Filt="IsFieldCategoryObject"/>
#! @Arguments ext
DeclareAttribute( "EndTerm", IsExtension );

#! @Description
#!  Returns the start term of an extension.
#! @Returns <Ref Filt="IsFieldCategoryObject"/>
#! @Arguments ext
DeclareAttribute( "StartTerm", IsExtension );

#! @Description
#!  Returns the degree of an extension.
#! @Returns <Ref Filt="IsPosInt"/>
#! @Arguments ext
DeclareAttribute( "ExtDegree", IsExtension );

#! @Description
#!  Returns an exact sequence of morphisms between objects 
#!  representing the entered extension.
#! @Returns <Ref Filt="IsList"/>
#! @Arguments ext
DeclareAttribute( "AsExactSequence", IsExtension );

#! @Description
#!  Returns an extension as a cocycle.
#! @Returns <Ref Filt="IsList"/>
#! @Arguments extension
DeclareAttribute( "AsCycle", IsExtension );

#! @Description
#!  Returns the category in which the space of extensions lives.
#! @Returns <Ref Filt="IsCapCategory"/>
#! @Arguments ext
DeclareAttribute( "CategoryOfExtSpace", IsExtSpace );

#! @Description
#!  Returns the end term of a space of extensions.
#! @Returns <Ref Filt="IsFieldCategoryObject"/>
#! @Arguments ext
DeclareAttribute( "EndTerm", IsExtSpace );

#! @Description
#!  Returns the start term of a space of extensions.
#! @Returns <Ref Filt="IsFieldCategoryObject"/>
#! @Arguments ext
DeclareAttribute( "StartTerm", IsExtSpace );

#! @Description
#!  Returns the degree of a space of extensions.
#! @Returns <Ref Filt="IsPosInt"/>
#! @Arguments ext
DeclareAttribute( "ExtDegree", IsExtSpace );

#! @Description
#!  Returns a morphism from the vector space of extensions to the 
#!  vector space of cocycles. 
#! @Returns <Ref Filt="IsLinearTransformation"/>
#! @Arguments ext
DeclareAttribute( "AsCycleMap", IsExtSpace );

#! @Description
#!  Returns an extension of degree <C>n</C> from an <C>n</C>-chain <A>f</A>. 
#!  If <A>f</A> is not an <C>n</C>-cycle, function returns <C>fail</C>.  
#! @Returns <Ref Filt="IsExtension"/>
#! @Arguments ext, f
DeclareOperation( "ExtensionFromCycle", [ IsExtSpace, IsFieldCategoryMorphism ] );

#