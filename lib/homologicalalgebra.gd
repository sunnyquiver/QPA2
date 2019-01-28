#! @Chapter Homological algebra

#! @Section Approximations

#! @Description
#!  This function computes a left <M>\add R</M>-approximation of the  
#!  representation <A>C</A>, and the approximation is not necessarily minimal.
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/>
#! @Arguments R, C
DeclareOperation( "LeftApproximationByAddR", [ IsQuiverRepresentation, IsQuiverRepresentation ] );

#! @Description
#!  This function computes a right <M>\add R</M>-approximation of the  
#!  representation <A>C</A>, and the approximation is not necessarily minimal.
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/>
#! @Arguments C, R
DeclareOperation( "RightApproximationByAddR", [ IsQuiverRepresentation, IsQuiverRepresentation ] );

#! @Section Classes of algebras

#! @Description
#! The function returns true if  <A>A</A>  is a finite dimensional semisimple 
#! algebra and searches for another method otherwise.
#! @Arguments A
DeclareProperty( "IsSemisimpleAlgebra", IsAlgebra );

#! @Description
#!  Returns <C>true</C> if the attribute <C>IsFiniteGlobalDimensionAlgebra</C>
#!  has been set to true.
#! @Returns <C>true</C> or <C>false</C>
#! @Arguments A
DeclareProperty( "IsFiniteGlobalDimensionAlgebra", IsAlgebra );

#! @Section Homological properties of modules and representations

#! @BeginGroup
#! @Description
#!  This function returns true if the entered module  <A>M</A>  is an
#!  exceptional module (ie. indecomposable and <M>\Ext^1(M,M)=(0)</M>, otherwise 
#!  false, if the field, over which the algebra  <A>M</A>  is defined over, 
#!  is finite.
#! @Returns <C>true</C> or <C>false</C>
#! @Arguments M
DeclareProperty( "IsExceptionalModule", IsQuiverModule );
#! @Arguments R
DeclareProperty( "IsExceptionalRepresentation", IsQuiverRepresentation );
#! @EndGroup

#! @Description
#!  This function tests if the object  <A>R</A>  is <M>\Omega</M>-periodic, that is,
#!  if  <M>R \simeq \Omega^i( R )</M>  when  <M>i</M>  ranges over the set <M>\{1,2,...,n\}</M>.
#!  Otherwise it returns false.
#! @Returns <C>IS_INT</C> or <C>false</C>
#! @Arguments R
DeclareOperation( "IsOmegaPeriodic", [ IsFieldCategoryObject, IS_INT ] );

#! @Description
#! This function returns true if the entered object  <A>R</A>  is a rigid 
#!  object, that is, no self extensions, otherwise false.
#! @Returns <C>true</C> or <C>false</C>
#! @Arguments R
DeclareProperty( "IsRigidObject", IsFieldCategoryObject );

#! @Section Projective resolutions

#! @Description
#!  This functions computes the  <A>n</A>-th syzygy of the object  <A>R</A>  by 
#!  first computing a projective resolution of  <A>R</A>, then it finds the
#!  <M>n-1</M>-th differential and takes the kernel.
#! @Returns <Ref Filt="IsFieldCategoryObject"/>
#! @Arguments R, n
DeclareOperation( "NthSyzygy", [ IsFieldCategoryObject, IS_INT ] );


#