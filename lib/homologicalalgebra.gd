#! @Chapter Homological algebra

#! @Section Projective resolutions

#! @Description
#!  This functions computes the  <A>n</A>-th syzygy of the object  <A>R</A>  by 
#!  first computing a projective resolution of  <A>R</A>, then it finds the
#!  <M>n-1</M>-th differential and takes the kernel.
#! @Returns <Ref Filt="IsFieldCategoryObject"/>
#! @Arguments R, n
DeclareOperation( "NthSyzygy", [ IsFieldCategoryObject, IS_INT ] );

#! @Description
#! Returns the projective cover of the quiver representation  <A>R</A>. 
#! @Returns IsQuiverRepresentationHomomorphism
#! @Arguments R
DeclareAttribute( "ProjectiveCover", IsQuiverRepresentation );

#! @Description
#! Returns the minimal projective resolution of the quiver representation  <A>R</A>
#! as a cochain complex with the homology in degree zero being isomorphic to <A>R</A>. 
#! @Returns IsCochainComplex
#! @Arguments R
# DeclareAttribute( "ProjectiveResolution", IsQuiverRepresentation );

#! @Section Injective resolutions

#! @Description
#! Returns the minimal injective envelope of the quiver representation  <A>R</A>
#! @Returns IsQuiverRepresentationHomomorphism
#! @Arguments R
DeclareAttribute( "InjectiveEnvelope", IsQuiverRepresentation );

#! @Description
#! Returns the minimal injective resolution of the quiver representation  <A>R</A>
#! as a cochain complex with the homology in degree zero being isomorphic to <A>R</A>. 
#! @Returns IsCochainComplex
#! @Arguments R
# DeclareAttribute( "InjectiveResolution", IsQuiverRepresentation );

#! @Section Approximations

#! @Description
#!  This function checks if the representation  <A>N</A>  has a finite coresolution
#!  in  add<A>R</A>  of length at most  <A>n</A>.  If it does, then this 
#!  resoultion is returned, otherwise false is returned. 
#! @Returns <C>IsList</C> 
#! @Arguments N, R 
DeclareOperation( "FiniteCoresolutionInAddR", [ IsQuiverRepresentation, IsQuiverRepresentation, IsInt ] );

#! @Description
#!  This function checks if the representation  <A>N</A>  has a finite resolution
#!  in  add<A>R</A>  of length at most  <A>n</A>.  If it does, then this 
#!  resoultion is returned, otherwise false is returned. 
#! @Returns <C>IsList</C> 
#! @Arguments N, R 
DeclareOperation( "FiniteResolutionInAddR", [ IsQuiverRepresentation, IsQuiverRepresentation, IsInt ] );

#! @Description
#!  This function computes a left <M>\add R</M>-approximation of the  
#!  representation <A>C</A>, and the approximation is not necessarily minimal.
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/>
#! @Arguments R, C
DeclareOperation( "LeftApproximationByAddR", [ IsQuiverRepresentation, IsQuiverRepresentation ] );

#! @Description
#!  This function computes a minimal left <M>\add R</M>-approximation of the  
#!  representation <A>C</A>. 
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/>
#! @Arguments R, C
DeclareOperation( "MinimalLeftApproximationByAddR", [ IsQuiverRepresentation, IsQuiverRepresentation ] );

#! @Description
#!  This function computes a right <M>\add R</M>-approximation of the  
#!  representation <A>C</A>, and the approximation is not necessarily minimal.
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/>
#! @Arguments C, R
DeclareOperation( "RightApproximationByAddR", [ IsQuiverRepresentation, IsQuiverRepresentation ] );

#! @Description
#!  This function computes a minimal right <M>\add R</M>-approximation of the  
#!  representation <A>C</A>. 
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/>
#! @Arguments C, R
DeclareOperation( "MinimalRightApproximationByAddR", [ IsQuiverRepresentation, IsQuiverRepresentation ] );

#! @Description
#!  Returns a left, not necessarily left minimal, Fac<A>R</A>-approximation 
#!  of the representation  <A>C</A>. 
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/> 
#! @Arguments C, R   
DeclareOperation( "LeftApproximationByFacR", [ IsQuiverRepresentation, IsQuiverRepresentation ] );

#! @Description
#!  Returns the left minimal Fac<A>R</A>-approximation of the representation  <A>C</A>. 
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/> 
#! @Arguments C, R 
DeclareOperation( "MinimalLeftApproximationByFacR", [ IsQuiverRepresentation, IsQuiverRepresentation ] );

#! @Description
#!  Returns a left, not necessarily left minimal, Sub<A>R</A>-approximation 
#!  of the representation  <A>C</A>. 
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/> 
#! @Arguments C, R 
DeclareOperation( "LeftApproximationBySubR", [ IsQuiverRepresentation, IsQuiverRepresentation ] );

#! @Description
#!  Returns the left minimal Sub<A>R</A>-approximation of the representation  <A>C</A>. 
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/> 
#! @Arguments C, R 
DeclareOperation( "MinimalLeftApproximationBySubR", [ IsQuiverRepresentation, IsQuiverRepresentation ] );

#! @Description
#!  Returns a right, not necessarily right minimal, Sub<A>R</A>-approximation 
#!  of the representation  <A>C</A>. 
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/> 
#! @Arguments R, C
DeclareOperation( "RightApproximationBySubR", [ IsQuiverRepresentation, IsQuiverRepresentation ] );

#! @Description
#!  Returns the right minimal Sub<A>R</A>-approximation of the representation  <A>C</A>. 
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/> 
#! @Arguments C, R 
DeclareOperation( "MinimalRightApproximationBySubR", [ IsQuiverRepresentation, IsQuiverRepresentation ] );

#! @Description
#!  Returns the minimal left <M>\widehat{\add T}</M>-approximation of the 
#!  representation  <A>R</A>.  It checks if  <A>T</A>  is a cotilting representation, and if not
#!  it returns an error message. 
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/> 
#! @Arguments C, R 
DeclareOperation( "LeftApproximationByAddTHat", [ IsQuiverRepresentation, IsQuiverRepresentation ]);

#! @Description
#!  Returns the minimal right <M>^\perp T</M>-approximation of the 
#!  representation  <A>R</A>.  It checks if  <A>T</A>  is a cotilting representation, and if not
#!  it returns an error message. 
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/> 
#! @Arguments C, R 
DeclareOperation( "RightApproximationByPerpT", [ IsQuiverRepresentation, IsQuiverRepresentation ]);


#! @Section Classes of algebras

#! @Description
#!  Returns <C>true</C> if the attribute <C>IsFiniteGlobalDimensionAlgebra</C>
#!  has been set to true.
#! @Returns <C>true</C> or <C>false</C>
#! @Arguments A
DeclareProperty( "IsFiniteGlobalDimensionAlgebra", IsAlgebra );

#! @Description
#!  Returns true or false depending on whether or not 
#!  the algebra  <A>A</A>  is hereditary.
#! @Returns <C>IS_INT</C> or <C>false</C>
#! @Arguments A
DeclareProperty( "IsHereditaryAlgebra", IsQuiverAlgebra );

#! @Description
#!  Returns true or false depending on whether or not 
#!  the algebra  <A>A</A>  is selfinjective.
#! @Returns <C>IS_INT</C> or <C>false</C>
#! @Arguments A
DeclareProperty( "IsSelfinjectiveAlgebra",  IsQuiverAlgebra );

#! @Description
#! The function returns true if  <A>A</A>  is a finite dimensional semisimple 
#! algebra and searches for another method otherwise.
#! @Arguments A
DeclareProperty( "IsSemisimpleAlgebra", IsAlgebra );

#! @Section Dimensions

#! @Description
#!  Returns the dominant dimension of the representation  <A>R</A>  if it 
#!  is less or equal to  <A>n</A>. If the representation  <A>R</A>  is injectiv 
#!  and projective, then it returns infinity. Otherwise it returns false.
#! @Returns <C>IS_INT</C> or <C>false</C>
#! @Arguments R, n                                                              
DeclareOperation( "DominantDimensionOfRepresentation", [ IsQuiverRepresentation, IS_INT ]);

#! @Description
#!  Returns the dominant dimension of the object  <A>R</A>, if it has been computed. 
#! @Returns <C>IS_INT</C>
#! @Arguments R
DeclareAttribute( "DominantDimension", IsFieldCategoryObject ); 

#! @Description
#!  Checks if the dominant dimension of  <A>A</A> is less or equal
#!  to <A>n</A>, and returns the value if so. 
#! @Returns <C>IS_INT</C>, <C>infty</C>, or <C>false</C>
#! @Arguments A, n
DeclareOperation( "DominantDimensionOfAlgebra", [ IsQuiverAlgebra, IS_INT ]);

#! @Description
#!  Returns faithful dimension of the representation  <A>R</A>. 
#! @Returns <C>IS_INT</C>
#! @Arguments R
DeclareAttribute( "FaithfulDimension", IsQuiverRepresentation );

#! @Description
#!  Checks if the global dimension of  <A>A</A> is less or equal
#!  to <A>n</A>, and returns the value if so. 
#! @Returns <C>IS_INT</C>, <C>infty</C>, or <C>false</C>
#! @Arguments A, n
DeclareOperation( "GlobalDimensionOfAlgebra", [ IsQuiverAlgebra, IS_INT ] );

#! @Description
#!  Returns the global dimension of <A>A</A>, if it has been computed. 
#! @Returns <C>IS_INT</C>
#! @Arguments A
DeclareAttribute( "GlobalDimension", IsAlgebra );

#! @Description
#!  Checks if the Gorenstein dimension of  <A>A</A> is less or equal
#!  to <A>n</A>, and returns the value if so. 
#! @Returns <C>IS_INT</C>, <C>infty</C>, or <C>false</C>
#! @Arguments A, n
DeclareOperation( "GorensteinDimensionOfAlgebra", [ IsQuiverAlgebra, IS_INT ]);

#! @Description
#!  Returns the Gorenstein dimension of the object  <A>R</A>, if it has been computed. 
#! @Returns <C>IS_INT</C>
#! @Arguments R
DeclareAttribute( "GorensteinDimension", IsQuiverAlgebra );

#! @Description
#!  Checks if the injective dimension of the object  <A>R</A> is less or equal
#!  to <A>n</A>, and returns the value if so. 
#! @Returns <C>IS_INT</C> or <C>false</C>
#! @Arguments R, n
DeclareOperation( "InjDimensionOfObject", [ IsFieldCategoryObject, IS_INT ] );

#! @Description
#!  Returns the injective dimension of the object  <A>R</A>, if it has been computed. 
#! @Returns <C>IS_INT</C>
#! @Arguments R
DeclareAttribute( "InjDimension", IsFieldCategoryObject ); 

#! @Description
#!  Checks if the projective dimension of the object  <A>R</A> is less or equal
#!  to <A>n</A>, and returns the value if so. 
#! @Returns <C>IS_INT</C> or <C>false</C>
#! @Arguments R, n
DeclareOperation( "ProjDimensionOfObject", [ IsFieldCategoryObject, IS_INT ] );

#! @Description
#!  Returns the projective dimension of the object  <A>R</A>, if it has been computed. 
#! @Returns <C>IS_INT</C>
#! @Arguments R
DeclareAttribute( "ProjDimension", IsFieldCategoryObject ); 

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

#! @Section Objects defined by homological properties 

#! @Description
#!  Checks if the representation  <A>R</A> is projective or not.
#! @Returns <C>true</C> or <C>false</C>
#! @Arguments R
DeclareProperty( "IsInjectiveRepresentation",  IsQuiverRepresentation  );

#! @Description
#!  Returns true if the object  <A>R</A>  is n-rigid, otherwise false. 
#! @Returns <C>true</C> or <C>false</C>
#! @Arguments R, n
DeclareOperation( "IsN_RigidObject", [ IsFieldCategoryObject, IS_INT ] );

#! @Description
#!  Returns true if the object  <A>R</A>  is a <M>N</M>th-syzygy, otherwise false. 
#! @Returns <C>true</C> or <C>false</C>
#! @Arguments R, n
DeclareOperation( "IsNthSyzygy", [ IsQuiverRepresentation, IS_INT ] );

#! @Description
#!  Checks if the representation  <A>R</A> is injective or not.
#! @Returns <C>true</C> or <C>false</C>
#! @Arguments R
DeclareProperty( "IsProjectiveRepresentation",  IsQuiverRepresentation  );

#! @Section Tilting theory 

#! @BeginGroup
#! @Description
#!  This function constructs all complements of an almost complete 
#!  tilting (cotilting) representation  <A>R</A>  given a complement  <A>X</A>  of  <A>R</A>.  The 
#!  complements are returned as a long exact sequence (whenever possible)
#!  of minimal left and minimal right  add<A>R</A>-approximations.
#! @Returns <C>IsList</C>
#! @Arguments R, X
DeclareOperation( "AllComplementsOfAlmostCompleteTiltingRepresentation", [ IsQuiverRepresentation, IsQuiverRepresentation ] );
#! @Arguments R, X
#! DeclareOperation( "AllComplementsOfAlmostCompleteCotiltingRepresentation", [ IsQuiverRepresentation, IsQuiverRepresentation ] );
#! @EndGroup

#! @Description
#!  This function checks if the representation  <A>R</A>  is a cotilting representation
#!  of injective dimension at most  <A>n</A>. If so, it returns a list of two 
#!  elements consisting of the injective dimension of the tilting module and the
#!  finite resolutions of all the indecomposable injective objects in the additive
#!  closure of the cotilting representation.  Otherwise it returns <C>false</C>.
#! @Returns <C>[ IS_INT, IsList ]</C> or <C>false</C>
#! @Arguments R, n
DeclareOperation( "CotiltingRepresentation", [ IsQuiverRepresentation, IsInt]);

#! @Description
#!  Returns <C>true</C> if the object  <A>R</A>  is a cotilting object.
#! @Returns <C>true</C> or <C>false</C>
#! @Arguments R
DeclareAttribute( "IsCotiltingObject", IsFieldCategoryObject );

#! @Description
#!  Returns <C>true</C> if the object  <A>R</A>  is a tilting object.
#! @Returns <C>true</C> or <C>false</C>
#! @Arguments R
DeclareAttribute( "IsTiltingObject", IsFieldCategoryObject );

#! @BeginGroup
#! @Description
#!  This function computes the left mutation of a complement  <A>N</A>  of
#!  an almost complete tilting (cotilting) representation  <A>R</A>, assuming that  <A>R</A>
#!  is an almost complete tilting (cotilting) representation.  If it doesn't exist, then the
#!  function returns false.
#! @Returns <Ref Filt="IsQuiverRepresentation"/> 
#! @Arguments R, N
DeclareOperation( "LeftMutationOfTiltingRepresentationComplement", [ IsQuiverRepresentation, IsQuiverRepresentation ] );
#! @Arguments R, N
#! DeclareOperation( "LeftMutationOfCotiltingRepresentationComplement", [ IsQuiverRepresentation, IsQuiverRepresentation ] );
#! @EndGroup

#! @BeginGroup
#! @Description
#!  This function computes the number complements of an almost 
#!  complete tilting/cotilting representation  <A>R</A>, assuming that  <A>R</A>
#!  is an almost complete tilting representation.
#!  representation  <A>R</A>.
#! @Returns <C>IS_INT</C> 
#! @Arguments R
DeclareOperation( "NumberOfComplementsOfAlmostCompleteTiltingRepresentation", [ IsQuiverRepresentation ] );
#! @Arguments R
#! DeclareOperation( "NumberOfComplementsOfAlmostCompleteCotiltingRepresentation", [ IsQuiverRepresentation ] );
#! @EndGroup

#! @BeginGroup
#! @Description
#!  This function computes the right mutation of a complement  <A>N</A>  of
#!  an almost complete tilting (cotilting) representation  <A>R</A>, assuming that  <A>R</A>
#!  is an almost complete tilting (cotilting) representation.  If it doesn't exist, then the
#!  function returns false.
#! @Returns <Ref Filt="IsQuiverRepresentation"/>
#! @Arguments R, N
DeclareOperation( "RightMutationOfTiltingRepresentationComplement", [ IsQuiverRepresentation, IsQuiverRepresentation ] );
#! @Arguments R, N
#! DeclareOperation( "RightMutationOfCotiltingRepresentationComplement", [ IsQuiverRepresentation, IsQuiverRepresentation ] );
#! @EndGroup

#! @Description
#!  This function checks if the representation  <A>R</A>  is a tilting representation
#!  of projective dimension at most  <A>n</A>. If so, it returns a list of two 
#!  elements consisting of the projective dimension of the tilting module and the
#!  finite coresolutions of all the indecomposable projective objects in the additive
#!  closure of the tilting representation.  Otherwise it returns <C>false</C>.
#! @Returns <C>[ IS_INT, IsList ]</C> or <C>false</C>
#! @Arguments R, n
DeclareOperation( "TiltingRepresentation", [ IsQuiverRepresentation, IsInt]);

#! @Section Representation dimension and generators

DeclareOperation( "PartialIyamaGenerator", [ IsQuiverRepresentation ] );

#! @Description
#!  Given a representation  <A>R</A> this function returns a representation 
#!  S  such that  <A>R</A>  is a direct summand of  S  and such that the 
#!  global dimension of the endomorphism ring of  S  is finite. 
#! @Returns <Ref Filt="IsQuiverRepresentation"/>
#! @Arguments R
DeclareOperation( "IyamaGenerator", [ IsQuiverRepresentation ] );

#