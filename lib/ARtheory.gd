#! @Chapter Auslander-Reiten theory

#! @Section Dual of the transpose and the transpose of the dual

#! @BeginGroup
#! @Description
#!  This function computes the dual of the transpose of a module <Arg>M</Arg>.  
#! @Returns <Ref Filt="IsQuiverModule"/>
#! @Arguments M
DeclareAttribute( "DTr", IsQuiverModule );
#! @Returns <Ref Filt="IsQuiverModule"/>
#! @Arguments M, n
# DeclareOperation( "DTr", [ IsQuiverModule, IS_INT ] );
#! @EndGroup

#! @BeginGroup
#! @Description
#!  This function computes the dual of the transpose of a module <Arg>M</Arg>.  
#! @Returns <Ref Filt="IsQuiverModule"/>
#! @Arguments M
DeclareAttribute( "TrD", IsQuiverModule );
#! @Returns <Ref Filt="IsQuiverModule"/>
#! @Arguments M, n
# DeclareOperation( "TrD", [ IsQuiverModule, IS_INT ] );
#! @EndGroup

#! @Section Almost split sequences and Auslander-Reiten quivers

#! @Description
#!  This function finds the almost split sequence ending or starting 
#!  in the module <A>M</A> depending on the second argument <A>side</A>.
#!  It checks if the module is not injective or not projective also 
#!  depending on the argument <A>side</A>. It returns fail if the module 
#!  is projective and <C>side = RIGHT</C> and if the module is injective and 
#!  <C>side = LEFT</C>. The almost split sequence is returned as a pair of 
#!  maps, the monomorphism and the epimorphism. The function assumes 
#!  that the module  <A>M</A>  is indecomposable. The entered endterm and the
#!  returned endterm might be a module that is isomorphic to the input, 
#!  not necessarily identical. 
#! @Returns <C>IsList</C>
#! @Arguments A, side
DeclareOperation( "AlmostSplitSequence", [ IsQuiverModule, IsDirection ] );

#