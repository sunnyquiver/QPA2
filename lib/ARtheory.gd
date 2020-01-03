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

#! @Description
#!  Returns the predecessors of the module <Arg>M</Arg> in the
#!  AR-quiver of the algebra <Arg>M</Arg> is given over of distance
#!  less or equal to <Arg>n</Arg>. 
#!  It returns two lists, the first is the indecomposable modules in
#!  the different layers and the second is the valuations for the
#!  arrows in the AR-quiver.  The different entries in the first list
#!  are the modules at distance zero, one, two, three, and so on, until
#!  layer  <Arg>n</Arg>. The <C>m</C>-th entry in the second list is 
#!  the valuations of the irreducible morphism from indecomposable 
#!  module number <C>i</C> in layer <C>m+1</C> to indecomposable module
#!  number <C>j</C> in layer <C>m</C> for the values of <C>i</C> and 
#!  <C>j</C> there is an irreducible morphism. Whenever <C>false</C> 
#!  occur in the output, it means that this valuation has not been 
#!  computed. 
#!
#!  The function assumes that the module <Arg>M</Arg> is indecomposable 
#!  and that the quotient of the path algebra is given over a finite field. 
#! @Returns <C>IsList</C>
#! @Arguments M, n
DeclareOperation( "PredecessorsOfModule", [ IsQuiverModule, IS_INT ]) ;

#