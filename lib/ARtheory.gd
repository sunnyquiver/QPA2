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

#