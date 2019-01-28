#! @Chapter Operations on objects and morphisms

#! @Section Radical

#! @BeginGroup
#!  This function computes the radical of the module <A>M</A>.
#! @Returns <Ref Filt="IsQuiverModule"/> or <Ref Filt="IsQuiverRepresentation"/>
#! @Arguments M
DeclareAttribute( "RadicalOfModule", IsQuiverModule );
#! @Arguments R
DeclareAttribute( "RadicalOfRepresentation", IsQuiverRepresentation );
#! @EndGroup

#! @BeginGroup
#!  This function returns the radical series of the module  <A>M</A>, for a 
#!  module over a (quotient of a) path algebra. It returns a list of 
#!  dimension vectors of the modules: <M>[ M/\rad M, \rad M/\rad^2 M, 
#!  \rad^2 M/\rad^3 M, \ldots ]</M>.
#! @Arguments M
DeclareOperation( "RadicalSeries",  [ IsQuiverModule ] ); 
#! @Arguments R
DeclareOperation( "RadicalSeries",  [ IsQuiverRepresentation ] );
#! @EndGroup

