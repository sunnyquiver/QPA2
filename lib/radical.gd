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

#! @Description
#!  This function returns the radical series of the algebra  <A>A</A> in a 
#!  list, where the first element is the algebra  <A>A</A> itself, then 
#!  radical of <A>A</A>, radical square of <A>A</A>, and so on.
#! @Returns a list of <Ref Filt="IsQuiverAlgebraIdeal"/>
#! @Arguments A
DeclareAttribute( "RadicalSeriesOfAlgebra", IsAlgebra );


#