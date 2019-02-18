#! @Chapter Operations on objects and morphisms

#! @Section Socle

#! @Description
#!  Returns a inclusion of the socle of the representation  <A>R</A>  into the 
#!  representation  <A>R</A>.
#! @Returns <Ref Filt="IsQuiverRepresentationHomomorphism"/>
#! @Arguments R
DeclareOperation( "SocleOfRepresentationInclusion", [ IsQuiverRepresentation ]);

#! @Description
#!  This function computes the socle of the representation <A>R</A>.
#! @Returns <Ref Filt="IsQuiverRepresentation"/>
#! @Arguments R
DeclareOperation( "SocleOfRepresentation", [ IsQuiverRepresentation ]);

#! @Description
#!  This function returns the socle series of the representation  <A>R</A>, 
#!  for a representation over a (quotient of a) path algebra. It 
#!  returns a list of dimension vectors of the modules: 
#!  <M>[..., soc(R/soc^3 R), soc(R/soc^2 R), soc(R/soc R), soc R]</M>.
#! @Returns a list of dimension vectors
#! @Arguments R
DeclareOperation( "SocleSeries", [ IsQuiverRepresentation ] ); 

#