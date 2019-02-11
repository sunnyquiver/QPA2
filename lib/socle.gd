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

#