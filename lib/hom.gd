#! @Chapter Functors

#! @Section Hom-functors

#! @Description
#!  Given two representations  <A>R1</A> and <A>R2</A> this constructs
#!  a vector space basis for the Hom-space between <A>R1</A> and <A>R2</A>.
#!  <P/>
#! @Returns List of <Ref Filt="IsQuiverRepresentationHomomorphism"/>
#! @Arguments R1, R2
DeclareOperation( "BasisOfHom", [ IsQuiverRepresentation, IsQuiverRepresentation ]);