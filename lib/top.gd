#! @Chapter Operations on objects and morphisms

#! @Section Top

#! @BeginGroup
#!  This function computes the top  <M>M/\rad(M)</M>  of the module <A>M</A>.
#! @Arguments M
DeclareAttribute( "TopOfModule", IsQuiverModule );
#! @Arguments R
DeclareAttribute( "TopOfRepresentation", IsQuiverRepresentation );
#! @EndGroup

#! @BeginGroup TopBasis
#! @Description
#!  Returns a list of elements in <A>M</A> which constitute a basis for the top.
#! @Returns list
#! @Arguments M
DeclareAttribute( "TopBasis", IsQuiverModule );
#! @Arguments R
DeclareAttribute( "TopBasis", IsQuiverRepresentation );
#! @EndGroup

# 


