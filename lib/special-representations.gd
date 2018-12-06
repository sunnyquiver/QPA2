#! @Chapter Special representations

#! @Section Injective, projective and simple representations.

#! Here we give how to construct injective, projective and simple 
#! representations over an admissible quiver algebra.

#! @Arguments A
#! @Description
#! Constructs the injective representations over an admissible quiver algebra <A>A</A>.
DeclareAttribute( "IndecInjRepresentations", IsQuiverAlgebra );

#! @Arguments A
#! @Description
#! Constructs the injective representations over an admissible quiver algebra <A>A</A>.
DeclareAttribute( "IndecProjRepresentations", IsQuiverAlgebra );

#! @Arguments A
#! @Description
#!  The basis elements of the algebra <A>A</A> ordered by the
#!  indecomposable projective module they belong to.
#!  The result is a list of lists, such that the first list contains
#!  the basis of the indecomposable projective in the first vertex,
#!  and so on.
DeclareAttribute( "BasisOfProjectives", IsQuiverAlgebra );

#! @Arguments A
#! @Description
#! Constructs the simple representations over an admissible quiver algebra <A>A</A>
DeclareAttribute( "SimpleRepresentations", IsQuiverAlgebra );

#! @Arguments R
#! @Description
#! Checks if the algebra of the representation <A>R</A> is an admissible
#! quiver algebra, and then tests if the representation <A>R</A> is simple.
DeclareProperty( "IsSimpleRepresentation", IsQuiverRepresentation );

#!
