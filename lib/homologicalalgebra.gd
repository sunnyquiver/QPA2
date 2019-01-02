#! @Chapter Homological algebra

#! @Section Classes of algebras 

#! @Description
#!  Returns <C>true</C> if the attribute <C>IsFiniteGlobalDimensionAlgebra</C>
#!  has been set to true.
#! @Returns <C>true</C> or <C>false</C>
#! @Arguments A
DeclareProperty( "IsFiniteGlobalDimensionAlgebra", IsAlgebra );

#! @Description
#! The function returns true if  <A>A</A>  is a finite dimensional semisimple 
#! algebra and searches for another method otherwise.
#! @Arguments A
DeclareProperty( "IsSemisimpleAlgebra", IsAlgebra );

InstallTrueMethod( IsFiniteGlobalDimensionAlgebra, IsSemisimpleAlgebra );
InstallTrueMethod( IsFiniteGlobalDimensionAlgebra, IsPathAlgebra );

#