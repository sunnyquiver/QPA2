#! @Chapter Special modules

#! @Section Injective, projective and simple modules. 

#! Here we give how to construct injective, projective and simple 
#! modules over an admissible quiver algebra. 

#! @Arguments side, A
#! @Description
#! Constructs the injective modules over an admissible quiver algebra <A>A</A>.
DeclareOperation( "IndecInjModules", [ IsSide, IsQuiverAlgebra ] );

#! @Arguments A
#! @Description
#! Constructs the injective left modules over an admissible quiver algebra <A>A</A>.
DeclareAttribute( "IndecInjLeftModules", IsQuiverAlgebra );

#! @Arguments A
#! @Description
#! Constructs the projective right modules over an admissible quiver algebra <A>A</A>.
DeclareAttribute( "IndecInjRightModules", IsQuiverAlgebra );

#! @Arguments algebras
#! @Description
#! Constructs the injective bimodules over two admissible quiver algebras <A>A</A> and 
#! <A>B</A> given as a list <C>algebras = [ A, B ]</C>.
DeclareOperation( "IndecInjBimodules", [ IsDenseList ] );

#! @Arguments side, A
#! @Description
#! Constructs the projective modules over an admissible quiver algebra <A>A</A>.
DeclareOperation( "IndecProjModules", [ IsSide, IsQuiverAlgebra ] );

#! @Arguments A
#! @Description
#! Constructs the projective left modules over an admissible quiver algebra <A>A</A>.
DeclareAttribute( "IndecProjLeftModules", IsQuiverAlgebra );

#! @Arguments A
#! @Description
#! Constructs the projective right modules over an admissible quiver algebra <A>A</A>.
DeclareAttribute( "IndecProjRightModules", IsQuiverAlgebra );

#! @Arguments algebras
#! @Description
#! Constructs the projective bimodules over two admissible quiver algebras <A>A</A> and 
#! <A>B</A> given as a list <C>algebras = [ A, B ]</C>.
DeclareOperation( "IndecProjBimodules", [ IsDenseList ] );

#! @Arguments side, A
#! @Description
#! Constructs the simple modules over an admissible quiver algebra <A>A</A>.
DeclareOperation( "SimpleModules", [ IsSide, IsQuiverAlgebra ] );

#! @Arguments A
#! @Description
#! Constructs the simple left modules over an admissible quiver algebra <A>A</A>.
DeclareAttribute( "SimpleLeftModules", IsQuiverAlgebra );

#! @Arguments A
#! @Description
#! Constructs the simple left modules over an admissible quiver algebra <A>A</A>.
DeclareAttribute( "SimpleRightModules", IsQuiverAlgebra );

#! @Arguments algebras
#! @Description
#! Constructs the simple bimodules over two admissible quiver algebras <A>A</A> and 
#! <A>B</A> given as a list <C>algebras = [ A, B ]</C>.
DeclareOperation( "SimpleBimodules", [ IsDenseList ] );

#! @Arguments M
#! @Description
#! Checks if the algebra of the module <A>M</A> is an admissible quiver
#! algebra, and then tests if the module <A>M</A> is simple.
DeclareProperty( "IsSimpleQuiverModule", IsQuiverModule );

#