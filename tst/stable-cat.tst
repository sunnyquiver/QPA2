gap> START_TEST( "stable-cat" );

gap> A := LeftNakayamaAlgebra( Rationals, [ 3, 3, 3 ] );;
gap> rep := CategoryOfQuiverRepresentations( A );;
gap> stab := StableCategoryModuloProjectives( rep );;
gap> Ps := IndecProjRepresentations( A );;
gap> Ps_st := List( Ps, P -> AsStableCategoryObject( P, stab ) );;
gap> ForAll( Ps_st, IsZero );
true

gap> A := LeftNakayamaAlgebra( Rationals, [ 3, 3, 3 ] );;
gap> lmod := LeftModuleCategory( A );;
gap> lmod_st := StableCategoryModuloProjectives( lmod );;
gap> Ps := IndecProjLeftModules( A );;
gap> Ps_st := List( Ps, P -> AsStableCategoryObject( P, lmod_st ) );;
gap> ForAll( Ps_st, IsZero );
true

gap> STOP_TEST( "stable-cat" );
