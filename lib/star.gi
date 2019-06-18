InstallMethod( StarOfModule,
"for a quiver module",
[ IsQuiverModule ],
function( N )
  
  local   side,  A,  M;
  
  side := Side( N );
  if side = LEFT_RIGHT then 
    Error( "The construction of the star of a module is on one-sided modules.\n" );
  fi;
  A := ActingAlgebra( side, N );
  M := AlgebraAsBimodule( A );
  
  return Hom( side, N, M );
end
  );

InstallMethod( StarOfModuleHomomorphism,
"for a quiver module homomorphism",
[ IsQuiverModuleHomomorphism ],
function( f )
  
  local   side,  A,  M;
  
  side := Side( Source( f ) );
  if side = LEFT_RIGHT then 
    Error( "The construction of the star of a homomorphism is on one-sided modules.\n" );
  fi;
  A := ActingAlgebra( side, Source( f ) );
  M := AlgebraAsBimodule( A );
  
  return Hom( side, f, M );
end
  );

InstallMethod( StarFunctor,
"for a category of quiver modules",
[ IsQuiverModuleCategory ],
function( C )
  local side, A, D, starfunctor, morphism, object;
  
  side := Side( C );
  A := ActingAlgebra( side, C );
  if side = LEFT_RIGHT then 
    Error( "The construction of the star functor is on one-sided modules.\n" );
  fi;
  D := ModuleCategory( Opposite( Side( C ) ), A ); 
  starfunctor := CapFunctor( "StarFunctor", [ [ C, true ] ], D );
  
  morphism := function( M1, h, M2 ) 
    return StarOfModuleHomomorphism( h );
  end;
  
  object := function( N )
    return StarOfModule( N );
  end;
  
  AddObjectFunction( starfunctor, object );
  
  AddMorphismFunction( starfunctor, morphism );
  
  return starfunctor;  
end
  );

#######################################################################
##
#A  NakayamaFunctorOfModule( <M> )
##
##  This function takes as an argument a module over an algebra  A  and
##  computes the image of the Nakayama functor, that is, the module  
##  Hom_K(Hom_A(M, A), K)  over  A. 
##  
InstallMethod( NakayamaFunctorOfModule, 
"for a quiver module",
[ IsQuiverModule ],
function( M );
    
    return DualOfModule( StarOfModule( M ) );
end
  );

#######################################################################
##
#A  NakayamaFunctorOfModuleHomomorphism( <f> )
##
##  This function takes as an argument a homomorphism  f  between two 
##  modules  M  and  N  over an algebra  A  and computes the induced 
##  homomorphism from the module  Hom_K(Hom_A(N, A), K)  to the module  
##  Hom_K(Hom_A(M, A), K)  over  A. 
##
InstallMethod( NakayamaFunctorOfModuleHomomorphism, 
"for a quiver module homomorphism",
[ IsQuiverModuleHomomorphism ],
function( f ); 

  return DualOfModuleHomomorphism( StarOfModuleHomomorphism( f ) );
end
);

InstallMethod( NakayamaFunctor, "for a category of quiver modules",
        [ IsQuiverModuleCategory ],
        function( C )
  
  local F, G;
  
  F := StarFunctor( C );
  G := DualFunctor( AsCapCategory( Range( F ) ) );

  return PreComposeFunctors( F, G );
end 
  );
