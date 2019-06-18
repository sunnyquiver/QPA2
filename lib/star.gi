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