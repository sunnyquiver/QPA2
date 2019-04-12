InstallMethod( StarOfModule,
"for a quiver module",
[ IsQuiverModule ],
function( N )
  
  local   side,  A,  M;
  
  side := Side( N );
  if side = LEFT_RIGHT then 
    Error( "The construction of the star of a module is on one-sided modules.\n" );
  fi;
  A := AlgebraOfCategory( CapCategory( N ) );
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
  A := AlgebraOfCategory( CapCategory( Source( f ) ) );
  M := AlgebraAsBimodule( A );
  
  return Hom( side, f, M );
end
  );

InstallMethod( StarFunctor,
"for a category of quiver modules",
[ IsQuiverModuleCategory ],
function( C )
  
  local   A,  side,  D,  U,  V,  starfunctor,  morphism,  object;
  
  A := AlgebraOfCategory( C );
  side := Side( C );
  if side = LEFT_RIGHT then 
    Error( "The construction of the star functor is on one-sided modules.\n" );
  fi;
  D := ModuleCategory( Opposite( Side( C ) ), A ); 
  U := StableCategoryModuloProjectives( C ); 
  V := StableCategoryModuloProjectives( D );
  starfunctor := CapFunctor( "StarFunctor", [ [ U, true ] ], V );
  
  morphism := function( M1, h, M2 ) 
    return AsStableCategoryMorphism( StarOfModuleHomomorphism( OriginalMorphism( h ) ), V );
  end;
  
  object := function( N )
    return AsStableCategoryObject( StarOfModule( OriginalObject( N ) ), V );
  end;
  
  AddObjectFunction( starfunctor, object );
  
  AddMorphismFunction( starfunctor, morphism );
  
  return starfunctor;  
end
  );