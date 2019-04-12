InstallMethod( TransposeOfModule, 
"for a quiver module",
[ IsQuiverModule ],
function( N )
    
  local   side,  M,  projres,  d1,  d1star;
  
  side := Side( N );
  if side = LEFT_RIGHT then 
    Error( "The construction of the transpose is on one-sided modules.\n" );
  fi;
  
  M := AlgebraAsBimodule( AlgebraOfRepresentation( UnderlyingRepresentation( N ) ) );  
  projres := AsChainComplex( ProjectiveResolution( N ) );
  d1 := projres^1;
  d1star := Hom( side, d1, M );
    
  return CokernelObject( d1star );
end 
  );

InstallMethod( TransposeOfModuleHomomorphism, 
"for a quiver module homomorphism",
[ IsQuiverModuleHomomorphism ],
function( h )
    
    local   side,  A,  M,  M1,  M2,  d0,  kerd0,  d1prime,  d1,  d0_2,  
            kerd0_2,  d1prime_2,  d1_2,  h0,  h1prime,  h1,  
            starofmaps,  map;
    
  side := Side( Source( h ) );
  if side = LEFT_RIGHT then 
    Error( "The construction of the transpose is on one-sided modules.\n" );
  fi;
  
  A := AlgebraOfRepresentation( UnderlyingRepresentation( Source( h ) ) );  
  M := AlgebraAsBimodule( A );
  M1 := Source( h );
  M2 := Range( h );
  d0 := ProjectiveCover( M1 );
  kerd0 := KernelEmbedding( d0 );
  d1prime := ProjectiveCover( kerd0 );
  d1 := PreCompose( d1prime, kerd0 );
    
  d0_2 := ProjectiveCover( M2 );
  kerd0_2 := KernelEmbedding( d0_2 );
  d1prime_2 := ProjectiveCover( kerd0_2 );
  d1_2 := PreCompose( d1prime_2, kerd0_2 ); 
    
  h0 := ProjectiveLift( PreCompose( d0, h ), d0_2 );
  h1prime := KernelObjectFunctorialWithGivenKernelObjects( kerd0, d0, h0, d0_2, kerd0_2 );
  h1 := ProjectiveLift( PreCompose( d1prime, h1prime ), d1prime_2 );
    
  starofmaps := List( [  d1_2, h0, h1, d1 ], m -> Hom( side, m, M ) );
  map := CokernelObjectFunctorial( [ starofmaps[ 1 ], starofmaps{[ 2..3 ]}, starofmaps[ 4 ] ] );

  return map;
end
  );
    
InstallMethod( TransposeFunctor, 
"for a category of quiver modules",
[ IsQuiverModuleCategory ],
function( C )
    
    local   A,  side,  D,  U,  V,  transposefunctor,  M,  morphism,  
            object;
  
  A := AlgebraOfCategory( C );
  side := Side( C );
  if side = LEFT_RIGHT then 
    Error( "The construction of the transpose functor is on one-sided modules.\n" );
  fi;
  D := ModuleCategory( Opposite( Side( C ) ), A ); 
  U := StableCategoryModuloProjectives( C ); 
  V := StableCategoryModuloProjectives( D );
  transposefunctor := CapFunctor( "TransposeFunctor", [ [ U, true ] ], V );
  
  M := AlgebraAsBimodule( A );
  
  morphism := function( M1, h, M2 ) 
    local   orig_h;
      
    orig_h := OriginalObject( h );

    return AsStableCategoryMorphism( TransposeOfModuleHomomorphism( orig_h ), V );
  end;
  
  object := function( N )
    return AsStableCategoryObject( TransposeOfModule( OriginalObject( N ) ), V );
  end;
  
  AddObjectFunction( transposefunctor, object );
  
  AddMorphismFunction( transposefunctor, morphism );
  
  return transposefunctor;      
end 
  ); 
