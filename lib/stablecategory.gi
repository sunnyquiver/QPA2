
BindGlobal( "FamilyOfStableObjects", 
        NewFamily( "stable objects" ) );

BindGlobal( "FamilyOfStableMorphisms", 
        NewFamily( "stable morphisms" ) );

InstallMethod( AsStableCategoryObject, 
        "for a quiver algebra representation", 
        [ IsQuiverRepresentation, IsStableCategoryModuloProjectives ], 
        function ( R, C )
    local   type,  obj;
    
    type := NewType( FamilyOfStableObjects,
                   IsStableCategoryModuloProjectivesObject and IsComponentObjectRep and IsAttributeStoringRep );
    obj := rec( );
    ObjectifyWithAttributes( obj, type,
            OriginalObject, R );
    Add( C, obj );
    
    return obj;
end
  );

InstallMethod( AsStableCategoryMorphism, 
        "for a quiver algebra representation homomorphism", 
        [ IsQuiverRepresentationHomomorphism, IsStableCategoryModuloProjectives ], 
        function ( f, C )
    local   type,  obj;
    
    type := NewType( FamilyOfStableMorphisms,
                   IsStableCategoryModuloProjectivesMorphism and IsComponentObjectRep and IsAttributeStoringRep );
    obj := rec( );
    ObjectifyWithAttributes( obj, type,
            OriginalMorphism, f, 
            Source, AsStableCategoryObject( Source( f ), C ), 
            Range, AsStableCategoryObject( Range( f ), C ) );
    Add( C, obj );
    
    return obj;
end
  );


InstallMethod( StableHomIdealInclusion,
               "for stable homomorphism space modulo projectives",
               [ IsStableHomSpaceModuloProjectives ],
function( hom )
    local   M,  N,  f;

  M := OriginalObject( Source( hom ) );
  N := OriginalObject( Range( hom ) );
  f := ProjectiveCover( N );
  
  return ImageEmbedding( Hom( M, f ) );
end
  );

InstallMethod( StableHomProjection,
               "for stable homomorphism space modulo projectives",
               [ IsStableHomSpaceModuloProjectives ],
function( hom )
    return CokernelProjection( StableHomIdealInclusion( hom ) );
end
  );

InstallMethod( StableCategoryModuloProjectives,
               "for quiver algebra category",
               [ IsQuiverRepresentationCategory ],
function( C )
    local   A,  cat,  equal_objects,  equal_morphisms,  zero_object,  
            zero_morphism,  identity_morphism,  pre_compose,  
            addition,  additive_inverse,  direct_sum,  direct_sum_inj,  
            direct_sum_proj,  to_be_finalized;
    
  A := AlgebraOfCategory( C );
  
  cat := CreateCapCategory( Concatenation( "stable quiver representations over ", String( A ) ) );
  SetFilterObj( cat, IsStableCategoryModuloProjectives );
  SetAlgebraOfCategory( cat, A );
  SetUnderlyingField( cat, UnderlyingField( C ) );
#  SetVectorSpaceCategory( cat, vecspace_cat );
  
  
  equal_objects := function( R1, R2 )
      return OriginalObject( R1 ) = OriginalObject( R2 );
  end;
  AddIsEqualForObjects( cat, equal_objects );
  
  equal_morphisms := function( m1, m2 )
      local p;
      p := StableHomProjection( Hom( Source( m1 ), Range( m2 ) ) );
      
      return ImageElm( p, OriginalMorphism( m1 ) ) = ImageElm( p, OriginalMorphism( m2 ) );
  end;
  AddIsEqualForMorphisms( cat, equal_morphisms );

  zero_object := function()
    return AsStableCategoryObject( ZeroObject( C ), cat );
  end;
  AddZeroObject( cat, zero_object );

  zero_morphism := function( R1, R2 )
    return AsStableCategoryMorphism( ZeroMorphism( OriginalObject( R1 ), OriginalObject( R2 ) ), cat );
  end;
  AddZeroMorphism( cat, zero_morphism );

  identity_morphism := function( R )
    return AsStableCategoryMorphism( IdentityMorphism( OriginalObject( R ) ), cat ); 
  end;
  AddIdentityMorphism( cat, identity_morphism );

  pre_compose := function( m1, m2 )
      return AsStableCategoryMorphism( PreCompose( OriginalMorphism( m1 ), OriginalMorphism( m2 ) ), cat );
  end;
  AddPreCompose( cat, pre_compose );

  addition := function( m1, m2 )
      return AsStableCategoryMorphism( AdditionForMorphisms( OriginalMorphism( m1 ), OriginalMorphism( m2 ) ), cat );
  end;
  AddAdditionForMorphisms( cat, addition );

  additive_inverse := function( m )
      return AsStableCategoryMorphism( AdditiveInverseForMorphisms( OriginalMorphism( m ) ), cat );
  end;
  AddAdditiveInverseForMorphisms( cat, additive_inverse );
  
  # 
  # Cannot find any method for checking if a morphism is a monomorphism
  # or an epimorphism, so we cannot implement LiftAlongMonomrophism and
  # ColiftAlongEpimorphism. 
  #

  direct_sum := function( summands )
      return AsStableCategoryObject( DirectSum( List( summands, OriginalObject ) ), cat );
  end;
  AddDirectSum( cat, direct_sum );

  direct_sum_inj := function( summands, i, sum )
      local   h;
      
      h := InjectionOfCofactorOfDirectSumWithGivenDirectSum
         ( List( summands, OriginalObject ), i, OriginalObject( sum ) ); 
    
      return AsStableCategoryMorphism( h );
  end;
  AddInjectionOfCofactorOfDirectSumWithGivenDirectSum( cat, direct_sum_inj );
  
  direct_sum_proj := function( summands, i, sum )
      local   h;
      
      h := ProjectionInFactorOfDirectSumWithGivenDirectSum
         ( List( summands, OriginalObject ), i, OriginalObject( sum ) ); 
    
      return AsStableCategoryMorphism( h );
  end;
  AddProjectionInFactorOfDirectSumWithGivenDirectSum( cat, direct_sum_proj );
  
  # The object/morphism constructors check whether the input is well-defined or not.
  # So if it has been created then it is well-defined
  AddIsWellDefinedForObjects( cat, ReturnTrue );
  AddIsWellDefinedForMorphisms( cat, ReturnTrue );

  to_be_finalized := ValueOption( "FinalizeCategory" );
  if to_be_finalized <> false then
    to_be_finalized := true;
  fi;
   
  if to_be_finalized then
    Finalize( cat );
  fi;
  
  return cat;
    
end );

InstallMethod( String, 
        "for an object in a stable category modulo projectives ",
               [ IsStableCategoryModuloProjectivesObject ],
function( M )
    return Concatenation( "(", String( OriginalObject( M ) ), ")/Proj" ); 
end
  );

InstallMethod( String, 
        "for a morphism in a stable category modulo projectives ",
               [ IsStableCategoryModuloProjectivesMorphism ],
function( f )
    return Concatenation( "(", String( Source( f ) ), ")",
                   "->", 
                   String( Range( f ) ), ")" ); 
end
  );

InstallMethod( \=, 
        "for two morphisms in a stable category modulo projectives ",
               [ IsStableCategoryModuloProjectivesMorphism, IsStableCategoryModuloProjectivesMorphism ],
function( m1, m2 )
  return IsIdenticalObj( CapCategory( m1 ), CapCategory( m2 ) )
         and Source( m1 ) = Source( m2 )
         and Range( m1 ) = Range( m2 )
         and IsEqualForMorphisms( m1, m2 );
end
  );