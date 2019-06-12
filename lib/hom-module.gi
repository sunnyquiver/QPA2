BindGlobal( "FamilyOfEnrichedQuiverModuleHomomorphisms",
            NewFamily( "enriched quiver module homomorphisms" ) );
BindGlobal( "FamilyOfHomModules",
            CollectionsFamily( FamilyOfEnrichedQuiverModuleHomomorphisms ) );

# We need this because the builtin global function Image uses
# the FamilySource attribute to check whether a given map can
# be applied to a given element.
SetFamilySource( FamilyOfEnrichedQuiverModuleHomomorphisms,
                 FamilyOfQuiverModuleElements );

InstallMethod( HomFunctor, "for side and quiver module categories",
               [ IsSide, IsQuiverModuleCategory, IsQuiverModuleCategory ],
function( side, cat1, cat2 )
  local K, acting_algebras, range, hom_functor, hom_alg, hom_cat;
  if UnderlyingField( cat1 ) <> UnderlyingField( cat2 ) then
    Error( "categories over different fields" );
  fi;
  K := UnderlyingField( cat1 );
  if ActingAlgebra( side, cat1 ) <> ActingAlgebra( side, cat2 ) then
    Error( "incompatible categories for ", side, " hom functor" );
  fi;
  hom_alg := ActingAlgebra( side, cat1 );
  if side = Side( cat1 ) and side = Side( cat2 ) then
    return HomFunctor( cat1 );
  fi;
  if hom_alg <> fail then
    hom_cat := ModuleCategory( side, hom_alg );
  else
    hom_cat := CategoryOfVectorSpaces( K );
  fi;
  if side = LEFT then
    acting_algebras := [ RightActingAlgebra( cat1 ), RightActingAlgebra( cat2 ) ];
  else # side = RIGHT
    acting_algebras := [ LeftActingAlgebra( cat2 ), LeftActingAlgebra( cat1 ) ];
  fi;
  range := ModuleCategory( acting_algebras );
  hom_functor := CapFunctor( "Hom", [ [ cat1, true ], [ cat2, false ] ], range );
  AddObjectFunction( hom_functor, function( M, N )
    local hom, type;
    hom := rec();
    type := NewType( FamilyOfHomModules,
                     IsHomModule and IsComponentObjectRep and IsAttributeStoringRep );
    ObjectifyWithAttributes( hom, type,
                             Source, M,
                             Range, N,
                             HomCategory, hom_cat,
                             UnderlyingHomFunctor, HomFunctor( hom_cat ),
                             HomSide, side );
    Add( range, hom );
    return Intern( hom );
  end );
  AddMorphismFunction( hom_functor, function( hom1, f, g, hom2 )
    local X1, X2, Y1, Y2;
    #     f
    # X1 --> X2
    #       /
    #      /
    #     / h
    #    /
    #   .
    # Y1 --> Y2
    #     g
    #
    # Hom( f, g ) : Hom( X2, Y1 ) -> Hom( X1, Y2 )
    #                       h    |-> PreCompose( [ f, h, g ] )
    X1 := Source( f ); X2 := Range( f );
    Y1 := Source( g ); Y2 := Range( g );
    return QuiverModuleHomomorphism( hom1, hom2,
                                     h -> PreCompose( f, h, g ) );
  end );
  return hom_functor;
end );

InstallMethod( UnderlyingRepresentation,
               [ IsHomModule ],
function( hom )
  local side, uhom, M, N, rep_cat, M_, N_, rep;
  side := HomSide( hom );
  uhom := UnderlyingHomFunctor( hom );
  M := Source( hom );
  N := Range( hom );
  rep_cat := UnderlyingRepresentationCategory( CapCategory( hom ) );
  if Side( M ) = LEFT_RIGHT and Side( N ) = LEFT_RIGHT then
    M_ := AsRepresentationOfModules( side, M );
    N_ := AsRepresentationOfModules( side, N );
    rep := MapRepresentation( uhom, [ M_, N_ ] );
    if side = RIGHT then
      rep := TensorFlipRestriction( rep );
    fi;
  elif Side( M ) = LEFT_RIGHT then
    M_ := AsRepresentationOfModules( side, M );
    rep := MapRepresentation( Mi -> Hom( Mi, N ), M_, rep_cat );
  elif Side( N ) = LEFT_RIGHT then
    N_ := AsRepresentationOfModules( side, N );
    rep := MapRepresentation( Ni -> Hom( M, Ni ), N_, rep_cat );
  else
    M_ := UnderlyingRepresentation( M );
    N_ := UnderlyingRepresentation( N );
    rep := MapRepresentation( uhom, [ M_, N_ ] );
    if side = RIGHT then
      rep := TensorFlipRestriction( rep );
    fi;
  fi;
  return rep;
end );

InstallMethod( EnrichedQuiverModuleHomomorphism,
               [ IsSide, IsQuiverModule, IsQuiverModule, IsList ],
function( side, M, N, morphisms )
  local hom;
  hom := Hom( side, M, N );
  return QuiverModuleElement( hom, morphisms );
end );

InstallMethod( AsModuleElement,
               [ IsQuiverRepresentationElement, IsHomModule ],
function( e, hom )
  local f, elem_cat;
  if RepresentationOfElement( e ) <> UnderlyingRepresentation( hom ) then
    Error( "Element is not from the underlying representation of the module" );
  fi;
  elem_cat := IsQuiverModuleElement ^ Side( hom );
  f := rec();
  ObjectifyWithAttributes( f, NewType( FamilyOfQuiverModuleElements,
                                        elem_cat and IsEnrichedQuiverModuleHomomorphism and IsQuiverModuleRep ),
                           UnderlyingRepresentationElement, e,
                           ModuleOfElement, hom,
                           Side, Side( hom ) );
  return f;
end );

InstallMethod( HomSide, "for enriched quiver module homomorphism",
               [ IsEnrichedQuiverModuleHomomorphism ],
               f -> HomSide( ModuleOfElement( f ) ) );

InstallMethod( Source, "for enriched quiver module homomorphism",
               [ IsEnrichedQuiverModuleHomomorphism ],
               f -> Source( ModuleOfElement( f ) ) );

InstallMethod( Range, "for enriched quiver module homomorphism",
               [ IsEnrichedQuiverModuleHomomorphism ],
               f -> Range( ModuleOfElement( f ) ) );

InstallMethod( String, "for enriched quiver module homomorphism",
               [ IsEnrichedQuiverModuleHomomorphism ],
function( f )
  return Concatenation( "enriched ", String( HomSide( f ) ), " homomorphism ",
                        "(", String( Source( f ) ), ")",
                        "->",
                        "(", String( Range( f ) ), ")" );
end );

InstallMethod( ImageElm, "for enriched quiver module homomorphism and quiver module element",
               [ IsEnrichedQuiverModuleHomomorphism, IsQuiverModuleElement ],
function( f, e )
  local hom, hom_rep, hom_rep_Q, hom_rep_Qs, side, uhom, M, N, e_, M_, 
        N_, elms, w, summands, v, v_op, vw, rep_elm;
  hom := ModuleOfElement( f );
  hom_rep := UnderlyingRepresentation( hom );
  hom_rep_Q := QuiverOfRepresentation( hom_rep );
  hom_rep_Qs := ProductQuiverFactors( hom_rep_Q );
  side := HomSide( hom );
  uhom := UnderlyingHomFunctor( hom );
  M := Source( hom );
  N := Range( hom );
  if Side( M ) = LEFT_RIGHT and Side( N ) = LEFT_RIGHT then
    e_ := AsRepresentationOfModulesElement( side, e );
    M_ := AsRepresentationOfModules( side, M );
    N_ := AsRepresentationOfModules( side, N );
    elms := [];
    for w in Vertices( QuiverOfRepresentation( N_ ) ) do
      summands := [];
      for v in Vertices( QuiverOfRepresentation( M_ ) ) do
        v_op := OppositePath( v );
        if side = LEFT then
          vw := PathInProductQuiver( hom_rep_Q, [ v_op, w ] );
        else
          vw := PathInProductQuiver( hom_rep_Q, [ w, v_op ] );
        fi;
        Add( summands, ImageElm( ElementVector( f, vw ),
                                 ElementVector( e_, v ) ) );
      od;
      elms[ VertexIndex( w ) ] := Sum( summands );
    od;
    rep_elm := QuiverRepresentationElement( N_, elms );
    return RepresentationOfModulesElementAsBimoduleElement( rep_elm );
  elif Side( M ) = LEFT_RIGHT then
    e_ := AsRepresentationOfModulesElement( side, e );
    return Sum( ListN( ElementVectors( f ), ElementVectors( e_ ),
                       ImageElm ) );
  elif Side( N ) = LEFT_RIGHT then
    N_ := AsRepresentationOfModules( side, N );
    elms := List( ElementVectors( f ), f_i -> ImageElm( f_i, e ) );
    rep_elm := QuiverRepresentationElement( N_, elms );
    return RepresentationOfModulesElementAsBimoduleElement( rep_elm );
  else
    e_ := UnderlyingRepresentationElement( e );
    M_ := UnderlyingRepresentation( M );
    N_ := UnderlyingRepresentation( N );
    for w in Vertices( QuiverOfRepresentation( N_ ) ) do
      summands := [];
      for v in Vertices( QuiverOfRepresentation( M_ ) ) do
        v_op := OppositePath( v );
        if side = LEFT then
          vw := PathInProductQuiver( hom_rep_Q, [ v_op, w ] );
        else
          vw := PathInProductQuiver( hom_rep_Q, [ w, v_op ] );
        fi;
        Add( summands, ImageElm( ElementVector( f, vw ),
                                 ElementVector( e_, v ) ) );
      od;
      elms[ VertexIndex( w ) ] := Sum( summands );
    od;
    rep_elm := QuiverRepresentationElement( N_, elms );
    return AsModuleElement( rep_elm, N );
  fi;
end );

InstallMethod( AsLinearTransformation, "for enriched quiver module homomorphism",
               [ IsEnrichedQuiverModuleHomomorphism ],
function( f )
  local hom, M, N, Bm, Bn, Vm, Vn, fun;
  hom := ModuleOfElement( f );
  M := Source( hom );
  N := Range( hom );
  Bm := CanonicalBasis( M );
  Bn := CanonicalBasis( N );
  Vm := AsQPAVectorSpace( M );
  Vn := AsQPAVectorSpace( N );
  fun := function( v )
    local m, n;
    m := LinearCombination( Bm, v );
    n := ImageElm( f, m );
    return Coefficients( Bn, n );
  end;
  return LinearTransformationByFunction( Vm, Vn, fun );
end );

InstallMethod( MorphismByFunction, "for hom module and function",
               [ IsHomModule, IsFunction ],
function( hom, f )
  local side, hom_rep, hom_rep_Q, M, N, M_, N_, Qm, Qn, morphisms, vw, 
        vw_factors, v_op, v, w, i, j, f_ij, morph_ij, f_i, morph_i, f_j, 
        morph_j;

  side := HomSide( hom );
  hom_rep := UnderlyingRepresentation( hom );
  hom_rep_Q := QuiverOfRepresentation( hom_rep );
  M := Source( hom );
  N := Range( hom );

  if Side( M ) = LEFT_RIGHT and Side( N ) = LEFT_RIGHT then
    M_ := AsRepresentationOfModules( side, M );
    N_ := AsRepresentationOfModules( side, N );
    Qm := QuiverOfRepresentation( M_ );
    Qn := QuiverOfRepresentation( N_ );
    morphisms := [];
    for vw in Vertices( hom_rep_Q ) do
      vw_factors := ProductPathFactors( vw );
      if side = LEFT then
        v_op := vw_factors[ 1 ];
        w := vw_factors[ 2 ];
      else
        v_op := vw_factors[ 2 ];
        w := vw_factors[ 1 ];
      fi;
      v := OppositePath( v );
      i := VertexIndex( v );
      j := VertexIndex( w );
      f_ij := function( m_i )
        local m_, m, n, n_, n_j;
        m_ := QuiverRepresentationElement( M_, [ Vertex( Qm, i ) ], m_i );
        m := RepresentationOfModulesElementAsBimoduleElement( m_ );
        n := f( m );
        n_ := AsRepresentationOfModulesElement( side, n );
        n_j := ElementVector( n_, j );
        return n_j;
      end;
      morph_ij := QuiverModuleHomomorphism( VectorSpaceOfRepresentation( M_, i ),
                                            VectorSpaceOfRepresentation( N_, j ),
                                            f_ij );
      Add( morphisms, morph_ij );
    od;
    return EnrichedQuiverModuleHomomorphism( side, M, N, morphisms );
  elif Side( M ) = LEFT_RIGHT then
    M_ := AsRepresentationOfModules( side, M );
    morphisms := [];
    Qm := QuiverOfRepresentation( M_ );
    for i in [ 1 .. NumberOfVertices( Qm ) ] do
      f_i := function( m_i )
        local m_, m, n;
        m_ := QuiverRepresentationElement( M_, [ Vertex( Qm, i ) ], m_i );
        m := RepresentationOfModulesElementAsBimoduleElement( m_ );
        n := f( m );
        return n;
      end;
      morph_i := QuiverModuleHomomorphism( VectorSpaceOfRepresentation( M_, i ), N, f_i );
      Add( morphisms, morph_i );
    od;
    return EnrichedQuiverModuleHomomorphism( side, M, N, morphisms );
  elif Side( N ) = LEFT_RIGHT then
    N_ := AsRepresentationOfModules( side, N );
    morphisms := [];
    Qn := QuiverOfRepresentation( N_ );
    for j in [ 1 .. NumberOfVertices( Qn ) ] do
      f_j := function( m )
        local n, n_, n_j;
        n := f( m );
        n_ := AsRepresentationOfModulesElement( side, n );
        n_j := ElementVector( n_, j );
        return n_j;
      end;
      morph_j := QuiverModuleHomomorphism( M, VectorSpaceOfRepresentation( N_, j ), f_j );
      Add( morphisms, morph_j );
    od;
    return EnrichedQuiverModuleHomomorphism( side, M, N, morphisms );
  else
    M_ := UnderlyingRepresentation( M );
    N_ := UnderlyingRepresentation( N );
    morphisms := [];
    Qm := QuiverOfRepresentation( M_ );
    Qn := QuiverOfRepresentation( N_ );
    for vw in Vertices( hom_rep_Q ) do
      vw_factors := ProductPathFactors( vw );
      if side = LEFT then
        v := vw_factors[ 1 ];
        w := vw_factors[ 2 ];
      else
        v := vw_factors[ 2 ];
        w := vw_factors[ 1 ];
      fi;
      i := VertexIndex( v );
      j := VertexIndex( w );
      f_ij := function( m_i )
        local m_, m, n, n_, n_j;
        m_ := QuiverRepresentationElement( M_, [ Vertex( Qm, i ) ], m_i );
        m := AsModuleElement( m_, M );
        n := f( m );
        n_ := UnderlyingRepresentationElement( n );
        n_j := ElementVector( n_, j );
        return n_j;
      end;
      morph_ij := LinearTransformationByFunction( VectorSpaceOfRepresentation( M_, i ),
                                                  VectorSpaceOfRepresentation( N_, j ),
                                                  f_ij );
      Add( morphisms, morph_ij );
    od;
    return EnrichedQuiverModuleHomomorphism( side, M, N, morphisms );
  fi;
end );

InstallMethod( PreCompose, "for two enriched quiver module homomorphisms",
               [ IsEnrichedQuiverModuleHomomorphism, IsEnrichedQuiverModuleHomomorphism ],
function( f1, f2 )
  local M1, M2, T1, T2;
  if Range( f1 ) <> Source( f2 ) then
    Error( "morphisms are not composable" );
  fi;
  M1 := Source( f1 );
  M2 := Range( f2 );
  T1 := AsLinearTransformation( f1 );
  T2 := AsLinearTransformation( f2 );
  return MorphismByLinearTransformation( M1, M2, PreCompose( T1, T2 ) );
end );

# TODO precompose, isomorphism{to,from}{left,right}module

InstallMethod( HomFunctor, "for side, quiver module and quiver module category",
               [ IsSide, IsQuiverModule, IsQuiverModuleCategory ],
function( side, M, cat )
  return FixFunctorArguments( HomFunctor( side, CapCategory( M ), cat ),
                              [ M, fail ] );
end );

InstallMethod( HomFunctor, "for side, quiver module and quiver module category",
               [ IsSide, IsQuiverModuleCategory, IsQuiverModule ],
function( side, cat, M )
  return FixFunctorArguments( HomFunctor( side, cat, CapCategory( M ) ),
                              [ fail, M ] );
end );

InstallMethod( Hom, "for side and quiver modules",
               [ IsSide, IsQuiverModule, IsQuiverModule ],
function( side, M, N )
  return ApplyFunctor( HomFunctor( side, CapCategory( M ), CapCategory( N ) ),
                       M, N );
end );

InstallMethod( Hom, "for side, quiver module and quiver module homomorphism",
               [ IsSide, IsQuiverModule, IsQuiverModuleHomomorphism ],
function( side, M, g )
  return ApplyFunctor( HomFunctor( side, CapCategory( M ), CapCategory( g ) ),
                       IdentityMorphism( M ), g );
end );

InstallMethod( Hom, "for side, quiver module homomorphism and quiver module",
               [ IsSide, IsQuiverModuleHomomorphism, IsQuiverModule ],
function( side, f, N )
  return ApplyFunctor( HomFunctor( side, CapCategory( f ), CapCategory( N ) ),
                       f, IdentityMorphism( N ) );
end );

InstallMethod( Hom, "for side and quiver module homomorphisms",
               [ IsSide, IsQuiverModuleHomomorphism, IsQuiverModuleHomomorphism ],
function( side, f, g )
  return ApplyFunctor( HomFunctor( side, CapCategory( f ), CapCategory( g ) ),
                       f, g );
end );

