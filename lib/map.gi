InstallMethod( MapRepresentation, "for functions, representation and representation category",
               [ IsFunction, IsFunction, IsQuiverRepresentation,
                 IsQuiverRepresentationCategory ],
function( vfun, afun, R, cat )
  return QuiverRepresentation( cat,
                               List( VectorSpacesOfRepresentation( R ), vfun ),
                               List( MapsOfRepresentation( R ), afun ) );
end );

InstallMethod( MapRepresentation, "for function, representation and representation category",
               [ IsFunction, IsQuiverRepresentation,
                 IsQuiverRepresentationCategory ],
function( f, R, cat )
  return MapRepresentation( f, f, R, cat );
end );

InstallMethod( MapRepresentation, "for three functions, representation homomorphism and representation category",
               [ IsFunction, IsFunction, IsFunction, IsQuiverRepresentationHomomorphism,
                 IsQuiverRepresentationCategory ],
function( mfun, afun_source, afun_range, m, cat )
  local maps, objs_source, objs_range, arrow_maps_source, 
        arrow_maps_range, source, range;
  maps := List( MapsOfRepresentationHomomorphism( m ), mfun );
  objs_source := List( maps, Source );
  objs_range := List( maps, Range );
  arrow_maps_source := List( MapsOfRepresentation( Source( m ) ), afun_source );
  arrow_maps_range := List( MapsOfRepresentation( Range( m ) ), afun_range );
  source := QuiverRepresentation( cat, objs_source, arrow_maps_source );
  range := QuiverRepresentation( cat, objs_range, arrow_maps_range );
  return QuiverRepresentationHomomorphism( source, range, maps );
end );

InstallMethod( MapRepresentation, "for two functions, representation homomorphism and representation category",
               [ IsFunction, IsFunction, IsQuiverRepresentationHomomorphism,
                 IsQuiverRepresentationCategory ],
function( mfun, afun, m, cat )
  return MapRepresentation( mfun, afun, afun, m, cat );
end );

InstallMethod( MapRepresentation, "for function, representation homomorphism and representation category",
               [ IsFunction, IsQuiverRepresentationHomomorphism,
                 IsQuiverRepresentationCategory ],
function( f, m, cat )
  return MapRepresentation( f, f, m, cat );
end );

InstallMethod( MapRepresentation, "for functions, dense list and representation category",
               [ IsFunction, IsFunction, IsDenseList,
                 IsQuiverRepresentationCategory ],
function( vfun, afun, Rs, cat )
  local T, Q_T, objects, maps;
  # TODO check that Rs consists of representations
  # and is consistent with cat
  T := AlgebraOfCategory( cat );
  Q_T := QuiverOfAlgebra( T );
  objects := List( Vertices( Q_T ),
                   v -> vfun( ListN( Rs, ProductPathFactors( v ), VectorSpaceOfRepresentation ) ) );
  maps := List( Arrows( Q_T ),
                a -> afun( ListN( Rs, ProductPathFactors( a ), MapForPath ) ) );
  return QuiverRepresentation( cat, objects, maps );
end );

InstallMethod( MapRepresentation, "for three functions, dense list and representation category",
               [ IsFunction, IsFunction, IsFunction, IsDenseList,
                 IsQuiverRepresentationCategory ],
function( mfun, afun_source, afun_range, ms, cat )
  local T, Q_T, sources, ranges, maps, objs_source, objs_range, 
        arrow_maps_source, arrow_maps_range, source, range;
  # TODO check that ms consists of representation homomorphisms
  # and is consistent with cat
  T := AlgebraOfCategory( cat );
  Q_T := QuiverOfAlgebra( T );
  sources := List( ms, Source );
  ranges := List( ms, Range );
  maps := List( Vertices( Q_T ),
                v -> mfun( ListN( ms, ProductPathFactors( v ), MapForVertex ) ) );
  objs_source := List( maps, Source );
  objs_range := List( maps, Range );
  arrow_maps_source := List( Arrows( Q_T ),
                             a -> afun_source( ListN( sources, ProductPathFactors( a ), MapForPath ) ) );
  arrow_maps_range := List( Arrows( Q_T ),
                             a -> afun_range( ListN( ranges, ProductPathFactors( a ), MapForPath ) ) );
  source := QuiverRepresentation( cat, objs_source, arrow_maps_source );
  range := QuiverRepresentation( cat, objs_range, arrow_maps_range );
  return QuiverRepresentationHomomorphism( source, range, maps );
end );

InstallMethod( MapRepresentation, "for functor and quiver algebra",
               [ IsCapFunctor, IsQuiverAlgebra ],
function( F, A )
  local source_ucat, range_ucat, source_cat, range_cat, map_F, 
        apply_F, object_fun, morphism_fun;
  source_ucat := AsCapCategory( Source( F ) );
  range_ucat := AsCapCategory( Range( F ) );
  source_cat := CategoryOfQuiverRepresentationsOverVectorSpaceCategory( A, source_ucat );
  range_cat := CategoryOfQuiverRepresentationsOverVectorSpaceCategory( A, range_ucat );
  map_F := CapFunctor( Concatenation( "Map(", Name( F ), ")" ),
                       source_cat, range_cat );
  apply_F := x -> ApplyFunctor( F, x );
  object_fun := function( R )
    return MapRepresentation( apply_F, R, range_cat );
  end;
  morphism_fun := function( R1, g, R2 )
    return MapRepresentation( apply_F, g, range_cat );
  end;
  AddObjectFunction( map_F, object_fun );
  AddMorphismFunction( map_F, morphism_fun );
  return map_F;
end );

InstallMethod( MapRepresentation, "for functor and dense list",
               [ IsCapFunctor, IsDenseList ],
function( F, list )
  local sig_u, sig, algebras, algebra_indices, i, cat_u, contra, alg, 
        cat, range_alg, range_ucat, range_cat, map_F, apply_F, rep_i, 
        object_fun, morphism_fun;
  sig_u := InputSignature( F );
  if Length( list ) <> Length( sig_u ) then
    Error( "length of algebra list does not match input signature of functor" );
  fi;
  sig := [];
  algebras := [];
  algebra_indices := [];
  for i in [ 1 .. Length( sig_u ) ] do
    if list[ i ] = fail then
      sig[ i ] := sig_u[ i ];
    else
      cat_u := sig_u[ i ][ 1 ];
      contra := sig_u[ i ][ 2 ];
      alg := list[ i ];
      cat := CategoryOfQuiverRepresentationsOverVectorSpaceCategory( alg, cat_u );
      sig[ i ] := [ cat, contra ];
      if contra then
        Add( algebras, OppositeAlgebra( alg ) );
      else
        Add( algebras, alg );
      fi;
      Add( algebra_indices, i );
    fi;
  od;

  if Length( algebras ) = 0 then
    return F;
  elif Length( algebras ) = 1 then
    range_alg := algebras[ 1 ];
  else
    range_alg := TensorProductOfAlgebras( algebras );
  fi;

  range_ucat := AsCapCategory( Range( F ) );
  range_cat := CategoryOfQuiverRepresentationsOverVectorSpaceCategory
               ( range_alg, range_ucat );

  map_F := CapFunctor( Concatenation( "Map(", Name( F ), ")" ),
                       sig, range_cat );
  
  apply_F := function( args )
    return CallFuncList( ApplyFunctor, Concatenation( [ F ], args ) );
  end;

  if Length( algebras ) = 1 then
    rep_i := algebra_indices[ 1 ];
    object_fun := function( arg )
      local id_morphisms, vfun, afun;
      id_morphisms := List( arg, x -> IdentityMorphism( x ) );
      vfun := x -> apply_F( Replace( arg, rep_i, x ) );
      afun := m -> apply_F( Replace( id_morphisms, rep_i, m ) );
      return MapRepresentation( vfun, afun, arg[ rep_i ], range_cat );
    end;
    morphism_fun := function( arg )
      local morphisms, sources, ranges, afun_source, afun_range, mfun;
      morphisms := arg{ [ 2 .. Length( arg ) - 1 ] };
      sources := List( morphisms, m -> IdentityMorphism( Source( m ) ) );
      ranges := List( morphisms, m -> IdentityMorphism( Range( m ) ) );
      afun_source := m -> apply_F( Replace( sources, rep_i, m ) );
      afun_range := m -> apply_F( Replace( ranges, rep_i, m ) );
      mfun := m -> apply_F( Replace( morphisms, rep_i, m ) );
      return MapRepresentation( mfun, afun_source, afun_range, morphisms[ rep_i ], range_cat );
    end;
  else
    object_fun := function( arg )
      local id_morphisms, vfun, afun;
      id_morphisms := List( arg, x -> IdentityMorphism( x ) );
      vfun := xs -> apply_F( Replace( arg, algebra_indices, xs ) );
      afun := ms -> apply_F( Replace( id_morphisms, algebra_indices, ms ) );
      return MapRepresentation( vfun, afun, arg{ algebra_indices }, range_cat );
    end;
    morphism_fun := function( arg )
      local morphisms, sources, ranges, afun_source, afun_range, mfun;
      morphisms := arg{ [ 2 .. Length( arg ) - 1 ] };
      sources := List( morphisms, Source );
      ranges := List( morphisms, Range );
      afun_source := ms -> apply_F( Replace( sources, algebra_indices, ms ) );
      afun_range := ms -> apply_F( Replace( ranges, algebra_indices, ms ) );
      mfun := ms -> apply_F( Replace( morphisms, algebra_indices, ms ) );
      return MapRepresentation( mfun, afun_source, afun_range, morphisms{ algebra_indices }, range_cat );
    end;
  fi;
  AddObjectFunction( map_F, object_fun );
  AddMorphismFunction( map_F, morphism_fun );
  return map_F;
end );
