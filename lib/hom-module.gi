InstallMethod( HomFunctor, "for side and quiver module categories",
               [ IsSide, IsQuiverModuleCategory, IsQuiverModuleCategory ],
function( side, cat1, cat2 )
  local side_result, rep1, rep2, rep_cat1, rep_cat2, hom_rep, 
        as_module, compute_hom_functor, range, hom_functor, hom;
  if side = Side( cat1 ) and side = Side( cat2 ) then
    return HomFunctor( cat1 );
  fi;
  if Side( cat1 ) = LEFT_RIGHT and Side( cat2 ) = LEFT_RIGHT then
    side_result := LEFT_RIGHT;
  elif Side( cat1 ) = LEFT_RIGHT then
    side_result := side;
  elif Side( cat2 ) = LEFT_RIGHT then
    side_result := Opposite( side );
  fi;
  rep1 := UnderlyingRepresentationFunctor( cat1 );
  rep2 := UnderlyingRepresentationFunctor( cat2 );
  rep_cat1 := UnderlyingRepresentationCategory( cat1 );
  rep_cat2 := UnderlyingRepresentationCategory( cat2 );
  hom_rep := HomFunctor( Int( side ), rep_cat1, rep_cat2 );
  as_module := AsModuleFunctor( side_result, AsCapCategory( Range( hom_rep ) ) );
  compute_hom_functor := PreComposeFunctors( PreComposeFunctors( [ rep1, rep2 ], hom_rep ),
                                             as_module );
    range := AsCapCategory( Range( compute_hom_functor ) );
  hom_functor := CapFunctor( "Hom",
                             [ [ cat1, true ], [ cat2, false ] ],
                             range );
  AddObjectFunction( hom_functor, function( M1, M2 )
    local hom;
    hom := ApplyFunctor( compute_hom_functor, M1, M2 );
    SetFilterObj( hom, IsHomModule );
    SetSource( hom, M1 );
    SetRange( hom, M2 );
    return hom;
  end );
  AddMorphismFunction( hom_functor, FunctorMorphismOperation( compute_hom_functor ) );
  return hom_functor;
end );

InstallMethod( HomFunctor, "for quiver module category",
               [ IsQuiverModuleCategory ],
function( cat )
  local rep, rep_cat, hom_rep;
  rep := UnderlyingRepresentationFunctor( cat );
  rep_cat := UnderlyingRepresentationCategory( cat );
  hom_rep := HomFunctor( rep_cat );
  return PreComposeFunctors( [ rep, rep ], hom_rep );
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

