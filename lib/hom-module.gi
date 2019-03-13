InstallMethod( HomFunctor, "for side and quiver module categories",
               [ IsSide, IsQuiverModuleCategory, IsQuiverModuleCategory ],
function( side, cat1, cat2 )
  local side_result, rep1, rep2, rep_cat1, rep_cat2, hom_rep, 
        as_module;
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
  return PreComposeFunctors( PreComposeFunctors( [ rep1, rep2 ], hom_rep ),
                             as_module );
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

