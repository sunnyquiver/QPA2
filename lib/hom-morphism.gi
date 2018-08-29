InstallMethod( Hom, "for field category morphism and field category object",
               [ IsFieldCategoryMorphism, IsFieldCategoryObject ],
function( f, Y )
  #      f
  # X1 ----> X2
  #   \     /
  #    .   .
  #      Y
  #
  # f* : Hom( X2, Y ) -> Hom( X1, Y )
  #            g     |-> PreCompose( f, g )
  local X1, X2;
  if not IsIdenticalObj( CapCategory( f ), CapCategory( Y ) ) then
    Error( "morphism and object from different categories" );
  fi;
  X1 := Source( f );
  X2 := Range( f );
  return LinearTransformationByFunction( Hom( X2, Y ),
                                         Hom( X1, Y ),
                                         g -> PreCompose( f, g ) );
end );

InstallMethod( Hom, "for field category object and field category morphism",
               [ IsFieldCategoryObject, IsFieldCategoryMorphism ],
function( X, g )
  #      X
  #    /   \
  #   .     .
  # Y1 ----> Y2
  #      g
  #
  # g_* : Hom( X, Y1 ) -> Hom( X, Y2 )
  #             f     |-> PreCompose( f, g )
  local Y1, Y2;
  if not IsIdenticalObj( CapCategory( X ), CapCategory( g ) ) then
    Error( "object and morphism from different categories" );
  fi;
  Y1 := Source( g );
  Y2 := Range( g );
  return LinearTransformationByFunction( Hom( X, Y1 ),
                                         Hom( X, Y2 ),
                                         f -> PreCompose( f, g ) );
end );

InstallMethod( Hom, "for field category morphism and field category morphism",
               [ IsFieldCategoryMorphism, IsFieldCategoryMorphism ],
function( f, g )
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
  local X1, X2, Y1, Y2;
  if not IsIdenticalObj( CapCategory( f ), CapCategory( g ) ) then
    Error( "morphisms from different categories" );
  fi;
  X1 := Source( f ); X2 := Range( f );
  Y1 := Source( g ); Y2 := Range( g );
  return LinearTransformationByFunction( Hom( X2, Y1 ),
                                         Hom( X1, Y2 ),
                                         h -> PreCompose( [ f, h, g ] ) );
end );