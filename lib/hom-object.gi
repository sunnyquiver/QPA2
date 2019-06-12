InstallMethod( String, [ IsHomObject ], 1000,
function( hom )
  return Concatenation( "Hom(", String( Source( hom ) ),
                        ", ", String( Range( hom ) ), ")" );
end );

InstallMethod( PrintObj, [ IsHomObject ], 1000,
function( hom )
  Print( String( hom ) );
end );

InstallMethod( ViewObj, [ IsHomObject ], 1000,
function( hom )
  Print( String( hom ) );
end );

InstallMethod( \=, [ IsHomObject, IsHomObject ],
               1000,
function( hom1, hom2 )
  return IsIdenticalObj( HomCategory( hom1 ), HomCategory( hom2 ) )
         and Source( hom1 ) = Source( hom2 )
         and Range( hom1 ) = Range( hom2 );
end );

InstallMethod( MorphismByLinearTransformation, "for hom object and linear transformation",
               [ IsHomObject, IsLinearTransformation ],
function( hom, T )
  local M, N, f;
  M := Source( hom );
  N := Range( hom );
  f := function( m )
    local m_vec, T_m_vec, T_m;
    # move m down to M's underlying vector space
    m_vec := Coefficients( CanonicalBasis( M ), m );
    # apply T:
    T_m_vec := ImageElm( T, m_vec );
    # move the result up to the module N:
    T_m := LinearCombination( CanonicalBasis( N ), T_m_vec );
    return T_m;
  end;
  return MorphismByFunction( hom, f );
end );

