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
