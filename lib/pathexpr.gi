DeclareRepresentation( "IsPathExprRep", IsComponentObjectRep,
                       [ "paths", "coefficients" ] );

InstallGlobalFunction( PathExpr,
function( coefficients, paths )
  local fam;
  if Length( paths ) = 1 and IsOne( coefficients[ 1 ] ) then
    return paths[ 1 ];
  else
    fam := FamilyObj( paths[ 1 ] );
    return Objectify( NewType( fam, IsPathExpr and IsPathExprRep ),
                      rec( paths := paths,
                           coefficients := coefficients ) );
  fi;
end );

InstallMethod( Coefficients, "for path expression",
               [ IsPathExpr and IsPathExprRep ],
function( e )
  return e!.coefficients;
end );
InstallMethod( Coefficients, "for path",
               [ IsPath ],
function( p )
  return [ 1 ];
end );

InstallMethod( Paths, "for path expression",
               [ IsPathExpr and IsPathExprRep ],
function( e )
  return e!.paths;
end );
InstallMethod( Paths, "for path",
               [ IsPath ],
function( p )
  return [ p ];
end );

InstallMethod( AdditiveInverse, "for path expression",
               [ IsPathExpr ],
function( e )
  return PathExpr( List( Coefficients( e ), AdditiveInverse ),
                   Paths( e ) );
end );

InstallMethod( \+, "for path expressions", IsIdenticalObj,
               [ IsPathExpr, IsPathExpr ],
function( e1, e2 )
  local Cs1, Ps1, Cs2, Ps2, Cs, Ps, i, j, c1, c2, p1, p2;
  Cs1 := Coefficients( e1 ); Ps1 := Paths( e1 );
  Cs2 := Coefficients( e2 ); Ps2 := Paths( e2 );
  Cs := []; Ps := [];
  i := 1; j := 1;
  while i <= Length( Cs1 ) and j <= Length( Cs2 ) do
    c1 := Cs1[ i ]; c2 := Cs2[ j ];
    p1 := Ps1[ i ]; p2 := Ps2[ j ];
    if p1 = p2 then
      if not IsZero( c1 + c2 ) then
        Add( Cs, c1 + c2 ); Add( Ps, p1 );
        i := i + 1;
        j := j + 1;
      fi;
    elif p1 < p2 then
      Add( Cs, c1 ); Add( Ps, p1 );
      i := i + 1;
    else
      Add( Cs, c2 ); Add( Ps, p2 );
      j := j + 1;
    fi;
  od;
  while i <= Length( Cs1 ) do
    Add( Cs, Cs1[ i ] ); Add( Ps, Ps1[ i ] );
    i := i + 1;
  od;
  while j <= Length( Cs2 ) do
    Add( Cs, Cs2[ j ] ); Add( Ps, Ps2[ j ] );
    j := j + 1;
  od;
  return PathExpr( Cs, Ps );
end );

InstallMethod( PrintObj, "for path expression",
               [ IsPathExpr ],
function( e )
  local Cs, Ps, i;
  Cs := Coefficients( e );
  Ps := Paths( e );
  for i in [ 1 .. Length( Cs ) ] do
    if i > 1 then Print( " + " ); fi;
    Print( Cs[ i ], "*");
    View( Ps[ i ] );
  od;
end );
