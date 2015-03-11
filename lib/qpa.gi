DeclareRepresentation( "IsQPAElementRep", IsComponentObjectRep,
                       [ "algebra", "paths", "coefficients" ] );

InstallGlobalFunction( QPAElement,
function( algebra, coefficients, paths )
  return Objectify( NewType( ElementsFamily( FamilyObj( algebra ) ),
                             IsQPAElement and IsQPAElementRep ),
                    rec( algebra := algebra,
                         paths := paths,
                         coefficients := coefficients ) );
end );

InstallMethod( Coefficients, "for element of quotient of path algebra",
               [ IsQPAElement and IsQPAElementRep ],
function( e )
  return e!.coefficients;
end );

InstallMethod( Paths, "for element of quotient of path algebra",
               [ IsQPAElement and IsQPAElementRep ],
function( e )
  return e!.paths;
end );

InstallMethod( Zero, "for quotient of path algebra",
               [ IsQPA ],
function( A )
  return QPAElement( A, [], [] );
end );

InstallMethod( One, "for quotient of path algebra",
               [ IsQPA ],
function( A )
  local vertices;
  vertices := Vertices( QuiverOfAlgebra( A ) );
  return QPAElement( A,
                     List( vertices, v -> One( LeftActingDomain( A ) ) ),
                     vertices );
end );

InstallMethod( Zero, "for element of quotient of path algebra",
               [ IsQPAElement and IsQPAElementRep ],
function( e )
  return Zero( e!.algebra );
end );

InstallMethod( One, "for element of quotient of path algebra",
               [ IsQPAElement and IsQPAElementRep ],
function( e )
  return One( e!.algebra );
end );

InstallMethod( AdditiveInverse, "for element of quotient of path algebra",
               [ IsQPAElement ],
function( e )
  return QPAElement( e!.algebra,
                     List( Coefficients( e ), AdditiveInverse ),
                     Paths( e ) );
end );

InstallMethod( \+, "for elements of quotient of path algebra", IsIdenticalObj,
               [ IsQPAElement, IsQPAElement ],
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
  return QPAElement( e1!.algebra, Cs, Ps );
end );

InstallMethod( \*, "for elements of quotient of path algebra", IsIdenticalObj,
               [ IsQPAElement, IsQPAElement ],
function( e1, e2 )
  #TODO
end );

InstallMethod( PrintObj, "for element of quotient of path algebra",
               [ IsQPAElement ],
function( e )
  local Cs, Ps, i;
  Cs := Coefficients( e );
  Ps := Paths( e );
  if Length( Ps ) = 0 then
    Print( "0" );
    return;
  fi;
  for i in [ 1 .. Length( Cs ) ] do
    if i > 1 then Print( " + " ); fi;
    Print( Cs[ i ], "*");
    View( Ps[ i ] );
  od;
end );

DeclareRepresentation( "IsQPARep", IsComponentObjectRep,
                       [ "field", "quiver", "relations", "pathAlgebra" ] );

InstallGlobalFunction( QPA,
function( k, Q, relations )
  local elementFam, algebraFam, algebraType, A;
  # TODO: algebra should have its own elements family,
  # but this should rely only on ( k, Q, relations).
  # If we make a new algebra with same arguments, we
  # should get the same family.
  elementFam := ElementsFamily( FamilyObj( Q ) );
  algebraFam := CollectionsFamily( elementFam );
  algebraType := NewType( algebraFam, IsQPA and IsQPARep );
  A := Objectify( algebraType,
                  rec( field := k,
                       quiver := Q,
                       relations := relations ) );
  if Length( relations ) = 0 then
    A!.pathAlgebra := A;
  else
    A!.pathAlgebra := PathAlgebra( k, Q );
  fi;
  return A;
end );

InstallMethod( QuiverOfAlgebra, "for quotient of path algebra",
               [ IsQPA and IsQPARep ],
function( A )
  return A!.quiver;
end );

InstallMethod( RelationsOfAlgebra, "for quotient of path algebra",
               [ IsQPA and IsQPARep ],
function( A )
  return A!.relations;
end );

InstallMethod( PathAlgebra, "for quotient of path algebra",
               [ IsQPA and IsQPARep ],
function( A )
  return A!.pathAlgebra;
end );

InstallMethod( LeftActingDomain, "for quotient of path algebra",
               [ IsQPA and IsQPARep ],
function( A )
  return A!.field;
end );

InstallOtherMethod( PathAlgebra, "for field and quiver",
                    [ IsField, IsQuiver ],
function( k, Q )
  return QPA( k, Q, [] );
end );

InstallMethod( DivideByList, "for element of quotient of path algebra and list",
               [ IsQPAElement, IsList ],
function( e, divisors )
  local remainder;
  remainder := Zero( e );
  #TODO
end );
