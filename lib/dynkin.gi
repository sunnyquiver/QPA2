InstallMethod( DynkinGraphAn, [ IsPosInt ],
function( n )
  return List( [ 1 .. ( n - 1 ) ], i -> [ i, i + 1 ] );
end );

InstallMethod( DynkinGraphDn, [ IsPosInt ],
function( n )
  if n < 4 then
    Error( "Dynkin type Dn only defined for n >= 4, not ", n );
  fi;
  return Concatenation( [ [ 1, 3 ],
                          [ 2, 3 ] ],
                        List( [ 3 .. ( n - 1 ) ], i -> [ i, i + 1 ] ) );
end );

InstallMethod( DynkinGraphEn, [ IsPosInt ],
function( n )
  if n = 6 then
    return [ [ 1, 2 ], [ 2, 3 ], [ 3, 4 ], [ 4, 5 ],
             [ 3, 6 ] ];
  elif n = 7 then
    return [ [ 1, 2 ], [ 2, 3 ], [ 3, 4 ], [ 4, 5 ], [ 5, 6 ],
             [ 3, 7 ] ];
  elif n = 8 then
    return [ [ 1, 2 ], [ 2, 3 ], [ 3, 4 ], [ 4, 5 ], [ 5, 6 ], [ 6, 7 ],
             [ 3, 8 ] ];
  else
    Error( "type En only defined for n = 6, 7 or 8; not ", n );
  fi;
end );

InstallMethod( DynkinGraph, [ IsChar, IsPosInt ],
function( type, n )
  if type = 'A' then
    return DynkinGraphAn( n );
  elif type = 'D' then
    return DynkinGraphDn( n );
  elif type = 'E' then
    return DynkinGraphEn( n );
  else
    Error( "type must be 'A', 'D' or 'E'; not ", type );
  fi;
end );

InstallMethod( EuclideanGraphAn, [ IsPosInt ],
function( n )
  return Concatenation( DynkinGraphAn( n + 1 ), [ [ n + 1, 1 ] ] );
end );

InstallMethod( EuclideanGraphDn, [ IsPosInt ],
function( n )
  return Concatenation( DynkinGraphDn( n ), [ [ n - 1, n + 1 ] ] );
end );

InstallMethod( EuclideanGraphEn, [ IsPosInt ],
function( n )
  if n = 6 then
    return [ [ 1, 2 ], [ 2, 3 ], [ 3, 4 ], [ 4, 5 ],
             [ 3, 6 ], [ 6, 7 ] ];
  elif n = 7 then
    return [ [ 1, 2 ], [ 2, 3 ], [ 3, 4 ], [ 4, 5 ], [ 5, 6 ], [ 6, 7 ],
             [ 4, 7 ] ];
  elif n = 8 then
    return [ [ 1, 2 ], [ 2, 3 ], [ 3, 4 ], [ 4, 5 ], [ 5, 6 ], [ 6, 7 ], [ 7, 8 ],
             [ 3, 9 ] ];
  else
    Error( "type En only defined for n = 6, 7 or 8; not ", n );
  fi;
end );

InstallMethod( EuclideanGraph, [ IsChar, IsPosInt ],
function( type, n )
  if type = 'A' then
    return EuclideanGraphAn( n );
  elif type = 'D' then
    return EuclideanGraphDn( n );
  elif type = 'E' then
    return EuclideanGraphEn( n );
  else
    Error( "type must be 'A', 'D' or 'E'; not ", type );
  fi;
end );

InstallMethod( ParseDynkinOrEuclideanDescriptionString, [ IsString ],
function( str )
  local err_invalid, type, i, euclidean, n_start, n_end, n, num_arrows, orientation;
  err_invalid := Concatenation( "the string \"", str, "\" is not ",
                                "a valid description of a Dynkin/Euclidean graph" );
  if Length( str ) < 2 or not ( str[ 1 ] in "ADE" ) then
    Error( err_invalid );
  fi;
  type := str[ 1 ];
  if str[ 2 ] = '~' then
    euclidean := true;
    n_start := 3;
  else
    euclidean := false;
    n_start := 2;
  fi;
  if not IsDigitChar( str[ n_start ] ) then
    Error( err_invalid );
  fi;
  i := n_start;
  while i <= Length( str ) and IsDigitChar( str[ i ] ) do
    i := i + 1;
  od;
  n_end := i - 1;
  n := Int( str{ [ n_start .. n_end ] } );
  if euclidean then
    if type = 'A' then
      num_arrows := n + 2;
    else
      num_arrows := n + 1;
    fi;
  else
    num_arrows := n - 1;
  fi;
  orientation := str{ [ i .. Length( str ) ] };
  if not ( Length( orientation ) in [ 0, num_arrows ]
           and ForAll( orientation, c -> c in "<>" ) ) then
    Error( err_invalid );
  fi;
  return [ type, euclidean, n, orientation ];
end );

InstallMethod( DynkinOrEuclideanGraph, [ IsChar, IsBool, IsPosInt ],
function( type, euclidean, n )
  return DynkinOrEuclideanGraph( type, euclidean, n, "" );
end );

InstallMethod( DynkinOrEuclideanGraph, [ IsChar, IsBool, IsPosInt, IsString ],
function( type, euclidean, n, orientation )
  local G, num_arrows, i;
  if euclidean then
    G := EuclideanGraph( type, n );
  else
    G := DynkinGraph( type, n );
  fi;
  num_arrows := Length( G );
  if Length( orientation ) > 0 then
    if Length( orientation ) <> num_arrows then
      Error( "length of orientation string \"", orientation, "\" (", Length( orientation ), ") ",
             "not the same as number of arrows (", num_arrows );
    fi;
    for i in [ 1 .. num_arrows ] do
      if orientation[ i ] = '<' then
        G[ i ] := [ G[ i ][ 2 ], G[ i ][ 1 ] ];
      elif orientation[ i ] <> '>' then
        Error( "orientation string for Dynkin/Euclidean graph must contain only '<' and '>', not ",
               orientation[ i ] );
      fi;
    od;
  fi;
  return G;
end );


DeclareDirectionOperations( DynkinOrEuclideanQuiver,
                            LeftDynkinOrEuclideanQuiver, RightDynkinOrEuclideanQuiver );
DeclareDirectionOperations( DynkinQuiver,
                            LeftDynkinQuiver, RightDynkinQuiver );
DeclareDirectionOperations( EuclideanQuiver,
                            LeftEuclideanQuiver, RightEuclideanQuiver );

InstallMethodWithDirections( DynkinOrEuclideanQuiver,
                             [ IsString ],
dir -> function( str )
  local tuple, type, euclidean, n, orientation;
  tuple := ParseDynkinOrEuclideanDescriptionString( str );
  type := tuple[ 1 ];
  euclidean := tuple[ 2 ];
  n := tuple[ 3 ];
  orientation := tuple[ 4 ];
  return DynkinOrEuclideanQuiver( dir, type, euclidean, n, orientation );
end );

InstallMethodWithDirections( DynkinOrEuclideanQuiver,
                             [ IsChar, IsBool, IsPosInt ],
dir -> function( type, euclidean, n )
  return DynkinOrEuclideanQuiver( dir, type, euclidean, n, "" );
end );

InstallMethodWithDirections( DynkinOrEuclideanQuiver,
                             [ IsChar, IsBool, IsPosInt, IsString ],
dir -> function( type, euclidean, n, orientation )
  local label, vertex_pattern, arrow_pattern;
  if euclidean then
    label := Concatenation( [ type ], "~", String( n ) );
  else
    label := Concatenation( [ type ], String( n ) );
  fi;
  vertex_pattern := "1";
  if n < 27 then
    # at most 26 arrows; label them 'a', 'b', 'c', ...
    arrow_pattern := "a";
  else
    # more than 26 arrows; label them "a1", "a2", "a3", ...
    arrow_pattern := "a1";
  fi;
  return DynkinOrEuclideanQuiver( dir, type, euclidean, n, orientation,
                                  [ label, vertex_pattern, arrow_pattern ] );
end );

InstallMethodWithDirections( DynkinOrEuclideanQuiver,
                             [ IsChar, IsBool, IsPosInt, IsString, IsList ],
dir -> function( type, euclidean, n, orientation, label_with_patterns_list )
  local num_vertices;
  if euclidean then
    num_vertices := n + 1;
  else
    num_vertices := n;
  fi;
  return Quiver( dir, label_with_patterns_list, num_vertices, [],
                 DynkinOrEuclideanGraph( type, euclidean, n, orientation ) );
end );

InstallMethodWithDirections( DynkinQuiver,
                             [ IsString ],
dir -> function( str )
  if ParseDynkinOrEuclideanDescriptionString( str )[ 2 ] then
    Error( "the string \"", str, "\" ",
           "describes a Euclidean quiver, not a Dynkin quiver" );
  fi;
  return DynkinOrEuclideanQuiver( dir, str );
end );

InstallMethodWithDirections( DynkinQuiver,
                             [ IsChar, IsPosInt ],
dir -> function( type, n )
  return DynkinOrEuclideanQuiver( dir, type, false, n );
end );

InstallMethodWithDirections( EuclideanQuiver,
                             [ IsString ],
dir -> function( str )
  if not ParseDynkinOrEuclideanDescriptionString( str )[ 2 ] then
    Error( "the string \"", str, "\" ",
           "describes a Dynkin quiver, not a Euclidean quiver" );
  fi;
  return DynkinOrEuclideanQuiver( dir, str );
end );

InstallMethodWithDirections( EuclideanQuiver,
                             [ IsChar, IsPosInt ],
dir -> function( type, n )
  return DynkinOrEuclideanQuiver( dir, type, true, n );
end );
