DeclareDirectionOperations( DynkinQuiver, LeftDynkinQuiver, RightDynkinQuiver );

InstallMethodWithDirections( DynkinQuiver, [ IsString ],
dir -> function( str )
  local tuple, type, n, orientation;
  tuple := ParseDynkinQuiverString( str );
  type := tuple[ 1 ];
  n := tuple[ 2 ];
  orientation := tuple[ 3 ];
  return DynkinQuiver( dir, type, n, orientation );
end );

InstallMethodWithDirections( DynkinQuiver, [ IsString, IsPosInt ],
dir -> function( type, n )
  return DynkinQuiver( dir, type, n, "" );
end );

InstallMethodWithDirections( DynkinQuiver, [ IsString, IsPosInt, IsString ],
dir -> function( type, n, orientation )
  local G, extended, num_vertices, num_arrows, label, vertex_pattern, arrow_pattern;

  G := DynkinGraph( type, n, orientation );
  extended := ( Length( type ) = 2 );
  if extended then
    num_vertices := n + 1;
  else
    num_vertices := n;
  fi;
  num_arrows := Length( G );

  label := Concatenation( type, String( n ) );
  vertex_pattern := "1";
  if num_arrows < 27 then
    # at most 26 arrows; label them 'a', 'b', 'c', ...
    arrow_pattern := "a";
  else
    # more than 26 arrows; label them "a1", "a2", "a3", ...
    arrow_pattern := "a1";
  fi;

  return Quiver( dir, [ label, vertex_pattern, arrow_pattern ],
                 num_vertices, [], G );
end );

InstallMethod( DynkinGraph, [ IsString, IsPosInt ],
function( type, n )
  if type = "A" then
    return List( [ 1 .. ( n - 1 ) ], i -> [ i, i + 1 ] );
  elif type = "D" then
    if n < 4 then
      Error( "Dynkin type D only defined for n >= 4, not ", n );
    fi;
    return Concatenation( [ [ 1, 3 ],
                            [ 2, 3 ] ],
                          List( [ 3 .. ( n - 1 ) ], i -> [ i, i + 1 ] ) );
  elif type = "E" then
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
      Error( "Dynkin type E only defined for n = 6, 7 or 8; not ", n );
    fi;
  elif type = "A~" then
    return Concatenation( DynkinGraph( "A", n + 1 ), [ [ n + 1, 1 ] ] );
  elif type = "D~" then
    return Concatenation( DynkinGraph( "D", n ), [ [ n - 1, n + 1 ] ] );
  elif type = "E~" then
    if n = 6 then
      return [ [ 1, 2 ], [ 2, 3 ], [ 3, 4 ], [ 4, 5 ],
               [ 3, 6 ], [ 6, 7 ] ];
    elif n = 7 then
      return [ [ 1, 2 ], [ 2, 3 ], [ 3, 4 ], [ 4, 5 ], [ 5, 6 ], [ 6, 7 ],
               [ 4, 8 ] ];
    elif n = 8 then
      return [ [ 1, 2 ], [ 2, 3 ], [ 3, 4 ], [ 4, 5 ], [ 5, 6 ], [ 6, 7 ], [ 7, 8 ],
               [ 3, 9 ] ];
    else
      Error( "Dynkin type E only defined for n = 6, 7 or 8; not ", n );
    fi;
  else
    Error( "Dynkin type must be \"A\", \"D\", \"E\", ",
           "\"A~\", \"D~\" or \"E~\"; not ", type );
  fi;
end );

InstallMethod( DynkinGraph, [ IsString, IsPosInt, IsString ],
function( type, n, orientation )
  local G, num_arrows, i;
  G := DynkinGraph( type, n );
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
        Error( "orientation string for Dynkin graph must contain only '<' and '>', not ",
               orientation[ i ] );
      fi;
    od;
  fi;
  return G;
end );

InstallMethod( ParseDynkinQuiverString, [ IsString ],
function( str )
  local err_invalid, type, extended, n_start, n_end, i, n, num_arrows, orientation;
  err_invalid := Concatenation( "the string \"", str, "\" is not ",
                                "a valid description of a Dynkin quiver" );
  if Length( str ) < 2 or not ( str[ 1 ] in "ADE" ) then
    Error( err_invalid );
  fi;
  if str[ 2 ] = '~' then
    type := str{ [ 1, 2 ] };
    extended := true;
    n_start := 3;
  else
    type := str{ [ 1 ] };
    extended := false;
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
  if extended then
    if type = "A~" then
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
  return [ type, n, orientation ];
end );
