InstallMethod( IsAdmissibleSequence, [ IsDenseList ],
function( seq )
  local i;
  if not ForAll( seq, IsPosInt ) then
    Error( "sequence to be tested for admissibility should be a ",
           "list of positive integers" );
  fi;
  for i in [ 1 .. ( Length( seq ) - 1 ) ] do
    if seq[ i + 1 ] < seq[ i ] - 1 or seq[ i ] = 1 then
      return false;
    fi;
  od;
  if seq[ 1 ] < seq[ Length( seq ) ] - 1 then
    return false;
  fi;
  return true;
end );

DeclareDirectionOperations( NakayamaAlgebra, LeftNakayamaAlgebra, RightNakayamaAlgebra );

InstallMethodWithDirections( NakayamaAlgebra,
                             [ IsField, IsDenseList ],
dir -> function( K, seq )
  local n, Q, arrows, kQ, rels, i, rel_path, A, index;

  n := Length( seq );

  if not IsAdmissibleSequence( seq ) then
    Error( "not an admissible sequence: ", seq );
  fi;

  if seq[ n ] = 1 then
    # A_n case
    Q := DynkinQuiver( dir, "A", n );
    arrows := Arrows( Q );
    kQ := PathAlgebra( K, Q );
    rels := [];
    for i in [ 1 .. ( n - 1 ) ] do
      if seq[ i ] - 1 <> seq[ i + 1 ] then
        rel_path := PathFromArrowList( arrows{ [ i .. ( i + seq[ i ] - 1 ) ] } );
        Add( rels, PathAsAlgebraElement( kQ, rel_path ) );
      fi;
    od;
    A := kQ / rels;
  else
    # A~_n case
    Q := DynkinQuiver( dir, "A~", n - 1 );
    arrows := Arrows( Q );
    kQ := PathAlgebra( K, Q );
    rels := [];
    index := i -> ( i - 1 ) mod n + 1; # for cyclic lookup of vertices/arrows
    for i in [ 1 .. n ] do
      if seq[ i ] - 1 <> seq[ index( i + 1 ) ] then
        rel_path := PathFromArrowList( arrows{ List( [ i .. ( i + seq[ i ] - 1 ) ], index ) } );
        Add( rels, PathAsAlgebraElement( kQ, rel_path ) );
      fi;
    od;
    A := kQ / rels;
  fi;

  SetIsNakayamaAlgebra( A, true );
  SetAdmissibleSequence( A, seq );
  return A;
  
end );

InstallMethod( IsNakayamaAlgebra, [ IsQuiverAlgebra ],
function( A )
  local Q;
  Q := QuiverOfAlgebra( A );
  # TODO: need to check that A = kQ/I where I is an admissible ideal
  return Length( SourceVertices( Q ) ) <= 1 and
         Length( SinkVertices( Q ) ) <= 1 and
         ForAll( Vertices( Q ), v -> Indegree( v ) <= 1 and Outdegree( v ) <= 1 );
end );
