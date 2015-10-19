DeclareRepresentation( "IsEmptyMatrixRep",
                       IsComponentObjectRep and IsAttributeStoringRep, [] );

InstallMethod( MakeEmptyMatrix, "for integers and ring",
               [ IsInt, IsInt, IsRing ],
function( m, n, R )
  local prototype, M;
  if m < 0 then
    Error( "Negative matrix height ", m, " not allowed" );
  elif n < 0 then
    Error( "Negative matrix width ", n, " not allowed" );
  elif m > 0 and n > 0 then
    Error( "Empty matrix can not have non-empty size ", m, "x", n );
  fi;
  prototype := [ [ Zero( R ) ] ];
  M := rec();
  ObjectifyWithAttributes( M, NewType( FamilyObj( prototype ),
                                       IsEmptyMatrix and IsEmptyMatrixRep ),
                           DimensionsMat, [ m, n ],
                           RingOfEmptyMatrix, R );
  return M;
end );

InstallMethod( String, "for empty matrix",
               [ IsEmptyMatrix ],
function( M )
  local dim;
  dim := DimensionsMat( M );
  return Concatenation( "empty ", String( dim[ 1 ] ), "x", String( dim[ 2 ] ),
                        " matrix over ", String( RingOfEmptyMatrix( M ) ) );
end );

InstallMethod( PrintObj, "for empty matrix",
               [ IsEmptyMatrix ],
function( M )
  Print( "<", String( M ), ">" );
end );

InstallMethod( ViewObj, "for empty matrix",
               [ IsEmptyMatrix ],
function( M )
  Print( "<", String( M ), ">" );
end );

InstallMethod( MakeZeroMatrix, "for integers and ring",
               [ IsInt, IsInt, IsRing ],
function( m, n, R )
  if m = 0 or n = 0 then
    return MakeEmptyMatrix( m, n, R );
  else
    return MakeImmutable( NullMat( m, n, R ) );
  fi;
end );

InstallTrueMethod( IsZero, IsEmptyMatrix );

InstallMethod( IdentityMatrix, "for integer and ring",
               [ IsInt, IsRing ],
function( n, R )
  if n = 0 then
    return MakeEmptyMatrix( n, n, R );
  else
    return MakeImmutable( IdentityMat( n, R ) );
  fi;
end );

InstallMethod( \*, "for empty matrix and matrix",
               [ IsEmptyMatrix, IsMatrix ],
function( M, N )
  local dim_M, dim_N;
  dim_M := DimensionsMat( M );
  dim_N := DimensionsMat( N );
  if dim_M[ 2 ] <> dim_N[ 1 ] then
    Error( "Dimensions of matrices do not match: ", dim_M, " and ", dim_N );
  fi;
  return MakeZeroMatrix( dim_M[ 1 ], dim_N[ 2 ], RingOfEmptyMatrix( M ) );
end );

InstallMethod( \*, "for matrix and empty matrix",
               [ IsMatrix, IsEmptyMatrix ],
function( M, N )
  local dim_M, dim_N;
  dim_M := DimensionsMat( M );
  dim_N := DimensionsMat( N );
  if dim_M[ 2 ] <> dim_N[ 1 ] then
    Error( "Dimensions of matrices do not match: ", dim_M, " and ", dim_N );
  fi;
  return MakeZeroMatrix( dim_M[ 1 ], dim_N[ 2 ], RingOfEmptyMatrix( N ) );
end );

InstallMethod( \+, "for empty matrices",
               [ IsEmptyMatrix, IsEmptyMatrix ],
function( M, N )
  local dim_M, dim_N;
  dim_M := DimensionsMat( M );
  dim_N := DimensionsMat( N );
  if dim_M <> dim_N then
    Error( "Dimensions of matrices do not match: ", dim_M, " and ", dim_N );
  fi;
  return M;
end );

InstallMethod( \*, "for empty matrix and vector",
               [ IsEmptyMatrix, IsVector ],
function( M, v )
  local dim;
  dim := DimensionsMat( M );
  if dim[ 2 ] <> Length( v ) then
    Error( "Can not multiply ", dim[ 1 ], "x", dim[ 2 ],
           " matrix by vector of length ", Length( v ) );
  fi;
  return [];
end );

InstallMethod( \*, "for empty matrix and empty list",
               [ IsEmptyMatrix, IsList and IsEmpty ],
function( M, v )
  local dim;
  dim := DimensionsMat( M );
  if dim[ 2 ] <> 0 then
    Error( "Can not multiply ", dim[ 1 ], "x", dim[ 2 ],
           " matrix by empty vector" );
  fi;
  return List( [ 1 .. dim[ 1 ] ],
               x -> Zero( RingOfEmptyMatrix( M ) ) );
end );

InstallMethod( \*, "for vector and empty matrix",
               [ IsVector, IsEmptyMatrix ],
function( v, M )
  local dim;
  dim := DimensionsMat( M );
  if dim[ 1 ] <> Length( v ) then
    Error( "Can not multiply vector of length ", Length( v ),
           " by ", dim[ 1 ], "x", dim[ 2 ], " matrix" );
  fi;
  return [];
end );

InstallMethod( \*, "for empty list and empty matrix",
               [ IsList and IsEmpty, IsEmptyMatrix ],
function( v, M )
  local dim;
  dim := DimensionsMat( M );
  if dim[ 1 ] <> 0 then
    Error( "Can not multiply empty vector by ",
           dim[ 1 ], "x", dim[ 2 ], " matrix" );
  fi;
  return List( [ 1 .. dim[ 2 ] ],
               x -> Zero( RingOfEmptyMatrix( M ) ) );
end );

InstallMethod( \=, "for empty matrices",
               [ IsEmptyMatrix, IsEmptyMatrix ],
function( M, N )
  return DimensionsMat( M ) = DimensionsMat( N )
         and RingOfEmptyMatrix( M ) = RingOfEmptyMatrix( N );
end );

InstallMethod( \=, "for matrix and empty matrix",
               [ IsMatrix, IsEmptyMatrix ],
               ReturnFalse );

InstallMethod( \=, "for empty matrix and matrix",
               [ IsEmptyMatrix, IsMatrix ],
               ReturnFalse );

InstallMethod( TransposedMat, "for empty matrix",
               [ IsEmptyMatrix ],
function( M )
  local dim;
  dim := DimensionsMat( M );
  return MakeEmptyMatrix( dim[ 2 ], dim[ 1 ], RingOfEmptyMatrix( M ) );
end );

InstallMethod( TriangulizedNullspaceMat, "for empty matrix",
               [ IsEmptyMatrix ],
function( M )
  local dim;
  dim := DimensionsMat( M );
  if dim[ 1 ] = 0 then
    return [];
  else
    return IdentityMatrix( dim[ 1 ], RingOfEmptyMatrix( M ) );
  fi;
end );
