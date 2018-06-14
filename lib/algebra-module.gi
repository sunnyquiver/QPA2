InstallMethod( QuiverRepresentationFromBasis,
               [ IsQuiverAlgebra, IsDenseList, IsDenseList, IsFunction ],
function( A, basis, basis_vertices, action )
  local Q, i, b, v, a, res, j;

  Q := QuiverOfAlgebra( A );
  if Length( basis ) <> Length( basis_vertices ) then
    Error( "list of vertices for basis does not have same length as list of basis elements" );
  fi;
  if not ForAll( basis_vertices, v -> IsVertex( v ) and v in Q ) then
    Error( "list of basis vertices contains at least one element which is not a vertex in the quiver" );
  fi;
  for i in [ 1 .. Length( basis ) ] do
    b := basis[ i ];
    v := basis_vertices[ i ];
    for a in OutgoingArrows( v ) do
      res := action( b, a );
      for j in [ 1 .. Length( basis ) ] do
        if basis_vertices[ j ] <> Target( a )
           and not IsZero( res[ j ] ) then
          Error( "acting on basis element ", b, " with arrow ", a, " ",
                 "gives result outside the target of the arrow" );
        fi;
      od;
    od;
  od;

  return QuiverRepresentationFromBasisNC( A, basis, basis_vertices, action );
end );


InstallMethod( QuiverRepresentationFromBasisNC,
               [ IsQuiverAlgebra, IsDenseList, IsDenseList, IsFunction ],
function( A, basis, basis_vertices, action )
  local Q, k, num_vertices, arrows, basis_by_vertex, i,
        perm_to_rep, perm_from_rep, matrices, a,
        src_basis_indices, tgt_basis_indices, matrix_rows;

  Q := QuiverOfAlgebra( A );
  k := LeftActingDomain( A );
  num_vertices := NumberOfVertices( Q );
  arrows := Arrows( Q );

  basis_by_vertex := List( [ 1 .. num_vertices ], i -> [] );
  for i in [ 1 .. Length( basis ) ] do
    Add( basis_by_vertex[ VertexNumber( basis_vertices[ i ] ) ], i );
  od;

  perm_from_rep := AsPermutation( Transformation( Flat( basis_by_vertex ) ) );
  perm_to_rep := Inverse( perm_from_rep );

  matrices := [];
  for a in arrows do
    src_basis_indices := basis_by_vertex[ VertexNumber( Source( a ) ) ];
    tgt_basis_indices := basis_by_vertex[ VertexNumber( Target( a ) ) ];
    if not IsEmpty( src_basis_indices ) and not IsEmpty( tgt_basis_indices ) then
      matrix_rows := List( basis{ src_basis_indices }, b -> action( b, a ){ tgt_basis_indices } );
      matrices[ ArrowNumber( a ) ] := matrix_rows;
    fi;
  od;

  return [ QuiverRepresentation( A, List( basis_by_vertex, Length ), matrices ),
           perm_to_rep, perm_from_rep ];
end );

InstallMethod( AlgebraAsRepresentation, [ IsQuiverAlgebra ],
function( A )
  local basis, basis_vertices, action, rep_and_perms;

  if not IsFiniteDimensional( A ) then
    Error( "can not create representation for infinite-dimensional algebra ", A );
  fi;

  basis := CanonicalBasis( A );
  basis_vertices := List( basis, b -> Target( Paths( b )[ 1 ] ) );
  action := function( b, a )
    # b: basis element
    # a: arrow
    return Coefficients( basis, PathAction( b, a ) );
  end;
  rep_and_perms := QuiverRepresentationFromBasis( A, basis, basis_vertices, action );
  SetBasisPermutationToRepresentation( A, rep_and_perms[ 2 ] );
  SetBasisPermutationFromRepresentation( A, rep_and_perms[ 3 ] );
  return rep_and_perms[ 1 ];
end );

InstallMethod( AlgebraAsRepresentationEnv, [ IsQuiverAlgebra ],
function( A )
  local A_env, basis, basis_vertices, b, p, v1, v2, action, rep_and_perms;

  if not IsFiniteDimensional( A ) then
    Error( "can not create representation for infinite-dimensional algebra ", A );
  fi;

  A_env := EnvelopingAlgebra( A );
  basis := CanonicalBasis( A );
  basis_vertices := [];
  for b in basis do
    p := Paths( b )[ 1 ];
    Add( basis_vertices, [ LeftEnd( p ), RightEnd( p ) ] ^ LEFT_RIGHT );
  od;

  action := function( b, a )
    # b: basis element
    # a: arrow (in enveloping algebra)
    local paths;
    paths := a ^ LEFT_RIGHT;
    return Coefficients( basis, paths[ 1 ] * b * paths[ 2 ] );
  end;
  rep_and_perms := QuiverRepresentationFromBasis( A_env, basis, basis_vertices, action );
  SetBasisPermutationToRepresentationEnv( A, rep_and_perms[ 2 ] );
  SetBasisPermutationFromRepresentationEnv( A, rep_and_perms[ 3 ] );
  return rep_and_perms[ 1 ];
end );

InstallMethod( AsRepresentationElement, [ IsQuiverAlgebraElement ],
function( a )
  local A, R, B_alg, B_rep, perm;
  A := AlgebraOfElement( a );
  R := AlgebraAsRepresentation( A );
  B_alg := CanonicalBasis( A );
  B_rep := CanonicalBasis( R );
  perm := BasisPermutationToRepresentation( A );
  return LinearCombination( B_rep, Permuted( Coefficients( B_alg, a ), perm ) );
end );

InstallMethod( AsAlgebraElement, [ IsQuiverRepresentationElement ],
function( r )
  local A, R, B_alg, B_rep, perm;
  R := RepresentationOfElement( r );
  A := AlgebraOfRepresentation( R );
  if R <> AlgebraAsRepresentation( A ) then
    Error( "element is not from the representation for the algebra" );
  fi;
  B_alg := CanonicalBasis( A );
  B_rep := CanonicalBasis( R );
  perm := BasisPermutationFromRepresentation( A );
  return LinearCombination( B_alg, Permuted( Coefficients( B_rep, r ), perm ) );
end );

InstallMethod( AsRepresentationElementEnv, [ IsQuiverAlgebraElement ],
function( a )
  local A, R, B_alg, B_rep, perm;
  A := AlgebraOfElement( a );
  R := AlgebraAsRepresentationEnv( A );
  B_alg := CanonicalBasis( A );
  B_rep := CanonicalBasis( R );
  perm := BasisPermutationToRepresentationEnv( A );
  return LinearCombination( B_rep, Permuted( Coefficients( B_alg, a ), perm ) );
end );

InstallMethod( AsAlgebraElementEnv, [ IsQuiverRepresentationElement ],
function( r )
  local A, R, alg_list, B_alg, B_rep, perm;
  R := RepresentationOfElement( r );
  alg_list := AlgebraOfRepresentation( R ) ^ LEFT_RIGHT;
  A := alg_list[ 1 ];
  if R <> AlgebraAsRepresentationEnv( A ) then
    Error( "element is not from the representation for the algebra" );
  fi;
  B_alg := CanonicalBasis( A );
  B_rep := CanonicalBasis( R );
  perm := BasisPermutationFromRepresentationEnv( A );
  return LinearCombination( B_alg, Permuted( Coefficients( B_rep, r ), perm ) );
end );


DeclareSideOperations( AlgebraAsModule, AlgebraAsLeftModule, AlgebraAsRightModule,
                       AlgebraAsBimodule );

InstallMethodWithSides( AlgebraAsModule,
                        [ IsQuiverAlgebra ],
side -> function( A )
  local R;
  if side = LEFT_RIGHT then
    R := AlgebraAsRepresentationEnv( A );
  else
    R := AlgebraAsRepresentation( A^side );
  fi;
  return AsModule( side, R );
end );

DeclareSideOperations( AsModuleElement, AsLeftModuleElement, AsRightModuleElement,
                       AsBimoduleElement );

InstallMethodWithSides( AsModuleElement,
                        [ IsQuiverAlgebraElement ],
side -> function( a )
  local r;
  if side = LEFT_RIGHT then
    r := AsRepresentationElementEnv( a );
  else
    r := AsRepresentationElement( a^side );
  fi;
  return AsModuleElement( side, r );
end );

InstallMethod( AsAlgebraElement, [ IsQuiverModuleElement ],
function( m )
  local r, R;
  r := UnderlyingRepresentationElement( m );
  R := RepresentationOfElement( r );
  if Side( m ) = LEFT_RIGHT then
    return AsAlgebraElementEnv( r );
  elif Side( m ) = Direction( QuiverOfRepresentation( R ) ) then
    return AsAlgebraElement( r );
  else
    return OppositeAlgebraElement( AsAlgebraElement( r ) );
  fi;
end );
