#######################################################################
##
#P  IsSimpleRepresentation( <R> )
##
##  This function checks if a representation <R> is a simple 
##  representation over an admissible quotient of a path algebra. 
##
InstallMethod ( IsSimpleRepresentation, 
"for a QuiverRepresentation",
[ IsQuiverRepresentation ], 
function( R )

  local A;

  A := AlgebraOfRepresentation( R );
  if not IsAdmissibleQuiverAlgebra( A ) then 
    Error( "The representation is not over an admissible quiver algebra.\n" );
  fi;
  
  return Dimension( R ) = 1;
end
  );

#######################################################################
##
#A  SimpleRepresentations( <A> )
##
##  This function constructs all the simple representations over an
##  admissible quotient of a path algebra. It returns all the simple
##  representations as a list of representations corresponding to the 
##  numbering of the vertices. 
##
InstallMethod ( SimpleRepresentations, 
    "for an admissible quotient of a path algebra",
    [ IsQuiverAlgebra ], 
    function( A )

    local KQ, num_vert, simple_rep, zero, v, temp, s;
#
    if not IsFiniteDimensional( A ) then
        Error( "argument entered is not a finite dimensional algebra,\n" );
    fi;
    if ( not IsPathAlgebra( A ) ) and ( not IsAdmissibleQuiverAlgebra(A) ) then
        Error( "argument entered is not a quotient of a path algebra by an admissible ideal,\n" );
    fi;
    num_vert := NumberOfVertices( QuiverOfAlgebra( A ) ); 
    simple_rep := [];
    zero := List( [ 1..num_vert ], x -> 0 );
    for v in [ 1..num_vert ] do
    	temp := ShallowCopy( zero );
        temp[ v ] := 1; 
        Add( simple_rep, QuiverRepresentation( A, temp, [] ) );
    od;
    for s in simple_rep do
        SetIsSimpleRepresentation( s, true );
    od;
    return simple_rep;
end
);

InstallMethod( BasisOfProjectives, "for quiver algebra",
               [ IsQuiverAlgebra ],
function( A )
  local   basis,  list,  b,  i,  j;
  basis := BasisVectors( Basis( A ) );
  list := List( Vertices( QuiverOfAlgebra( A ) ),
                v -> List( Vertices( QuiverOfAlgebra( A ) ), w -> [] ) );
  for b in basis do
    i := VertexNumber( Source( Paths( b )[ 1 ] ) );
    j := VertexNumber( Target( Paths( b )[ 1 ] ) );
    Add( list[ i ][ j ], b );
  od;
  return list;
end );

InstallMethod( IndecProjRepresentations, "for quiver algebra",
               [ IsQuiverAlgebra ],
function( A )
  local basis_list, proj_modules, i, basis, dimensions, arrows, 
        arrows_with_matrices, matrices, a, source, target, dim_source, 
        dim_target, b_source, b_target, matrix, b, b_a_path, b_a, 
        coeffs, R;

  if not IsFiniteDimensional( A ) then
    Error( "infinite-dimensional algebra" );
  fi;
  if not IsAdmissibleQuiverAlgebra( A ) then
    TryNextMethod();
  fi;

  basis_list := BasisOfProjectives( A );
  proj_modules := [];
  for i in [ 1 .. NumberOfVertices( QuiverOfAlgebra( A ) ) ] do
    basis := basis_list[ i ];
    dimensions := List( basis, Length );
    arrows := Arrows( QuiverOfAlgebra( A ) );
    arrows_with_matrices := [];
    matrices := [];
    for a in arrows do
      source := VertexNumber( Source( a ) );
      target := VertexNumber( Target( a ) );
      dim_source := dimensions[ source ];
      dim_target := dimensions[ target ];
      if dim_source <> 0 and dim_target <> 0 then
        b_source := List( basis[ source ], b -> Paths(b)[ 1 ] );
        b_target := List( basis[ target ], b -> Paths(b)[ 1 ] );
        matrix := [];
        for b in b_source do
          b_a_path := ComposePaths( b, a );
          b_a := PathAsAlgebraElement( A, b_a_path );
          coeffs := CoefficientsOfPaths( b_target, b_a );
          Add( matrix, coeffs );
        od;
        Add( arrows_with_matrices, a );
        Add( matrices, matrix );
      fi;
    od;
    R := QuiverRepresentation( A, dimensions, arrows_with_matrices, matrices );
    Add( proj_modules, R );
  od;
  return proj_modules;
end );

InstallMethod( IndecInjRepresentations, "for quiver algebra",
               [ IsQuiverAlgebra ],
function( A )

  local Aop, proj_op, cat, dual;
  
  Aop := OppositeAlgebra( A );
  proj_op := IndecProjRepresentations( Aop );
  cat := CategoryOfQuiverRepresentations( Aop );
  dual := DualFunctor( cat );

  return List( proj_op, p -> ApplyFunctor( dual, p ) );
end
  );

#######################################################################
##
#P  IsSemisimpleRepresentation( <R> )
##
##  Checks whether <R> is semisimple.
##
InstallMethod( IsSemisimpleRepresentation, 
"for a representation over a quotient of a path algebra",
[ IsQuiverRepresentation ],
function( R ); 

   if Dimension ( RadicalOfRepresentation( R ) ) = 0 then 
      return true;
   else
      return false;
   fi;
end
  ); 
