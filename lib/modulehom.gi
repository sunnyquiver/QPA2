BindGlobal( "FamilyOfQuiverRepresentationHomomorphisms",
            NewFamily( "quiver representation homomorphisms" ) );

# The builtin global function Image uses the FamilySource attribute to check
# whether a given map can be applied to a given element.
SetFamilySource( FamilyOfQuiverRepresentationHomomorphisms,
                 FamilyOfQuiverRepresentationElements );

DeclareRepresentation( "IsQuiverRepresentationHomomorphismRep",
                       IsComponentObjectRep,
                       [ "source", "range", "matrices" ] );

InstallMethod( QuiverRepresentationHomomorphism,
               "for quiver representations and dense list",
               [ IsQuiverRepresentation, IsQuiverRepresentation,
                 IsDenseList ],
function( source, range, matrices )
  local A, Q, i, dim_src, dim_tgt, expected_dim, matrix_dim,
        arrow, comp1, comp2;
  A := AlgebraOfRepresentation( source );
  if A <> AlgebraOfRepresentation( range ) then
    Error( "Source and range are representations of different algebras" );
  fi;
  Q := QuiverOfAlgebra( A );
  for i in [ 1 .. NumberOfVertices( Q ) ] do
    dim_src := VertexDimension( source, i );
    dim_tgt := VertexDimension( range, i );
    if IsLeftQuiver( Q ) then
      expected_dim := [ dim_tgt, dim_src ];
    else
      expected_dim := [ dim_src, dim_tgt ];
    fi;
    matrix_dim := DimensionsMat( matrices[ i ] );
    if matrix_dim <> expected_dim then
      Error( "Wrong dimensions of matrix for vertex ", i,
             " (dimensions are ", matrix_dim, ", should be ", expected_dim, ")" );
    fi;
  od;
  for arrow in Arrows( Q ) do
    if IsLeftQuiver( Q ) then
      comp1 := matrices[ VertexNumber( Target( arrow ) ) ] *
               MatrixForArrow( source, arrow );
      comp2 := MatrixForArrow( range, arrow ) *
               matrices[ VertexNumber( Source( arrow ) ) ];
    else
      comp1 := MatrixForArrow( source, arrow ) *
               matrices[ VertexNumber( Target( arrow ) ) ];
      comp2 := matrices[ VertexNumber( Source( arrow ) ) ] *
               MatrixForArrow( range, arrow );
    fi;
    if comp1 <> comp2 then
      Error( "Does not commute with maps for arrow ", arrow );
    fi;
  od;
  return QuiverRepresentationHomomorphismNC( source, range, matrices );
end );

InstallMethod( QuiverRepresentationHomomorphismNC,
               "for quiver representations and dense list",
               [ IsQuiverRepresentation, IsQuiverRepresentation,
                 IsDenseList ],
function( source, range, matrices )
  return Objectify( NewType( FamilyOfQuiverRepresentationHomomorphisms,
                             IsQuiverRepresentationHomomorphism and
                             IsQuiverRepresentationHomomorphismRep),
                    rec( source := source,
                         range := range,
                         matrices := matrices ) );
end );

InstallMethod( String,
               "for quiver representation homomorphism",
               [ IsQuiverRepresentationHomomorphism ],
function( f )
  return Concatenation( "(", String( Source( f ) ), ")",
                        "->",
                        "(", String( Range( f ) ), ")" );
end );

InstallMethod( ViewObj,
               "for quiver representation homomorphism",
               [ IsQuiverRepresentationHomomorphism ],
function( f )
  Print( "<", String( f ), ">" );
end );

InstallMethod( Source,
               "for quiver representation homomorphism",
               [ IsQuiverRepresentationHomomorphism and
                 IsQuiverRepresentationHomomorphismRep ],
function( f )
  return f!.source;
end );

InstallMethod( Range,
               "for quiver representation homomorphism",
               [ IsQuiverRepresentationHomomorphism and
                 IsQuiverRepresentationHomomorphismRep ],
function( f )
  return f!.range;
end );

InstallMethod( MatricesOfRepresentationHomomorphism,
               "for quiver representation homomorphism",
               [ IsQuiverRepresentationHomomorphism and
                 IsQuiverRepresentationHomomorphismRep ],
function( f )
  return f!.matrices;
end );

InstallMethod( ImageElm,
               [ IsQuiverRepresentationHomomorphism,
                 IsQuiverRepresentationElement ],
function( f, e )
  local Q;
  Q := QuiverOfRepresentation( Source( f ) );
  return QuiverRepresentationElement
         ( Range( f ),
           ListN( MatricesOfRepresentationHomomorphism( f ),
                  ElementVectors( e ),
                  VertexDimensions( Range( f ) ),
                  MatrixVectorMultiplication( Q ) ) );
end );
