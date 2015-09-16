BindGlobal( "FamilyOfQuiverRepresentationHomomorphisms",
            NewFamily( "quiver representation homomorphisms" ) );

# We need this because the builtin global function Image uses
# the FamilySource attribute to check whether a given map can
# be applied to a given element.
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
                  MatrixVectorMultiplication( Q ) ) );
end );


BindGlobal( "FamilyOfQuiverModuleHomomorphisms",
            NewFamily( "quiver module homomorphisms" ) );

# We need this because the builtin global function Image uses
# the FamilySource attribute to check whether a given map can
# be applied to a given element.
SetFamilySource( FamilyOfQuiverModuleHomomorphisms,
                 FamilyOfQuiverModuleElements );

DeclareRepresentation( "IsQuiverModuleHomomorphismRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [] );

InstallMethod( AsModuleHomomorphism,
               [ IsQuiverRepresentationHomomorphism, IsQuiverAlgebra ],
function( rf, A )
  local R1, R2, rep_algebra, hom_cat, M1, M2, matrices, f;
  R1 := Source( rf );
  R2 := Range( rf );
  rep_algebra := AlgebraOfRepresentation( R1 );
  if A = AlgebraForLeftModules( rep_algebra ) then
    hom_cat := IsLeftQuiverModuleHomomorphism;
  elif A = AlgebraForRightModules( rep_algebra ) then
    hom_cat := IsRightQuiverModuleHomomorphism;
  else
    Error( "Representation homomorphism is not over the given algebra or its opposite" );
  fi;
  M1 := AsModule( R1, A );
  M2 := AsModule( R2, A );
  matrices := MatricesOfRepresentationHomomorphism( rf );
  f := rec();
  ObjectifyWithAttributes( f, NewType( FamilyOfQuiverModuleHomomorphisms,
                                       hom_cat and IsQuiverModuleHomomorphismRep ),
                           UnderlyingRepresentationHomomorphism, rf,
                           MatricesOfModuleHomomorphism, matrices,
                           Source, M1,
                           Range, M2 );
  return f;
end );

InstallMethod( AsLeftModuleHomomorphism,
               [ IsQuiverRepresentationHomomorphism ],
function( f )
  local A;
  A := AlgebraOfRepresentation( Source( f ) );
  return AsModuleHomomorphism( f, AlgebraForLeftModules( A ) );
end );

InstallMethod( AsRightModuleHomomorphism,
               [ IsQuiverRepresentationHomomorphism ],
function( f )
  local A;
  A := AlgebraOfRepresentation( Source( f ) );
  return AsModuleHomomorphism( f, AlgebraForRightModules( A ) );
end );

InstallMethod( LeftQuiverModuleHomomorphism,
               [ IsLeftQuiverModule, IsLeftQuiverModule, IsDenseList ],
function( M1, M2, matrices )
  local R1, R2, rf;
  R1 := UnderlyingRepresentation( M1 );
  R2 := UnderlyingRepresentation( M2 );
  rf := QuiverRepresentationHomomorphism( R1, R2, matrices );
  return AsLeftModuleHomomorphism( rf );
end );

InstallMethod( RightQuiverModuleHomomorphism,
               [ IsRightQuiverModule, IsRightQuiverModule, IsDenseList ],
function( M1, M2, matrices )
  local R1, R2, rf;
  R1 := UnderlyingRepresentation( M1 );
  R2 := UnderlyingRepresentation( M2 );
  rf := QuiverRepresentationHomomorphism( R1, R2, matrices );
  return AsRightModuleHomomorphism( rf );
end );

