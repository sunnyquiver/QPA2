InstallMethod( \=, [ IsHomSpace, IsHomSpace ], ReturnFalse );

InstallMethod( Zero, [ IsHomSpace ],
function( hom )
  return ZeroMorphism( Source( hom ), Range( hom ) );
end );

BindGlobal( "FamilyOfVectorSpaceHomSpaces",
            NewFamily( "vector space hom spaces" ) );
DeclareRepresentation( "IsVectorSpaceHomSpaceRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [] );

InstallMethod( Hom,
               [ IsQPAVectorSpace, IsQPAVectorSpace ],
function( V1, V2 )
  local F, cat, type, hom;
  if not IsIdenticalObj( CapCategory( V1 ), CapCategory( V2 ) ) then
    Error( "vector spaces from differenct categories" );
  fi;
  F := UnderlyingField( V1 );
  cat := CategoryOfVectorSpaces( F );
  type := NewType( FamilyOfQPAVectorSpaces,
                   IsVectorSpaceHomSpace and IsVectorSpaceHomSpaceRep );
  hom := rec();
  ObjectifyWithAttributes( hom, type,
                           Source, V1,
                           Range, V2,
                           UnderlyingField, F );
  Add( cat, hom );
  return Intern( hom );
end );

InstallMethod( CanonicalBasis, [ IsVectorSpaceHomSpace ],
function( hom )
  local V1, V2, dim, basis_vectors, F, lists, basis;
  V1 := Source( hom );
  V2 := Range( hom );
  dim := Dimension( hom );
  if dim = 0 then
    basis_vectors := [];
  else
    F := UnderlyingField( hom );
    lists := IdentityMat( dim, F );
    basis_vectors := List( lists,
                           l -> LinearTransformationByRightMatrix( V1, V2, l ) );
  fi;
  basis := rec();
  ObjectifyWithAttributes( basis,
                           NewType( FamilyOfQPAVectorSpaces,
                                    IsBasis and IsVectorSpaceBasisRep ),
                           BasisVectors, basis_vectors,
                           UnderlyingLeftModule, hom );
  return basis;
end );
  
InstallMethod( Dimension, [ IsVectorSpaceHomSpace ],
function( hom )
  return Dimension( Source( hom ) ) * Dimension( Range( hom ) );
end );

InstallMethod( String, [ IsVectorSpaceHomSpace ],
function( hom )
  return Concatenation( "Hom(", String( Source( hom ) ),
                        ", ", String( Range( hom ) ), ")" );
end );

InstallMethod( PrintObj, [ IsVectorSpaceHomSpace ],
function( hom )
  Print( String( hom ) );
end );

InstallMethod( ViewObj, [ IsVectorSpaceHomSpace ],
function( hom )
  Print( String( hom ) );
end );

InstallMethod( \=, [ IsVectorSpaceHomSpace, IsVectorSpaceHomSpace ],
function( hom1, hom2 )
  return Source( hom1 ) = Source( hom2 )
         and Range( hom1 ) = Range( hom2 );
end );

InstallMethod( Hom, [ IsLinearTransformation, IsQPAVectorSpace ],
function( T, V )
  local W1, W2;
  W1 := Source( T );
  W2 := Range( T );
  #      T
  # W1 ----> W2
  #   \     /
  #    .   .
  #      V
  #
  # T* : Hom( W2, V ) -> Hom( W1, V )
  #            f     |-> PreCompose( T, f )
  return LinearTransformationByFunction( Hom( W2, V ),
                                         Hom( W1, V ),
                                         f -> PreCompose( T, f ) );
end );

InstallMethod( Hom, [ IsQPAVectorSpace, IsLinearTransformation ],
function( V, T )
  local W1, W2;
  W1 := Source( T );
  W2 := Range( T );
  #      V
  #    /   \
  #   .     .
  # W1 ----> W2
  #      T
  #
  # T_* : Hom( V, W1 ) -> Hom( V, W2 )
  #             f     |-> PreCompose( f, T )
  return LinearTransformationByFunction( Hom( V, W1 ),
                                         Hom( V, W2 ),
                                         f -> PreCompose( f, T ) );
end );

BindGlobal( "FamilyOfQuiverRepresentationHomSpaces",
            NewFamily( "quiver representation hom spaces" ) );
DeclareRepresentation( "IsQuiverRepresentationHomSpaceRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [ ] );

InstallMethod( Hom,
               [ IsQuiverRepresentation, IsQuiverRepresentation ],
function( R1, R2 )
  local F, cat, type, hom;
  if not IsIdenticalObj( CapCategory( R1 ), CapCategory( R2 ) ) then
    Error( "representations from different categories" );
  fi;
  F := FieldOfRepresentation( R1 );
  cat := CategoryOfVectorSpaces( F );
  type := NewType( FamilyOfQPAVectorSpaces,
                   IsQuiverRepresentationHomSpace and IsQuiverRepresentationHomSpaceRep );
  hom := rec();
  ObjectifyWithAttributes( hom, type,
                           Source, R1,
                           Range, R2,
                           UnderlyingField, F );
  Add( cat, hom );
  return Intern( hom );
end );

InstallMethod( CanonicalBasis, [ IsQuiverRepresentationHomSpace ],
function( hom )
  local ker, ker_basis, vertex_spaces, vertex_projection_maps, 
        ker_to_morphism, basis_morphisms, basis;

  ker := KernelEmbedding( HomSpaceVertexToArrowMap( hom ) );
  ker_basis := Basis( Source( ker ) );

  vertex_spaces := VertexHomSpaces( hom );
  vertex_projection_maps := List( [ 1 .. Length( vertex_spaces ) ],
                                  i -> ProjectionInFactorOfDirectSum( vertex_spaces, i ) );


  ker_to_morphism := function( v )
    local vertex_sum_elem, vertex_projections;
    vertex_sum_elem := ImageElm( ker, v );
    vertex_projections := List( vertex_projection_maps,
                                m -> ImageElm( m, vertex_sum_elem ) );
    return QuiverRepresentationHomomorphismByMorphisms
           ( Source( hom ), Range( hom ), vertex_projections );
  end;

  basis_morphisms := List( ker_basis, ker_to_morphism );

  basis := rec();
  ObjectifyWithAttributes( basis,
                           NewType( FamilyOfVectorSpaceBases,
                                    IsBasis and IsVectorSpaceBasisRep ),
                           BasisVectors, basis_morphisms,
                           UnderlyingLeftModule, hom );
  return basis;
end );

InstallMethod( Dimension, [ IsQuiverRepresentationHomSpace ],
               hom -> Length( CanonicalBasis( hom ) ) );

InstallMethod( String, [ IsQuiverRepresentationHomSpace ],
function( hom )
  return Concatenation( "Hom(", String( Source( hom ) ),
                        ", ", String( Range( hom ) ), ")" );
end );

InstallMethod( PrintObj, [ IsQuiverRepresentationHomSpace ],
function( hom )
  Print( String( hom ) );
end );

InstallMethod( ViewObj, [ IsQuiverRepresentationHomSpace ],
function( hom )
  Print( String( hom ) );
end );

InstallMethod( \=, [ IsQuiverRepresentationHomSpace, IsQuiverRepresentationHomSpace ],
function( hom1, hom2 )
  return Source( hom1 ) = Source( hom2 )
         and Range( hom1 ) = Range( hom2 );
end );

InstallMethod( VertexHomSpaces, "for quiver representation hom space",
               [ IsQuiverRepresentationHomSpace ],
function( hom )
  local R1, R2, Q;
  R1 := Source( hom );
  R2 := Range( hom );
  Q := QuiverOfRepresentation( R1 );
  return List( Vertices( Q ),
               v -> Hom( VectorSpaceOfRepresentation( R1, v ),
                         VectorSpaceOfRepresentation( R2, v ) ) );
end );

InstallMethod( ArrowHomSpaces, "for quiver representation hom space",
               [ IsQuiverRepresentationHomSpace ],
function( hom )
  local R1, R2, Q;
  R1 := Source( hom );
  R2 := Range( hom );
  Q := QuiverOfRepresentation( R1 );
  return List( Arrows( Q ),
               a -> Hom( VectorSpaceOfRepresentation( R1, Source( a ) ),
                         VectorSpaceOfRepresentation( R2, Target( a ) ) ) );
end );

InstallMethod( SumOfVertexHomSpaces, "for quiver representation hom space",
               [ IsQuiverRepresentationHomSpace ],
               hom -> DirectSum( VertexHomSpaces( hom ) ) );

InstallMethod( SumOfArrowHomSpaces, "for quiver representation hom space",
               [ IsQuiverRepresentationHomSpace ],
               hom -> DirectSum( ArrowHomSpaces( hom ) ) );

InstallMethod( HomSpaceVertexToArrowMap, "for quiver representation hom space",
               [ IsQuiverRepresentationHomSpace ],
function( hom )
  local R1, R2, Q, vertex_spaces, arrow_spaces, morphisms, v, 
        morphisms_v, a, m;
  R1 := Source( hom );
  R2 := Range( hom );
  Q := QuiverOfRepresentation( R1 );
  # arrow a: v1 -> v2
  #             a1
  # R1       .  -> .
  #        f v  \  v g
  # R2       .  -> .
  #             a2
  #
  # sumofvertex -> sumofarrow
  # (f,g) -> [a1,g] - [f,a2]
  vertex_spaces := VertexHomSpaces( hom );
  arrow_spaces := ArrowHomSpaces( hom );
  morphisms := [];
  for v in Vertices( Q ) do
    morphisms_v := [];
    for a in Arrows( Q ) do
      if v = Source( a ) then
        m := - Hom( VectorSpaceOfRepresentation( R1, v ),
                    MapForArrow( R2, a ) );
        # f |-> PreCompose( f, MapForArrow( R2, a ) )
      elif v = Target( a ) then
        m := Hom( MapForArrow( R1, a ),
                  VectorSpaceOfRepresentation( R2, v ) );
        # g |-> PreCompose( MapForArrow( R1, a ), g )
      else
        m := ZeroMorphism( vertex_spaces[ VertexNumber( v ) ],
                           arrow_spaces[ ArrowNumber( a ) ] );
      fi;
      Add( morphisms_v, m );
    od;
    Add( morphisms, morphisms_v );
  od;
  
  return MorphismBetweenDirectSums( SumOfVertexHomSpaces( hom ),
                                    morphisms,
                                    SumOfArrowHomSpaces( hom ) );
end );

DeclareSideOperations( IsQuiverModuleHomSpace,
                       IsLeftQuiverModuleHomSpace, IsRightQuiverModuleHomSpace,
                       IsQuiverBimoduleHomSpace );

BindGlobal( "FamilyOfQuiverModuleHomSpaces",
            NewFamily( "quiver module hom spaces" ) );
DeclareRepresentation( "IsQuiverModuleHomSpaceRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [ ] );

InstallMethod( Hom,
               [ IsQuiverModule, IsQuiverModule ],
function( M1, M2 )
  local rep_hom, F, cat, type, hom;
  if not IsIdenticalObj( CapCategory( M1 ), CapCategory( M2 ) ) then
    Error( "modules from different categories" );
  fi;
  rep_hom := Hom( UnderlyingRepresentation( M1 ),
                  UnderlyingRepresentation( M2 ) );
  F := UnderlyingField( rep_hom );
  cat := CategoryOfVectorSpaces( F );
  type := NewType( FamilyOfQPAVectorSpaces,
                   IsQuiverModuleHomSpace and IsQuiverModuleHomSpaceRep );
  hom := rec();
  ObjectifyWithAttributes( hom, type,
                           Source, M1,
                           Range, M2,
                           Side, Side( M1 ),
                           UnderlyingField, F,
                           UnderlyingRepresentationHomSpace, rep_hom );
  Add( cat, hom );
  return Intern( hom );
end );

InstallMethod( CanonicalBasis, [ IsQuiverModuleHomSpace ],
function( hom )
  local basis, basis_vectors;
  basis_vectors := List( CanonicalBasis( UnderlyingRepresentationHomSpace( hom ) ),
                         AsModuleHomomorphism ^ Side( hom ) );
  basis := rec();
  ObjectifyWithAttributes( basis,
                           NewType( FamilyOfVectorSpaceBases,
                                    IsBasis and IsVectorSpaceBasisRep ),
                           BasisVectors, basis_vectors,
                           UnderlyingLeftModule, hom );
  return basis;
end );

InstallMethod( Dimension, [ IsQuiverModuleHomSpace ],
               hom -> Length( CanonicalBasis( hom ) ) );

InstallMethod( String, [ IsQuiverModuleHomSpace ],
function( hom )
  return Concatenation( "Hom(", String( Source( hom ) ),
                        ", ", String( Range( hom ) ), ")" );
end );

InstallMethod( PrintObj, [ IsQuiverModuleHomSpace ],
function( hom )
  Print( String( hom ) );
end );

InstallMethod( ViewObj, [ IsQuiverModuleHomSpace ],
function( hom )
  Print( String( hom ) );
end );

InstallMethod( \=, [ IsQuiverModuleHomSpace, IsQuiverModuleHomSpace ],
function( hom1, hom2 )
  return Source( hom1 ) = Source( hom2 )
         and Range( hom1 ) = Range( hom2 );
end );

InstallMethod( BasisOfHom,
        "for two representations of a quiver",
        [ IsQuiverRepresentation, IsQuiverRepresentation ],
        function( R1, R2 )

  local   A,  F,  vertices,  dim_R1,  dim_R2,  num_vert,  support_R1,  
          support_R2,  i,  num_cols,  num_rows,  block_intervals,  
          block_rows,  block_cols,  prev_col,  prev_row,  a,  
          source_arrow,  target_arrow,  equations,  arrows,  mats_R1,  
          mats_R2,  j,  row_start_pos, row_end_pos, col_start_pos, col_end_pos,
          m, n, b, homs, dim_hom, hom_basis, map, k, y, x, mat;

  A := AlgebraOfRepresentation( R1 ); 
  if A <> AlgebraOfRepresentation( R2 ) then
    Print("The two modules entered are not modules over the same algebra.");
    return fail;
  fi;
  F := LeftActingDomain(A);
  #
  # Finding the support of R1 and R2 
  # 
  vertices := Vertices( QuiverOfAlgebra( A ) );
  dim_R1 := DimensionVector( R1 );
  dim_R2 := DimensionVector( R2 );
  num_vert := Length( dim_R1 );   
  support_R1 := [];
  support_R2 := [];
  for i in [ 1..num_vert ] do
    if ( dim_R1[ i ] <> 0 ) then 
      AddSet( support_R1, i );
    fi;
    if ( dim_R2[ i ] <> 0 ) then 
      AddSet( support_R2, i );
    fi;
  od;
  #
  # Deciding the size of the equations, 
  # number of columns and rows
  #
  num_cols := 0;
  num_rows := 0;
  block_intervals := [];
  block_rows := [];
  block_cols := [];
  prev_col := 0;
  prev_row := 0;
  for i in support_R1 do
    num_rows := num_rows + dim_R1[ i ] * dim_R2[ i ];
    block_rows[ i ] := prev_row + 1;
    prev_row := num_rows;
    for a in OutgoingArrows( vertices[ i ] ) do
      source_arrow := VertexNumber( Source( a ) );
      target_arrow := VertexNumber( Target( a ) );
      if ( target_arrow in support_R2 ) and 
         ( ( source_arrow in support_R2 ) or ( target_arrow in support_R1 ) ) then 
        num_cols := num_cols + dim_R1[ source_arrow ] * dim_R2[ target_arrow ];
        Add( block_cols, [ a, prev_col + 1, num_cols ] );
      fi;
      prev_col := num_cols; 
    od;
  od;
  if num_rows = 0 then
    return [];
  fi;
  #
  # Finding the linear equations for the maps between M and N
  #
  equations := MutableNullMat( num_rows, num_cols, F);
  arrows := Arrows( QuiverOfAlgebra( A ) );
  mats_R1 := List( MapsOfRepresentation( R1 ), m -> RowsOfMatrix( RightMatrixOfLinearTransformation( m ) ) ); 
  mats_R2 := List( MapsOfRepresentation( R2 ), m -> RowsOfMatrix( RightMatrixOfLinearTransformation( m ) ) ); 
  prev_col := 0;
  prev_row := 0;
  for i in support_R1 do
    for a in OutgoingArrows( vertices[i] ) do
      source_arrow := VertexNumber( Source( a ) );
      target_arrow := VertexNumber( Target( a ) );
      if ( target_arrow in support_R2 ) and 
         ( ( source_arrow in support_R2 ) or ( target_arrow in support_R1 ) ) then
        for j in [ 1..dim_R1[ source_arrow ] ] do
          row_start_pos := block_rows[ source_arrow ] + ( j - 1 ) * dim_R2[ source_arrow ]; 
          row_end_pos := block_rows[ source_arrow ] - 1 + j * dim_R2[ source_arrow ];
          col_start_pos := prev_col + 1 + ( j-1 ) * dim_R2[ target_arrow ];
          col_end_pos := prev_col + j * dim_R2[ target_arrow ];
          if ( source_arrow in support_R2 ) then 
            equations{ [ row_start_pos..row_end_pos ] }{ [ col_start_pos..col_end_pos ] } := 
              mats_R2[ ArrowNumber( a ) ];
          fi;
          if ( target_arrow in support_R1 ) then 
            for m in [ 1..DimensionsMat( mats_R1[ ArrowNumber( a ) ] )[ 2 ] ] do
              for n in [ 1..dim_R2[ target_arrow ] ] do
                b := block_rows[ target_arrow ] + (m - 1) * dim_R2[ target_arrow ];
                equations[ b + n - 1 ][ col_start_pos + n - 1 ] := 
                  equations[ b + n - 1 ][ col_start_pos + n - 1 ] + (-1) * mats_R1[ ArrowNumber( a ) ][ j ][ m ];
              od;
            od;
          fi;
        od;
        prev_col := prev_col + dim_R1[ source_arrow ] * dim_R2[ target_arrow ];
      fi;
    od;
  od;
  #
  # Creating the maps between the module M and N
  #
  if num_cols = 0 then 
    equations := NullMat( num_rows, num_cols + 1, F);
  fi;
  homs := [];
  dim_hom := 0;
  hom_basis := NullspaceMat( equations );
  for b in hom_basis do
    map := [];
    dim_hom := dim_hom + 1;
    k := 1;
    for i in [ 1..num_vert ] do 
      if ( dim_R1[ i ] <> 0 ) and ( dim_R2[ i ] <> 0 ) then 
        mat := MutableNullMat( dim_R1[ i ], dim_R2[ i ], F );
        for y in [ 1..dim_R1[ i ] ] do 
          for x in [ 1..dim_R2[ i ] ] do 
            mat[ y ][ x ] := b[ k ];
            k := k + 1;
          od;
        od;
        map[ i ] := MatrixByRows( F, mat );
      fi;
    od;
    homs[dim_hom] := QuiverRepresentationHomomorphismByRightMatrices( R1, R2, map );
  od;
  
  return homs;
end
  );
