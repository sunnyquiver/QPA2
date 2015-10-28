BindGlobal( "FamilyOfQuiverRepresentationElements",
            NewFamily( "quiver representation elements" ) );
BindGlobal( "FamilyOfQuiverRepresentations",
            CollectionsFamily( FamilyOfQuiverRepresentationElements ) );

DeclareRepresentation( "IsQuiverRepresentationElementRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [ "representation", "vectors" ] );

InstallMethod( QuiverRepresentationElement, "for quiver representation and collection",
               [ IsQuiverRepresentation, IsDenseList ],
function( R, vectors )
  local field, Q, numVertices, i, v, space;
  vectors := Immutable( vectors );
  field := FieldOfRepresentation( R );
  Q := QuiverOfRepresentation( R );
  numVertices := Length( Vertices( Q ) );
  if Length( vectors ) <> numVertices then
    Error( "Wrong number of vectors in QuiverRepresentationElement constructor ",
           "(", Length( vectors ), " given, expected ", numVertices, ")" );
  fi;
  for i in [ 1 .. numVertices ] do
    v := vectors[ i ];
    space := VectorSpaceOfRepresentation( R, i );
    if not v in space then
      Error( "Vector ", v, " given for vertex ", Vertex( Q, i ),
             " is not in the vector space ", space );
    fi;
  od;
  return QuiverRepresentationElementNC( R, vectors );
end );

InstallMethod( QuiverRepresentationElementNC, "for quiver representation and collection",
               [ IsQuiverRepresentation, IsDenseList ],
function( R, vectors )
  local type, elem;
  type := NewType( FamilyOfQuiverRepresentationElements,
                   IsQuiverRepresentationElement and IsQuiverRepresentationElementRep );
  elem := rec();
  ObjectifyWithAttributes( elem, type,
                           RepresentationOfElement, R,
                           ElementVectors, vectors );
  return elem;
end );

InstallMethod( QuiverRepresentationElementByVertices, "for quiver representation and dense lists",
               [ IsQuiverRepresentation, IsDenseList, IsDenseList ],
function( R, vertices, vectors )
  local num_vectors, i, rep_vectors, vertex_number;

  num_vectors := Length( vectors );
  if Length( vertices ) <> num_vectors then
    Error( "Different lengths of list of vertices (", Length( vertices ),
           ") and list of vectors (", num_vectors, ")" );
  fi;
  for i in [ 1 .. num_vectors ] do
    if not IsVertex( vertices[ i ] ) then
      Error( "Not a vertex: ", vertices[ i ] );
    fi;
    if not vertices[ i ] in QuiverOfRepresentation( R ) then
      Error( "Vertex ", vertices[ i ], " is not in the quiver of the representation ", R );
    fi;
  od;
      
  rep_vectors := List( VectorSpacesOfRepresentation( R ), Zero );
  for i in [ 1 .. num_vectors ] do
    vertex_number := VertexNumber( vertices[ i ] );
    rep_vectors[ vertex_number ] := vectors[ i ];
  od;

  return QuiverRepresentationElement( R, rep_vectors );
end );

InstallMethod( \in,
               [ IsQuiverRepresentationElement,
                 IsQuiverRepresentation ],
function( e, R )
  return RepresentationOfElement( e ) = R;
end );

InstallMethod( PrintObj, "for element of quiver representation",
               [ IsQuiverRepresentationElement ],
function( e )
  Print( "<quiver representation element ",
         ElementVectors( e ),
         ">" );
end );

InstallMethod( RepresentationOfElement, "for quiver representation element",
               [ IsQuiverRepresentationElement and IsQuiverRepresentationElementRep ],
function( e )
  return e!.representation;
end );

InstallMethod( ElementVectors, "for quiver representation element",
               [ IsQuiverRepresentationElement and IsQuiverRepresentationElementRep ],
function( e )
  return e!.vectors;
end );

InstallMethod( ElementVector, "for quiver representation element and positive integer",
               [ IsQuiverRepresentationElement, IsPosInt ],
function( e, i )
  return ElementVectors( e )[ i ];
end );

InstallMethod( ElementVector, "for quiver representation element and vertex",
               [ IsQuiverRepresentationElement, IsVertex ],
function( e, v )
  return ElementVectors( e )[ VertexNumber( v ) ];
end );

InstallMethod( PathAction,
               "for element of quiver representation and path",
               [ IsQuiverRepresentationElement, IsPath ],
function( e, p )
  local R, M, source_vec, target_vec, mult;
  R := RepresentationOfElement( e );
  M := MapForPath( R, p );
  source_vec := ElementVector( e, Source( p ) );
  target_vec := ImageElm( M, source_vec );
  return QuiverRepresentationElementByVertices( R, [ Target( p ) ], [ target_vec ] );
end );

InstallMethod( QuiverAlgebraAction,
               "for element of quiver representation and element of quiver algebra",
               [ IsQuiverRepresentationElement, IsQuiverAlgebraElement ],
function( re, ae )
  local Cs, Ps;
  Cs := Coefficients( ae );
  Ps := Paths( ae );
  return Sum( ListN( Cs, List( Ps, p -> PathAction( re, p ) ),
                     \* ) );
end );

InstallMethod( \=, "for elements of quiver representation",
               [ IsQuiverRepresentationElement, IsQuiverRepresentationElement ],
function( e1, e2 )
  return RepresentationOfElement( e1 ) = RepresentationOfElement( e2 )
         and ElementVectors( e1 ) = ElementVectors( e2 );
end );

InstallMethod( \+, "for elements of quiver representation",
               [ IsQuiverRepresentationElement, IsQuiverRepresentationElement ],
function( e1, e2 )
  return QuiverRepresentationElementNC( RepresentationOfElement( e1 ),
                                        ListN( ElementVectors( e1 ),
                                               ElementVectors( e2 ),
                                               \+ ) );
end );

InstallMethod( AdditiveInverse, "for element of quiver representation",
               [ IsQuiverRepresentationElement ],
function( e )
  return QuiverRepresentationElementNC
         ( RepresentationOfElement( e ),
           List( ElementVectors( e ), AdditiveInverse ) );
end );

InstallMethod( \*, "for multiplicative element and element of quiver representation",
               [ IsMultiplicativeElement, IsQuiverRepresentationElement ],
function( c, e )
  local R;
  R := RepresentationOfElement( e );
  if c in FieldOfRepresentation( R ) then
    return QuiverRepresentationElementNC( R,
                                          List( ElementVectors( e ), v -> c * v ) );
  else
    TryNextMethod();
  fi;
end );

InstallMethod( \*, "for element of quiver representation and multiplicative element",
               [ IsQuiverRepresentationElement, IsMultiplicativeElement ],
function( e, c )
  local R;
  R := RepresentationOfElement( e );
  if c in FieldOfRepresentation( R ) then
    return c * e;
  else
    TryNextMethod();
  fi;
end );

# TODO module structure


DeclareRepresentation( "IsQuiverRepresentationRep", IsComponentObjectRep and IsAttributeStoringRep,
                       [ "algebra", "dimensions", "matrices" ] );

InstallMethod( QuiverRepresentation, "for path algebra and dense lists",
               [ IsPathAlgebra, IsDenseList, IsDenseList ],
function( A, dimensions, matrices )
  return QuiverRepresentation( CategoryOfQuiverRepresentations( A ),
                               dimensions, matrices );
end );

InstallMethod( QuiverRepresentationNC, "for path algebra and dense lists",
               [ IsPathAlgebra, IsDenseList, IsDenseList ],
function( A, dimensions, matrices )
  return QuiverRepresentationNC( CategoryOfQuiverRepresentations( A ),
                                 dimensions, matrices );
  # local field, vecspace_type, make_vecspace, make_morphism;
  # field := LeftActingDomain( A );
  # vecspace_type := VectorSpaceTypeForRepresentations( A );
  # make_vecspace := dim -> MakeQPAVectorSpace( vecspace_type, field, dim );
  # make_morphism := mat -> MakeLinearTransformation( vecspace_type, field, mat );
  # return QuiverRepresentationByObjectsAndMorphismsNC
  #        ( A, List( dimensions, make_vecspace ), List( matrices, make_morphism ) );
end );

InstallMethod( QuiverRepresentation, "for representation category and dense lists",
               [ IsQuiverRepresentationCategory, IsDenseList, IsDenseList ],
function( cat, dimensions, matrices )
  local vecspace_cat;
  vecspace_cat := VectorSpaceCategory( cat );
  return QuiverRepresentationByObjectsAndMorphisms
         ( cat,
           List( dimensions, VectorSpaceConstructor( vecspace_cat ) ),
           List( matrices, LinearTransformationConstructor( vecspace_cat ) ) );
end );

InstallMethod( QuiverRepresentationNC, "for representation category and dense lists",
               [ IsQuiverRepresentationCategory, IsDenseList, IsDenseList ],
function( cat, dimensions, matrices )
  local vecspace_cat;
  vecspace_cat := VectorSpaceCategory( cat );
  return QuiverRepresentationByObjectsAndMorphismsNC
         ( cat,
           List( dimensions, VectorSpaceConstructor( vecspace_cat ) ),
           List( matrices, LinearTransformationConstructor( vecspace_cat ) ) );
end );

InstallMethod( QuiverRepresentationByObjectsAndMorphisms,
               [ IsQuiverRepresentationCategory, IsDenseList, IsDenseList ],
function( cat, objects, morphisms )
  local vecspace_cat, A, Q, vertices, numVertices, arrows, numArrows, i,
        src, correct_src, rng, correct_rng;

  A := AlgebraOfCategory( cat );
  Q := QuiverOfAlgebra( A );
  vertices := Vertices( Q );
  numVertices := Length( vertices );
  arrows := Arrows( Q );
  numArrows := Length( arrows );
  vecspace_cat := VectorSpaceCategory( cat );

  if Length( objects ) <> numVertices then
    Error( "Wrong number of objects ",
           "(", Length( objects ), " given, expected ", numVertices, ")" );
  fi;
  for i in [ 1 .. numVertices ] do
    if not ( IsCapCategoryObject( objects[ i ] )
             and CapCategory( objects[ i ] ) = vecspace_cat ) then
      Error( "Object ", objects[ i ], " for vertex ", vertices[ i ],
             " is not a CAP object in the correct category" );
    fi;
  od;
  if Length( morphisms ) <> numArrows then
    Error( "Wrong number of morphisms in QuiverRepresentation constructor ",
           "(", Length( morphisms ), " given, expected ", numArrows, ")" );
  fi;
  for i in [ 1 .. numArrows ] do
    if not ( IsCapCategoryMorphism( morphisms[ i ] ) and
             CapCategory( morphisms[ i ] ) = vecspace_cat ) then
      Error( "Morphism ", morphisms[ i ], " for arrow ", arrows[ i ],
             " is not a CAP object in the correct category" );
    fi;
    src := Source( morphisms[ i ] );
    correct_src := objects[ VertexNumber( Source( arrows[ i ] ) ) ];
    rng := Range( morphisms[ i ] );
    correct_rng := objects[ VertexNumber( Target( arrows[ i ] ) ) ];
    if not IsEqualForObjects( src, correct_src ) then
      Error( "Morphism ", morphisms[ i ], " for arrow ", arrows[ i ],
             " has wrong source (is ", src, ", should be ", correct_src, ")" );
    fi;
    if not IsEqualForObjects( rng, correct_rng ) then
      Error( "Morphism ", morphisms[ i ], " for arrow ", arrows[ i ],
             " has wrong source (is ", rng, ", should be ", correct_rng, ")" );
    fi;
  od;
  return QuiverRepresentationByObjectsAndMorphismsNC( cat, objects, morphisms );
end );

InstallMethod( QuiverRepresentationByObjectsAndMorphismsNC,
               [ IsQuiverRepresentationCategory, IsDenseList, IsDenseList ],
function( cat, objects, morphisms )
  local A, repType, R;
  A := AlgebraOfCategory( cat );
  repType := NewType( FamilyOfQuiverRepresentations,
                      IsQuiverRepresentation and IsQuiverRepresentationRep );
  R := rec();
  ObjectifyWithAttributes
    ( R, repType,
      AlgebraOfRepresentation, A,
      VectorSpacesOfRepresentation, objects,
      MapsOfRepresentation, morphisms,
      MatricesOfRepresentation, List( morphisms, MatrixOfLinearTransformation ),
      DimensionVector, List( objects, Dimension ) );
  Add( cat, R );
  return R;
end );

InstallMethod( UnderlyingCategoryForRepresentations, [ IsQuiverAlgebra ],
function( A )
  if IsLeftQuiver( QuiverOfAlgebra( A ) ) then
    return CategoryOfColSpaces( LeftActingDomain( A ) );
  else
    return CategoryOfRowSpaces( LeftActingDomain( A ) );
  fi;
end );

InstallMethod( VectorSpaceTypeForRepresentations, [ IsQuiverAlgebra ],
function( A )
  if IsLeftQuiver( QuiverOfAlgebra( A ) ) then
    return "col";
  else
    return "row";
  fi;
end );

InstallMethod( AsRepresentationOfQuotientAlgebra,
               "for quiver representation and quotient of path algebra",
               [ IsQuiverRepresentation, IsQuotientOfPathAlgebra ],
function( R, A )
  local kQ, rels, rel;
  kQ := PathAlgebra( A );
  rels := RelationsOfAlgebra( A );
  for rel in rels do
    if not IsZero( MapForAlgebraElement( R, rel ) ) then
      Error( "Not a well-defined representation of the algebra ", A,
             "; does not respect the relation ", rel );
    fi;
  od;
  return QuiverRepresentationNC( A, DimensionVector( R ),
                                 MatricesOfRepresentation( R ) );
end );

InstallMethod( QuiverRepresentation, "for quotient of path algebra and dense lists",
               [ IsQuotientOfPathAlgebra, IsDenseList, IsDenseList ],
function( A, dimensions, matrices )
  local kQ, rels, kQrep, rel;
  kQ := PathAlgebra( A );
  rels := RelationsOfAlgebra( A );
  kQrep := QuiverRepresentation( kQ, dimensions, matrices );
  return AsRepresentationOfQuotientAlgebra( kQrep, A );
end );

InstallMethod( QuiverRepresentationByArrows, "for quiver algebra and dense lists",
               [ IsQuiverAlgebra, IsDenseList, IsDenseList, IsDenseList ],
function( A, dimensions, arrows, matrices )
  return QuiverRepresentationByArrows
         ( CategoryOfQuiverRepresentations( A ),
           dimensions, arrows, matrices );
end );

InstallMethod( QuiverRepresentationByArrows, "for quiver algebra and dense lists",
               [ IsQuiverRepresentationCategory, IsDenseList, IsDenseList, IsDenseList ],
function( cat, dimensions, arrows, matrices )
  local A, field, Q, all_arrows, all_matrices, num_specified_arrows, i;
  A := AlgebraOfCategory( cat );
  field := LeftActingDomain( A );
  Q := QuiverOfAlgebra( A );
  all_arrows := Arrows( Q );
  all_matrices :=
    List( all_arrows,
          a ->
          MakeZeroMatrix( dimensions[ VertexNumber( LeftEnd( a ) ) ],
                          dimensions[ VertexNumber( RightEnd( a ) ) ],
                          field ) );
  num_specified_arrows := Length( arrows );
  if num_specified_arrows <> Length( matrices ) then
    Error( "Length of arrow list not the same as length of matrix list" );
  fi;
  for i in [ 1 .. num_specified_arrows ] do
    if DimensionsMat( matrices[ i ] ) <>
       DimensionsMat( all_matrices[ ArrowNumber( arrows[ i ] ) ] ) then
      Error( "Wrong dimensions of matrix for arrow ", arrows[ i ],
             " (dimensions are ", DimensionsMat( matrices[ i ] ),
             ", should be ",
             DimensionsMat( all_matrices[ ArrowNumber( arrows[ i ] ) ] ),
             ")" );
    fi;
    all_matrices[ ArrowNumber( arrows[ i ] ) ] := matrices[ i ];
  od;
  if IsPathAlgebra( A ) then
    return QuiverRepresentationNC( cat, dimensions, all_matrices );
  else
    return QuiverRepresentation( cat, dimensions, all_matrices );
  fi;
end );

InstallMethod( ZeroRepresentation, "for quiver algebra",
               [ IsQuiverAlgebra ],
function( A )
  local numVertices;
  numVertices := NumberOfVertices( QuiverOfAlgebra( A ) );
  return QuiverRepresentationByArrows( A,
                                       List( [ 1 .. numVertices ],
                                             i -> 0 ),
                                       [], [] );
end );

InstallMethod( \=, [ IsQuiverRepresentation, IsQuiverRepresentation ],
               IsEqualForObjects );

InstallMethod( PrintObj, "for quiver representation",
               [ IsQuiverRepresentation ],
function( R )
  Print( "<quiver representation with dimensions ",
         DimensionVector( R ),
         " over ",
         AlgebraOfRepresentation( R ),
         ">" );
end );

InstallMethod( String, "for quiver representation",
               [ IsQuiverRepresentation ],
function( R )
  return JoinStringsWithSeparator( DimensionVector( R ),
                                   "," );
end );

InstallMethod( ViewObj, "for quiver representation",
               [ IsQuiverRepresentation ],
function( R )
  Print( "<", String( R ), ">" );
end );

InstallMethod( Zero, "for quiver representation",
               [ IsQuiverRepresentation ],
function( R )
  return QuiverRepresentationElementByVertices( R, [], [] );
end );

InstallMethod( VectorSpaceOfRepresentation, "for quiver representation and positive integer",
               [ IsQuiverRepresentation, IsPosInt ],
function( R, i )
  return VectorSpacesOfRepresentation( R )[ i ];
end );

InstallMethod( VectorSpaceOfRepresentation, "for quiver representation and vertex",
               [ IsQuiverRepresentation, IsVertex ],
function( R, v )
  return VectorSpacesOfRepresentation( R )[ VertexNumber( v ) ];
end );

InstallMethod( VertexDimension, "for quiver representation and positive integer",
               [ IsQuiverRepresentation, IsPosInt ],
function( R, i )
  return DimensionVector( R )[ i ];
end );

InstallMethod( VertexDimension, "for quiver representation and vertex",
               [ IsQuiverRepresentation, IsVertex ],
function( R, v )
  return VertexDimension( R, VertexNumber( v ) );
end );

InstallMethod( MapForArrow, "for quiver representation and positive integer",
               [ IsQuiverRepresentation, IsPosInt ],
function( R, i )
  return MapsOfRepresentation( R )[ i ];
end );

InstallMethod( MapForArrow, "for quiver representation and arrow",
               [ IsQuiverRepresentation, IsArrow ],
function( R, a )
  return MapForArrow( R, ArrowNumber( a ) );
end );

InstallMethod( MapForPath, "for quiver representation and vertex",
               [ IsQuiverRepresentation, IsVertex ],
function( R, v )
  return IdentityMat( VertexDimension( R, v ),
                      FieldOfRepresentation( R ) );
end );

InstallMethod( MapForPath, "for quiver representation and arrow",
               [ IsQuiverRepresentation, IsArrow ],
               MapForArrow );

InstallMethod( MapForPath, "for quiver representation and composite path",
               [ IsQuiverRepresentation, IsCompositePath ],
function( R, p )
  return PreCompose( List( ArrowListLR( p ), a -> MapForArrow( R, a ) ) );
end );

InstallMethod( MapForAlgebraElement, "for quiver representation and uniform quiver algebra element",
               [ IsQuiverRepresentation, IsQuiverAlgebraElement ],
function( R, e )
  return Sum( ListN( Coefficients( e ),
                     List( Paths( e ), p -> MapForPath( R, p ) ),
                     \* ) );
end );

InstallMethod( QuiverOfRepresentation, "for quiver representation",
               [ IsQuiverRepresentation ],
function( R )
  return QuiverOfAlgebra( AlgebraOfRepresentation( R ) );
end );

InstallMethod( FieldOfRepresentation, "for quiver representation",
               [ IsQuiverRepresentation ],
function( R )
  return LeftActingDomain( AlgebraOfRepresentation( R ) );
end );

InstallMethod( LeftActingDomain, "for quiver representation",
               [ IsQuiverRepresentation ],
               FieldOfRepresentation );

BindGlobal( "FamilyOfQuiverRepresentationBases",
            NewFamily( "quiver representation bases" ) );

DeclareRepresentation( "IsQuiverRepresentationBasisRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [ "representation", "basisVectors" ] );

InstallMethod( CanonicalBasis, "for quiver representation",
               [ IsQuiverRepresentation ],
function( R )
  local Q, vertices, spaces, basis_elements, i, vertex_basis,
        basis_vector, basis;
  Q := QuiverOfRepresentation( R );
  vertices := Vertices( Q );
  spaces := VectorSpacesOfRepresentation( R );
  basis_elements := [];
  for i in [ 1 .. Length( vertices ) ] do
    vertex_basis := CanonicalBasis( spaces[ i ] );
    for basis_vector in vertex_basis do
      Add( basis_elements,
           QuiverRepresentationElementByVertices
           ( R, [ vertices[ i ] ], [ basis_vector ] ) );
    od;
  od;
  basis := rec();
  ObjectifyWithAttributes
    ( basis,
      NewType( FamilyOfQuiverRepresentationBases,
               IsBasis and IsQuiverRepresentationBasisRep ),
      BasisVectors, basis_elements,
      UnderlyingLeftModule, R );
  return basis;
end );

InstallMethod( Basis, "for quiver representation",
               [ IsQuiverRepresentation ],
               CanonicalBasis );

InstallMethod( Coefficients, "for quiver representation basis and quiver representation element",
               [ IsBasis and IsQuiverRepresentationBasisRep,
                 IsQuiverRepresentationElement ],
function( B, e )
  local R, coefficients_of_vector;
  R := UnderlyingLeftModule( B );
  coefficients_of_vector := function( V, v )
    return Coefficients( CanonicalBasis( V ), v );
  end;
  return Concatenation
         ( ListN( VectorSpacesOfRepresentation( R ),
                  ElementVectors( e ),
                  coefficients_of_vector ) );
end );

BindGlobal( "FamilyOfQuiverRepresentationHomomorphisms",
            NewFamily( "quiver representation homomorphisms" ) );

# We need this because the builtin global function Image uses
# the FamilySource attribute to check whether a given map can
# be applied to a given element.
SetFamilySource( FamilyOfQuiverRepresentationHomomorphisms,
                 FamilyOfQuiverRepresentationElements );

DeclareRepresentation( "IsQuiverRepresentationHomomorphismRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [ ] );

InstallMethod( QuiverRepresentationHomomorphism,
               "for quiver representations and dense list",
               [ IsQuiverRepresentation, IsQuiverRepresentation,
                 IsDenseList ],
function( source, range, matrices )
  local vecspace_cat;
  vecspace_cat := VectorSpaceCategory( CapCategory( source ) );
  return QuiverRepresentationHomomorphismByMorphisms
         ( source, range,
           List( matrices, LinearTransformationConstructor( vecspace_cat ) ) );
end );

InstallMethod( QuiverRepresentationHomomorphismNC,
               "for quiver representations and dense list",
               [ IsQuiverRepresentation, IsQuiverRepresentation,
                 IsDenseList ],
function( source, range, matrices )
  local vecspace_cat;
  vecspace_cat := VectorSpaceCategory( CapCategory( source ) );
  return QuiverRepresentationHomomorphismByMorphismsNC
         ( source, range,
           List( matrices, LinearTransformationConstructor( vecspace_cat ) ) );
end );

InstallMethod( QuiverRepresentationHomomorphismByMorphisms,
               "for quiver representations and dense list",
               [ IsQuiverRepresentation, IsQuiverRepresentation,
                 IsDenseList ],
function( source, range, maps )
  local A, Q, i, src, correct_src, rng, correct_rng, arrow, comp1, comp2;
  A := AlgebraOfRepresentation( source );
  if A <> AlgebraOfRepresentation( range ) then
    Error( "Source and range are representations of different algebras" );
  fi;
  Q := QuiverOfAlgebra( A );
  for i in [ 1 .. NumberOfVertices( Q ) ] do
    src := Source( maps[ i ] );
    correct_src := VectorSpaceOfRepresentation( source, i );
    rng := Range( maps[ i ] );
    correct_rng := VectorSpaceOfRepresentation( range, i );
    if src <> correct_src then
      Error( "Map for vertex ", Vertex( Q, i ), " has wrong source",
             " (is ", src, ", should be ", correct_src, ")" );
    fi;
    if rng <> correct_rng then
      Error( "Map for vertex ", Vertex( Q, i ), " has wrong range",
             " (is ", rng, ", should be ", correct_rng, ")" );
    fi;
  od;
  for arrow in Arrows( Q ) do
    comp1 := PreCompose( MapForArrow( source, arrow ),
                         maps[ VertexNumber( Target( arrow ) ) ] );
    comp2 := PreCompose( maps[ VertexNumber( Source( arrow ) ) ],
                         MapForArrow( range, arrow ) );
    if not IsEqualForMorphisms( comp1, comp2 ) then
      Error( "Does not commute with maps for arrow ", arrow );
    fi;
  od;
  return QuiverRepresentationHomomorphismByMorphismsNC( source, range, maps );
end );

InstallMethod( QuiverRepresentationHomomorphismByMorphismsNC,
               "for quiver representations and dense list",
               [ IsQuiverRepresentation, IsQuiverRepresentation,
                 IsDenseList ],
function( source, range, maps )
  local f;
  f := rec();
  ObjectifyWithAttributes
    ( f, NewType( FamilyOfQuiverRepresentationHomomorphisms,
                  IsQuiverRepresentationHomomorphism and
                  IsQuiverRepresentationHomomorphismRep ),
      Source, source,
      Range, range,
      MapsOfRepresentationHomomorphism, maps,
      MatricesOfRepresentationHomomorphism, List( maps, MatrixOfLinearTransformation ) );
  Add( CapCategory( source ), f );
  return f;
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

InstallMethod( \=, [ IsQuiverRepresentationHomomorphism, IsQuiverRepresentationHomomorphism ],
               IsEqualForMorphisms );

InstallMethod( ImageElm,
               [ IsQuiverRepresentationHomomorphism,
                 IsQuiverRepresentationElement ],
function( f, e )
  return QuiverRepresentationElement
         ( Range( f ),
           ListN( MatricesOfRepresentationHomomorphism( f ),
                  ElementVectors( e ),
                  ImageElm ) );
end );

InstallMethod( MapForVertex, "for quiver representation homomorphism and vertex",
               [ IsQuiverRepresentationHomomorphism, IsVertex ],
function( f, v )
  return MapsOfRepresentationHomomorphism( f )[ VertexNumber( v ) ];
end );

InstallMethod( MapForVertex, "for quiver representation homomorphism and positive integer",
               [ IsQuiverRepresentationHomomorphism, IsPosInt ],
function( f, i )
  return MapsOfRepresentationHomomorphism( f )[ i ];
end );



InstallMethod( CategoryOfQuiverRepresentations, "for quiver algebra",
               [ IsQuiverAlgebra ],
function( A )
  return CategoryOfQuiverRepresentationsOverVectorSpaceCategory
         ( A, UnderlyingCategoryForRepresentations( A ) );
end );

InstallMethod( CategoryOfQuiverRepresentationsOverVectorSpaceCategory,
               "for quiver algebra and vector space category",
               [ IsQuiverAlgebra, IsVectorSpaceCategory ],
function( A, vecspace_cat )
  local Q, cat, equal_objects, equal_morphisms, zero_object, zero_morphism,
        identity_morphism, pre_compose, addition, additive_inverse,
        kernel, kernel_emb, coker, coker_proj,
        mono_lift, epi_colift,
        direct_sum, direct_sum_inj, direct_sum_proj;

  Q := QuiverOfAlgebra( A );

  cat := CreateCapCategory( Concatenation( "quiver representations over ", String( A ) ) );
  SetFilterObj( cat, IsQuiverRepresentationCategory );
  SetAlgebraOfCategory( cat, A );
  SetVectorSpaceCategory( cat, vecspace_cat );

  SetIsAbelianCategory( cat, true );

  equal_objects := function( R1, R2 )
    return AlgebraOfRepresentation( R1 ) = AlgebraOfRepresentation( R2 ) and
           DimensionVector( R1 ) = DimensionVector( R2 ) and
           ForAll( ListN( MapsOfRepresentation( R1 ),
                          MapsOfRepresentation( R2 ),
                          IsEqualForMorphisms ),
                   IdFunc );
  end;
  AddIsEqualForObjects( cat, equal_objects );

  equal_morphisms := function( m1, m2 )
    return ForAll( ListN( MapsOfRepresentationHomomorphism( m1 ),
                          MapsOfRepresentationHomomorphism( m2 ),
                          IsEqualForMorphisms),
                   IdFunc );
  end;
  AddIsEqualForMorphisms( cat, equal_morphisms );

  zero_object := function()
    return QuiverRepresentationByArrows
           ( cat, List( Vertices( Q ), v -> 0 ), [], [] );
  end;
  AddZeroObject( cat, zero_object );

  zero_morphism := function( R1, R2 )
    return QuiverRepresentationHomomorphismByMorphisms
           ( R1, R2,
             ListN( VectorSpacesOfRepresentation( R1 ),
                    VectorSpacesOfRepresentation( R2 ),
                    ZeroMorphism ) );
  end;
  AddZeroMorphism( cat, zero_morphism );

  # TODO: need IsZeroForMorphisms?

  identity_morphism := function( R )
    return QuiverRepresentationHomomorphismByMorphisms
           ( R, R,
             List( VectorSpacesOfRepresentation( R ),
                   IdentityMorphism ) );
  end;
  AddIdentityMorphism( cat, identity_morphism );

  pre_compose := function( m1, m2 )
    return QuiverRepresentationHomomorphismByMorphisms
           ( Source( m1 ), Range( m2 ),
             ListN( MapsOfRepresentationHomomorphism( m1 ),
                    MapsOfRepresentationHomomorphism( m2 ),
                    PreCompose ) );
  end;
  AddPreCompose( cat, pre_compose );

  addition := function( m1, m2 )
    return QuiverRepresentationHomomorphismByMorphisms
           ( Source( m1 ), Range( m1 ),
             ListN( MapsOfRepresentationHomomorphism( m1 ),
                    MapsOfRepresentationHomomorphism( m2 ),
                    AdditionForMorphisms ) );
  end;
  AddAdditionForMorphisms( cat, addition );

  additive_inverse := function( m )
    return QuiverRepresentationHomomorphismByMorphisms
           ( Source( m ), Range( m ),
             List( MapsOfRepresentationHomomorphism( m ),
                   AdditiveInverse ) );
  end;
  AddAdditiveInverseForMorphisms( cat, additive_inverse );

  kernel_emb := function( m )
    local emb_maps, ker_objs, map_for_arrow, ker;
    emb_maps := List( MapsOfRepresentationHomomorphism( m ),
                      KernelEmbedding );
    ker_objs := List( emb_maps, Source );
    map_for_arrow := function( a )
      return KernelObjectFunctorial
             ( MapForVertex( m, Source( a ) ),
               MapForArrow( Source( m ), a ),
               MapForVertex( m, Target( a ) ) );
    end;
    ker := QuiverRepresentationByObjectsAndMorphisms
           ( cat, ker_objs, List( Arrows( Q ), map_for_arrow ) );
    return QuiverRepresentationHomomorphismByMorphisms
           ( ker, Source( m ), emb_maps );
  end;
  AddKernelEmbedding( cat, kernel_emb );

  coker := function( m )
    local map_for_arrow;
    map_for_arrow := function( a )
      return CokernelFunctorial
             ( MapForVertex( m, Source( a ) ),
               MapForArrow( Range( m ), a ),
               MapForVertex( m, Target( a ) ) );
    end;
    return QuiverRepresentationByObjectsAndMorphisms
           ( cat,
             List( MapsOfRepresentationHomomorphism( m ),
                   CokernelObject ),
             List( Arrows( Q ),
                   map_for_arrow ) );
  end;
  coker_proj := function( m )
    return QuiverRepresentationHomomorphismByMorphisms
           ( Range( m ), coker( m ),
             List( MapsOfRepresentationHomomorphism( m ),
                   CokernelProjection ) );
  end;
  AddCokernelProjection( cat, coker_proj );

  mono_lift := function( i, test )
    return QuiverRepresentationHomomorphismByMorphisms
           ( Source( test ), Source( i ),
             ListN( MapsOfRepresentationHomomorphism( i ),
                    MapsOfRepresentationHomomorphism( test ),
                    LiftAlongMonomorphism ) );
  end;
  AddLiftAlongMonomorphism( cat, mono_lift );

  epi_colift := function( e, test )
    return QuiverRepresentationHomomorphismByMorphisms
           ( Range( e ), Range( test ),
             ListN( MapsOfRepresentationHomomorphism( e ),
                    MapsOfRepresentationHomomorphism( test ),
                    ColiftAlongEpimorphism ) );
  end;
  AddColiftAlongEpimorphism( cat, epi_colift );

  direct_sum := function( summands )
    return QuiverRepresentationByObjectsAndMorphisms
           ( cat,
             List( Transpose( List( summands,
                                    VectorSpacesOfRepresentation ) ),
                   DirectSum ),
             List( Transpose( List( summands,
                                    MapsOfRepresentation ) ),
                   DirectSumFunctorial ) );
  end;
  AddDirectSum( cat, direct_sum );

  direct_sum_inj := function( summands, i, sum )
    local map_for_vertex;
    map_for_vertex := function( v )
      return InjectionOfCofactorOfDirectSumWithGivenDirectSum
             ( List( summands, R -> VectorSpaceOfRepresentation( R, v ) ),
               i,
               VectorSpaceOfRepresentation( sum, v ) );
    end;
    return QuiverRepresentationHomomorphismByMorphisms
           ( summands[ i ], sum,
             ListN( Vertices( Q ), map_for_vertex ) );
  end;
  AddInjectionOfCofactorOfDirectSumWithGivenDirectSum( cat, direct_sum_inj );
  
  direct_sum_proj := function( summands, i, sum )
    local map_for_vertex;
    map_for_vertex := function( v )
      return ProjectionInFactorOfDirectSumWithGivenDirectSum
             ( List( summands, R -> VectorSpaceOfRepresentation( R, v ) ),
               i,
               VectorSpaceOfRepresentation( sum, v ) );
    end;
    return QuiverRepresentationHomomorphismByMorphisms
           ( sum, summands[ i ],
             ListN( Vertices( Q ), map_for_vertex ) );
  end;
  AddProjectionInFactorOfDirectSumWithGivenDirectSum( cat, direct_sum_proj );
  
  Finalize( cat );

  return cat;
end );



InstallMethod( Transpose, "for dense list",
               [ IsDenseList ],
function( lists )
  local len;
  if IsEmpty( lists ) then
    return lists;
  fi;
  if not ForAll( lists, IsDenseList ) then
    Error( "all entries in list must be dense lists" );
  fi;
  len := Length( lists[ 1 ] );
  if not ForAll( lists, L -> Length( L ) = len ) then
    Error( "all lists must have same length" );
  fi;
  return List( [ 1 .. len ],
               i -> List( lists, L -> L[ i ] ) );
end );
