BindGlobal( "FamilyOfQuiverRepresentationElements",
            NewFamily( "quiver representation elements" ) );
BindGlobal( "FamilyOfQuiverRepresentations",
            CollectionsFamily( FamilyOfQuiverRepresentationElements ) );

DeclareRepresentation( "IsQuiverRepresentationElementRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [ "representation", "vectors" ] );

InstallMethod( QuiverRepresentationElement, "for quiver representation and collection",
               [ IsQuiverRepresentation, IsList ],
function( R, vectors )
  local field, Q, numVertices, i, v, space;
  vectors := ShallowCopy( vectors );
  field := FieldOfRepresentation( R );
  Q := QuiverOfRepresentation( R );
  numVertices := Length( Vertices( Q ) );
  if Length( vectors ) > numVertices then
    Error( "Too many vectors ",
           "(", Length( vectors ), " vectors, but only ", numVertices, ")" );
  fi;
  for i in [ 1 .. numVertices ] do
    space := VectorSpaceOfRepresentation( R, i );
    if not IsBound( vectors[ i ] ) then
      vectors[ i ] := Zero( space );
    elif not ( vectors[ i ] in space ) then
      vectors[ i ] := Vector( space, vectors[ i ] );
    fi;
  od;
  return QuiverRepresentationElementNC( R, vectors );
end );

InstallMethod( QuiverRepresentationElementNC, "for quiver representation and collection",
               [ IsQuiverRepresentation, IsDenseList ],
function( R, vectors )
  local type, elem, V, list, vector;
  type := NewType( FamilyOfQuiverRepresentationElements,
                   IsQuiverRepresentationElement and IsQuiverRepresentationElementRep );
  elem := rec();
  V := AsQPAVectorSpace( R );
  list := Concatenation( List( vectors, AsList ) );
  vector := Vector( V, list );
  ObjectifyWithAttributes( elem, type,
                           RepresentationOfElement, R,
                           ElementVectors, vectors,
                           AsList, list,
                           AsVector, vector );
  return elem;
end );

InstallMethod( QuiverRepresentationElement, "for quiver representation and dense lists",
               [ IsQuiverRepresentation, IsDenseList, IsList ],
function( R, vertices, vectors )
  local num_vectors, i, rep_vectors, vertex_number;

  num_vectors := Length( vectors );
  if Length( vertices ) < num_vectors then
    Error( "Too many vectors (", num_vectors, " vectors for ", Length( vertices ), " vertices)" );
  fi;
  for i in [ 1 .. num_vectors ] do
    if not IsQuiverVertex( vertices[ i ] ) then
      Error( "Not a vertex: ", vertices[ i ] );
    fi;
    if not vertices[ i ] in QuiverOfRepresentation( R ) then
      Error( "Vertex ", vertices[ i ], " is not in the quiver of the representation ", R );
    fi;
  od;
      
  rep_vectors := List( VectorSpacesOfRepresentation( R ), Zero );
  for i in [ 1 .. num_vectors ] do
    vertex_number := VertexIndex( vertices[ i ] );
    if IsBound( vectors[ i ] ) then
      rep_vectors[ vertex_number ] := vectors[ i ];
    fi;
  od;

  return QuiverRepresentationElement( R, rep_vectors );
end );

InstallMethod( QuiverRepresentationElement, "for quiver representation and vector",
               [ IsQuiverRepresentation, IsQPAVector ],
function( R, v )
  local v_list, dim, dim_sums, i, vectors;
  v_list := AsList( v );
  dim := DimensionVector( R );
  dim_sums := [ 0 ];
  for i in [ 2 .. Length( dim ) + 1 ] do
    dim_sums[ i ] := dim_sums[ i - 1 ] + dim[ i - 1 ];
  od;
  vectors := [];
  for i in [ 1 .. Length( dim ) ] do
    if dim[ i ] > 0 then
      vectors[ i ] := v_list{ [ dim_sums[ i ] + 1 .. dim_sums[ i + 1 ] ] };
    fi;
  od;
  return QuiverRepresentationElement( R, vectors );
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
               [ IsQuiverRepresentationElement, IsQuiverVertex ],
function( e, v )
  return ElementVectors( e )[ VertexIndex( v ) ];
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
  return QuiverRepresentationElement( R, [ Target( p ) ], [ target_vec ] );
end );

InstallMethod( QuiverAlgebraAction,
               "for element of quiver representation and element of quiver algebra",
               [ IsQuiverRepresentationElement, IsQuiverAlgebraElement ],
function( re, ae )
  local Cs, Ps;
  if IsZero( ae ) then
    return Zero( re );
  fi;
  Cs := Coefficients( ae );
  Ps := Paths( ae );
  return Sum( ListN( Cs, List( Ps, p -> PathAction( re, p ) ),
                     \* ) );
end );

InstallMethod( QuiverAlgebraActionAsLinearTransformation,
               "for quiver representation and element of quiver algebra",
               [ IsQuiverRepresentation, IsQuiverAlgebraElement ],
function( R, a )
  local   F,  V,  B,  m;
  F := FieldOfRepresentation( R );
  V := AsQPAVectorSpace( R );
  B := Basis( V );
  m := List( B, b -> AsList( AsVector( QuiverAlgebraAction( QuiverRepresentationElement( R, b ),
                                                            a ) ) ) );
  return LinearTransformation( V, V, m );
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

InstallMethod( \*, [ IsMultiplicativeElement, IsQuiverRepresentationHomomorphism ],
  function( c, m )
  local Q;
  Q := QuiverOfRepresentation( Source( m ) );
  if c in UnderlyingField( m ) then
    return QuiverRepresentationHomomorphismNC
           ( Source( m ), Range( m ), c*List( Vertices( Q ), v -> MapForVertex( m, v ) ) );
  else
    TryNextMethod();
  fi;
end );

# TODO module structure


DeclareRepresentation( "IsQuiverRepresentationRep", IsComponentObjectRep and IsAttributeStoringRep,
                       [ "algebra", "dimensions", "matrices" ] );

InstallMethod( QuiverRepresentation, "for quiver algebra, dense list and list",
               [ IsQuiverAlgebra, IsDenseList, IsList ],
function( A, dimensions, matrices )
  local cat, Q, objects, m, morphisms, i, a, source, range;

  cat := CategoryOfQuiverRepresentations( A );
  Q := QuiverOfAlgebra( A );

  objects := List( dimensions, VectorSpaceConstructor( cat ) );

  m := LinearTransformationConstructor( cat );
  morphisms := [];
  for i in [ 1 .. Length( matrices ) ] do
    if IsBound( matrices[ i ] ) then
      a := Arrow( Q, i );
      source := objects[ VertexIndex( Source( a ) ) ];
      range := objects[ VertexIndex( Target( a ) ) ];
      morphisms[ i ] := m( source, range, matrices[ i ] );
    fi;
  od;

  return QuiverRepresentation( cat, objects, morphisms );
end );

InstallMethod( QuiverRepresentation, "for quiver algebra, dense list, dense list and list",
               [ IsQuiverAlgebra, IsDenseList, IsDenseList, IsList ],
function( A, dimensions, arrows, matrices_for_arrows )
  local matrices, i;
  if Length( matrices_for_arrows ) > Length( arrows ) then
    Error( "too many matrices" );
  fi;
  matrices := [];
  for i in [ 1 .. Length( arrows ) ] do
    if IsBound( matrices_for_arrows[ i ] ) then
      matrices[ ArrowIndex( arrows[ i ] ) ] := matrices_for_arrows[ i ];
    fi;
  od;
  return QuiverRepresentation( A, dimensions, matrices );
end );

InstallMethod( QuiverRepresentation,
               [ IsQuiverRepresentationCategory, IsDenseList, IsList ],
function( cat, objects, morphisms )
  local A, R, Q, vertices, num_vertices, arrows, num_arrows, 
        vecspace_cat, i, src, correct_src, rng, correct_rng;

  A := AlgebraOfCategory( cat );
  vecspace_cat := VectorSpaceCategory( cat );
  if IsQuotientOfPathAlgebra( A ) then
    R := QuiverRepresentation
         ( CategoryOfQuiverRepresentationsOverVectorSpaceCategory( PathAlgebra( A ), vecspace_cat ),
           objects, morphisms );
    return AsRepresentationOfQuotientAlgebra( R, A );
  fi;

  Q := QuiverOfAlgebra( A );
  vertices := Vertices( Q );
  num_vertices := Length( vertices );
  arrows := Arrows( Q );
  num_arrows := Length( arrows );

  if Length( objects ) <> num_vertices then
    Error( "Wrong number of objects ",
           "(", Length( objects ), " given, expected ", num_vertices, ")" );
  fi;
  for i in [ 1 .. num_vertices ] do
    if not ( IsCapCategoryObject( objects[ i ] ) and
             IsIdenticalObj( CapCategory( objects[ i ] ), vecspace_cat ) ) then
      Error( "Object ", objects[ i ], " for vertex ", vertices[ i ],
             " is not a CAP object in the correct category" );
    fi;
  od;
  if Length( morphisms ) > num_arrows then
    Error( "Too many morphisms in quiver representation constructor ",
           "(", Length( morphisms ), " specified, but quiver has only ",
           num_arrows, " arrows)" );
  fi;
  for i in [ 1 .. num_arrows ] do
    if not IsBound( morphisms[ i ] ) then
      continue;
    fi;
    if not ( IsCapCategoryMorphism( morphisms[ i ] ) and
             IsIdenticalObj( CapCategory( morphisms[ i ] ), vecspace_cat ) ) then
      Error( "Morphism ", morphisms[ i ], " for arrow ", arrows[ i ],
             " is not a CAP object in the correct category" );
    fi;
    src := Source( morphisms[ i ] );
    correct_src := objects[ VertexIndex( Source( arrows[ i ] ) ) ];
    rng := Range( morphisms[ i ] );
    correct_rng := objects[ VertexIndex( Target( arrows[ i ] ) ) ];
    if not IsEqualForObjects( src, correct_src ) then
      Error( "Morphism ", morphisms[ i ], " for arrow ", arrows[ i ],
             " has wrong source (is ", src, ", should be ", correct_src, ")" );
    fi;
    if not IsEqualForObjects( rng, correct_rng ) then
      Error( "Morphism ", morphisms[ i ], " for arrow ", arrows[ i ],
             " has wrong source (is ", rng, ", should be ", correct_rng, ")" );
    fi;
  od;

  return QuiverRepresentationNC( cat, objects, morphisms );
end );

InstallMethod( QuiverRepresentation,
               [ IsQuiverRepresentationCategory, IsDenseList, IsDenseList, IsList ],
function( cat, objects, arrows, morphisms_for_arrows )
  local morphisms, i;
  morphisms := [];
  if Length( morphisms_for_arrows ) > Length( arrows ) then
    Error( "too many morphisms" );
  fi;
  for i in [ 1 .. Length( arrows ) ] do
    if IsBound( morphisms_for_arrows[ i ] ) then
      morphisms[ ArrowIndex( arrows[ i ] ) ] := morphisms_for_arrows[ i ];
    fi;
  od;
  return QuiverRepresentation
         ( cat, objects, morphisms );
end );

InstallMethod( QuiverRepresentationNC,
               [ IsQuiverRepresentationCategory, IsDenseList, IsList ],
function( cat, objects, morphisms )
  local A, Q, i, a, source, target, repType, R;

  A := AlgebraOfCategory( cat );
  Q := QuiverOfAlgebra( A );

  # fill in zero morphisms where no morphism is provided:
  morphisms := ShallowCopy( morphisms );
  for i in [ 1 .. NumberOfArrows( Q ) ] do
    if not IsBound( morphisms[ i ] ) then
      a := Arrow( Q, i );
      source := objects[ VertexIndex( Source( a ) ) ];
      target := objects[ VertexIndex( Target( a ) ) ];
      morphisms[ i ] := ZeroMorphism( source, target );
    fi;
  od;

  repType := NewType( FamilyOfQuiverRepresentations,
                      IsQuiverRepresentation and IsQuiverRepresentationRep );
  R := rec();
  ObjectifyWithAttributes
    ( R, repType,
      AlgebraOfRepresentation, A,
      VectorSpacesOfRepresentation, objects,
      MapsOfRepresentation, morphisms
      );
  Add( cat, R );
  return R;
end );

InstallMethod( MatricesOfRepresentation,
               [ IsQuiverRepresentation ],
               R -> List( MapsOfRepresentation( R ),
                          MatrixOfLinearTransformation ^ Direction( AlgebraOfRepresentation( R ) ) ) );

InstallMethod( DimensionVector,
               [ IsQuiverRepresentation ],
               R -> List( VectorSpacesOfRepresentation( R ), Dimension ) );

InstallMethod( Dimension, "for quiver representation",
               [ IsQuiverRepresentation ],
               R -> Sum( DimensionVector( R ) ) );

InstallTrueMethod( IsFiniteDimensional, IsQuiverRepresentation );

InstallMethod( AsQPAVectorSpace,
               [ IsQuiverRepresentation ],
               R -> VectorSpaceConstructor( CapCategory( R ) )( Sum( DimensionVector( R ) ) ) );

InstallMethod( UnderlyingCategoryForRepresentations, [ IsQuiverAlgebra ],
function( A )
  return CategoryOfVectorSpaces( LeftActingDomain( A ) );
end );

InstallMethod( VectorSpaceConstructor, [ IsQuiverRepresentationCategory ],
function( cat )
  local A, F;
  A := AlgebraOfCategory( cat );
  F := LeftActingDomain( A );
  return dim -> StandardVectorSpace( F, dim );
end );

InstallMethod( LinearTransformationConstructor, [ IsQuiverRepresentationCategory ],
function( cat )
  local A;
  A := AlgebraOfCategory( cat );
  return LinearTransformation ^ Direction( A );
end );

# InstallMethod( LinearTransformationConstructor, [ IsQuiverRepresentationCategory ],
# function( cat )
#   A := AlgebraOfCategory( cat );
#   if IsLeftQuiver( QuiverOfAlgebra( A ) ) then
#     return function( matrix )
#       dims := DimensionsMat( matrix );
      
#     end;

#     return dim -> ColVectorSpace( F, dim );
#   else
#     return dim -> RowVectorSpace( F, dim );
#   fi;
# end );

InstallMethod( AsRepresentationOfQuotientAlgebra,
               "for quiver representation and quotient of path algebra",
               [ IsQuiverRepresentation, IsQuotientOfPathAlgebra ],
function( R, A )
  local kQ, rels, rel, R_, cat;
  kQ := AlgebraOfRepresentation( R );
  if kQ <> PathAlgebra( A ) then
    Error( "The algebra ", A, " is not a quotient of the algebra ", kQ );
  fi;
  rels := RelationsOfAlgebra( A );
  for rel in rels do
    if not IsZero( MapForAlgebraElement( R, rel ) ) then
      Error( "Not a well-defined representation of the algebra ", A,
             "; does not respect the relation ", rel );
    fi;
  od;
  cat := CategoryOfQuiverRepresentationsOverVectorSpaceCategory
         ( A, VectorSpaceCategory( CapCategory( R ) ) );
  R_ := QuiverRepresentationNC
        ( cat,
          VectorSpacesOfRepresentation( R ),
          MapsOfRepresentation( R ) );
  SetAsRepresentationOfPathAlgebra( R_, R );
  return R_;
end );

InstallMethod( AsRepresentationOfPathAlgebra, "for quiver representation",
               [ IsQuiverRepresentation ],
function( R )
  local A, cat;
  A := AlgebraOfRepresentation( R );
  if IsPathAlgebra( A ) then
    return R;
  else
    cat := CategoryOfQuiverRepresentationsOverVectorSpaceCategory
           ( PathAlgebra( A ), VectorSpaceCategory( CapCategory( R ) ) );
    return QuiverRepresentationNC
           ( cat,
             VectorSpacesOfRepresentation( R ),
             MapsOfRepresentation( R ) );
  fi;
end );


InstallMethod( ZeroRepresentation, "for quiver algebra",
               [ IsQuiverAlgebra ],
function( A )
  local numVertices;
  numVertices := NumberOfVertices( QuiverOfAlgebra( A ) );
  return QuiverRepresentation( A, List( [ 1 .. numVertices ],
                                        i -> 0 ), [], [] );
end );

InstallMethod( \=, [ IsQuiverRepresentation, IsQuiverRepresentation ],
function( R1, R2 )
  return IsIdenticalObj( CapCategory( R1 ), CapCategory( R2 ) )
         and IsEqualForObjects( R1, R2 );
end );

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
  if IsVectorSpaceCategory( VectorSpaceCategory( CapCategory( R ) ) ) then
    return JoinStringsWithSeparator( DimensionVector( R ),
                                     "," );
  else
    return "representation";
  fi;
end );

InstallMethod( ViewObj, "for quiver representation",
               [ IsQuiverRepresentation ],
function( R )
  Print( "<", String( R ), ">" );
end );

InstallMethod( Zero, "for quiver representation",
               [ IsQuiverRepresentation ],
function( R )
  return QuiverRepresentationElement( R, [] );
end );

InstallMethod( Zero, "for quiver representation element",
               [ IsQuiverRepresentationElement ],
function( e )
  return Zero( RepresentationOfElement( e ) );
end );

InstallMethod( VectorSpaceOfRepresentation, "for quiver representation and positive integer",
               [ IsQuiverRepresentation, IsPosInt ],
function( R, i )
  return VectorSpacesOfRepresentation( R )[ i ];
end );

InstallMethod( VectorSpaceOfRepresentation, "for quiver representation and vertex",
               [ IsQuiverRepresentation, IsQuiverVertex ],
function( R, v )
  return VectorSpacesOfRepresentation( R )[ VertexIndex( v ) ];
end );

InstallMethod( VertexDimension, "for quiver representation and positive integer",
               [ IsQuiverRepresentation, IsPosInt ],
function( R, i )
  return DimensionVector( R )[ i ];
end );

InstallMethod( VertexDimension, "for quiver representation and vertex",
               [ IsQuiverRepresentation, IsQuiverVertex ],
function( R, v )
  return VertexDimension( R, VertexIndex( v ) );
end );

InstallMethod( MapForArrow, "for quiver representation and positive integer",
               [ IsQuiverRepresentation, IsPosInt ],
function( R, i )
  return MapsOfRepresentation( R )[ i ];
end );

InstallMethod( MapForArrow, "for quiver representation and arrow",
               [ IsQuiverRepresentation, IsArrow ],
function( R, a )
  return MapForArrow( R, ArrowIndex( a ) );
end );

InstallMethod( MapForPath, "for quiver representation and vertex",
               [ IsQuiverRepresentation, IsQuiverVertex ],
function( R, v )
  return IdentityMorphism( VectorSpaceOfRepresentation( R, v ) );
end );

InstallMethod( MapForPath, "for quiver representation and arrow",
               [ IsQuiverRepresentation, IsArrow ],
               MapForArrow );

InstallMethod( MapForPath, "for quiver representation and composite path",
               [ IsQuiverRepresentation, IsCompositePath ],
function( R, p )
  return PreCompose( List( ArrowList( p ), a -> MapForArrow( R, a ) ) );
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
  local Q, vertices, spaces, basis_elements_by_vertex, i, vertex_basis,
        basis_vector, temp, basis;
  Q := QuiverOfRepresentation( R );
  vertices := Vertices( Q );
  spaces := VectorSpacesOfRepresentation( R );
  basis_elements_by_vertex := List( vertices, v -> [] );
  for i in [ 1 .. Length( vertices ) ] do
    vertex_basis := CanonicalBasis( spaces[ i ] );
    for basis_vector in vertex_basis do
      temp := QuiverRepresentationElement( R, [ vertices[ i ] ], [ basis_vector ] );
      SetSupportOfElement( temp, [ vertices[ i ] ] );
      Add( basis_elements_by_vertex[ i ], temp );
    od;
  od;
  basis := rec();
  ObjectifyWithAttributes
    ( basis,
      NewType( FamilyOfQuiverRepresentationBases,
               IsQuiverRepresentationBasis and IsQuiverRepresentationBasisRep ),
      BasisVectors, Flat( basis_elements_by_vertex ),
      BasisVectorsByVertex, basis_elements_by_vertex,
      UnderlyingLeftModule, R,
      IsCanonicalBasis, true );
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
               "for quiver representations and list",
               [ IsQuiverRepresentation, IsQuiverRepresentation,
                 IsList ],
function( R1, R2, maps )
  local cat, ucat, Q, morphisms, i, V1, V2, morphism, m, a, comp1, 
        comp2;

  cat := CapCategory( R1 );
  if not IsIdenticalObj( cat, CapCategory( R2 ) ) then
    Error( "representations in different categories" );
  fi;
  ucat := VectorSpaceCategory( cat );

  Q := QuiverOfRepresentation( R1 );
  if Length( maps ) > NumberOfVertices( Q ) then
    Error( "too many maps for representation homomorphism: ", Length( maps ), " maps, ",
           "but only ", NumberOfVertices( Q ), " vertices in quiver" );
  fi;

  morphisms := [];
  for i in [ 1 .. NumberOfVertices( Q ) ] do
    V1 := VectorSpaceOfRepresentation( R1, i );
    V2 := VectorSpaceOfRepresentation( R2, i );
    if not IsBound( maps[ i ] ) then
      morphism := ZeroMorphism( V1, V2 );
    else
      m := maps[ i ];
      if IsCapCategoryMorphism( m ) then
        if not IsIdenticalObj( CapCategory( m ), ucat ) then
          Error( "morphism for vertex ", Vertex( Q, i ), " is from wrong category" );
        elif Source( m ) <> V1 then
          Error( "morphism for vertex ", Vertex( Q, i ), " has wrong source",
                 " (is ", Source( m ), ", should be ", V1, ")" );
        elif Range( m ) <> V2 then
          Error( "morphism for vertex ", Vertex( Q, i ), " has wrong range",
                 " (is ", Range( m ), ", should be ", V2, ")" );
        fi;
        morphism := m;
      else
        morphism := LinearTransformationConstructor( cat )( V1, V2, m );
      fi;
    fi;
    Add( morphisms, morphism );
  od;

  for a in Arrows( Q ) do
    comp1 := PreCompose( MapForArrow( R1, a ),
                         morphisms[ VertexIndex( Target( a ) ) ] );
    comp2 := PreCompose( morphisms[ VertexIndex( Source( a ) ) ],
                         MapForArrow( R2, a ) );
    if not IsEqualForMorphisms( comp1, comp2 ) then
      Error( "maps for representation homomorphism do not commute with maps for arrow ", a );
    fi;
  od;

  return QuiverRepresentationHomomorphismNC( R1, R2, morphisms );
end );

InstallMethod( QuiverRepresentationHomomorphism,
               "for quiver representations and function",
               [ IsQuiverRepresentation, IsQuiverRepresentation, IsFunction ],
function( R1, R2, f )
  local B, fB;
  B := BasisVectors( Basis( R1 ) );
  fB := List( B, f );
  return QuiverRepresentationHomomorphismByImages( R1, R2, B, fB );
end );

InstallMethod( QuiverRepresentationHomomorphismNC,
               "for quiver representations and dense list",
               [ IsQuiverRepresentation, IsQuiverRepresentation,
                 IsDenseList ],
function( R1, R2, morphisms )
  local f;
  f := rec();
  ObjectifyWithAttributes
    ( f, NewType( FamilyOfQuiverRepresentationHomomorphisms,
                  IsQuiverRepresentationHomomorphism and
                  IsQuiverRepresentationHomomorphismRep ),
      Source, R1,
      Range, R2,
      MapsOfRepresentationHomomorphism, morphisms );
  Add( CapCategory( R1 ), f );
  return f;
end );

InstallMethod( MatricesOfRepresentationHomomorphism,
               "for quiver representation",
               [ IsQuiverRepresentationHomomorphism ],
function( m )
  return List( MapsOfRepresentationHomomorphism( m ),
               MatrixOfLinearTransformation ^ Direction( AlgebraOfRepresentation( Source( m ) ) ) );
end );

InstallMethod( SpaceContainingVector,
               "for quiver representation homomorphism",
               [ IsQuiverRepresentationHomomorphism ],
               f -> Hom( Source( f ), Range( f ) ) );

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
function( m1, m2 )
  return IsIdenticalObj( CapCategory( m1 ), CapCategory( m2 ) )
         and Source( m1 ) = Source( m2 )
         and Range( m1 ) = Range( m2 )
         and IsEqualForMorphisms( m1, m2 );
end );

InstallOtherMethod( IsZero, [ IsQuiverRepresentationHomomorphism ],
               IsZeroForMorphisms );

InstallMethod( ImageElm,
               [ IsQuiverRepresentationHomomorphism,
                 IsQuiverRepresentationElement ],
function( f, e )
  return QuiverRepresentationElement
         ( Range( f ),
           ListN( MapsOfRepresentationHomomorphism( f ),
                  ElementVectors( e ),
                  ImageElm ) );
end );

InstallOtherMethod( \+,
               [ IsQuiverRepresentationHomomorphism, IsQuiverRepresentationHomomorphism ],
function( f, g )
  if Source( f ) <> Source( g ) then
    Error( "adding homomorphisms with different sources" );
  elif Range( f ) <> Range( g ) then
    Error( "adding homomorphisms with different ranges" );
  fi;
  return QuiverRepresentationHomomorphism
         ( Source( f ), Range( f ),
           ListN( MapsOfRepresentationHomomorphism( f ),
                  MapsOfRepresentationHomomorphism( g ),
                  \+ ) );
end );

InstallMethod( MapForVertex, "for quiver representation homomorphism and vertex",
               [ IsQuiverRepresentationHomomorphism, IsQuiverVertex ],
function( f, v )
  return MapsOfRepresentationHomomorphism( f )[ VertexIndex( v ) ];
end );

InstallMethod( MapForVertex, "for quiver representation homomorphism and positive integer",
               [ IsQuiverRepresentationHomomorphism, IsPosInt ],
function( f, i )
  return MapsOfRepresentationHomomorphism( f )[ i ];
end );

InstallMethod( AsLinearTransformation, "for quiver representation homomorphism",
               [ IsQuiverRepresentationHomomorphism ],
function( f )
  return DirectSumFunctorial( MapsOfRepresentationHomomorphism( f ) );
end );

InstallMethod( MorphismByLinearTransformation, "for quiver representations and linear transformation",
               [ IsQuiverRepresentation, IsQuiverRepresentation, IsLinearTransformation ],
function( R1, R2, T )
  local Q, n, Vs1, Vs2, maps;
  if not IsIdenticalObj( CapCategory( R1 ),
                         CapCategory( R2 ) ) then
    Error( "representations from different categories" );
  fi;
  Q := QuiverOfRepresentation( R1 );
  n := NumberOfVertices( Q );
  Vs1 := VectorSpacesOfRepresentation( R1 );
  Vs2 := VectorSpacesOfRepresentation( R2 );
  maps := List( [ 1 .. n ],
                i -> PreCompose( InjectionOfCofactorOfDirectSum( Vs1, i ),
                                 T,
                                 ProjectionInFactorOfDirectSum( Vs2, i ) ) );
  return QuiverRepresentationHomomorphism( R1, R2, maps );
end );

InstallMethod( CategoryOfQuiverRepresentations, "for quiver algebra",
               [ IsQuiverAlgebra ],
function( A )
  local underlying_category_for_representations;
  underlying_category_for_representations := UnderlyingCategoryForRepresentations( A : FinalizeCategory := true );
  return CategoryOfQuiverRepresentationsOverVectorSpaceCategory
         ( A, underlying_category_for_representations );
end );

InstallMethod( \=, [ IsQuiverRepresentationCategory, IsQuiverRepresentationCategory ],
function( cat1, cat2 )
  return AlgebraOfCategory( cat1 ) = AlgebraOfCategory( cat2 )
         and VectorSpaceCategory( cat1 ) = VectorSpaceCategory( cat2 );
end );

InstallMethodWithCache( CategoryOfQuiverRepresentationsOverVectorSpaceCategory,
               "for quiver algebra and vector space category",
               [ IsQuiverAlgebra, IsAbelianCategory ],
function( A, vecspace_cat )
  local Q, cat, equal_objects, equal_morphisms, zero_object, 
        zero_morphism, identity_morphism, pre_compose, addition, 
        additive_inverse, kernel_emb, coker, coker_proj, mono_lift, 
        epi_colift, proj_lift, direct_sum, direct_sum_inj, 
        direct_sum_proj, to_be_finalized;

  Q := QuiverOfAlgebra( A );

  cat := CreateCapCategory( Concatenation( "quiver representations over ", String( A ) ) );
  cat!.category_as_first_argument := true;
  SetFilterObj( cat, IsQuiverRepresentationCategory );
  SetAlgebraOfCategory( cat, A );
  SetVectorSpaceCategory( cat, vecspace_cat );

  SetIsAbelianCategory( cat, true );
  SetIsAbelianCategoryWithEnoughProjectives( cat, true );
  SetIsAbelianCategoryWithEnoughInjectives( cat, true );

  equal_objects := function( category, R1, R2 )
    return AlgebraOfRepresentation( R1 ) = AlgebraOfRepresentation( R2 ) and
           DimensionVector( R1 ) = DimensionVector( R2 ) and
           ForAll( ListN( VectorSpacesOfRepresentation( R1 ),
                          VectorSpacesOfRepresentation( R2 ),
                          IsEqualForObjects ),
                   IdFunc ) and
           ForAll( ListN( MapsOfRepresentation( R1 ),
                          MapsOfRepresentation( R2 ),
                          IsEqualForMorphisms ),
                   IdFunc );
  end;
  AddIsEqualForObjects( cat, equal_objects );

  equal_morphisms := function( category, m1, m2 )
    return ForAll( ListN( MapsOfRepresentationHomomorphism( m1 ),
                          MapsOfRepresentationHomomorphism( m2 ),
                          IsEqualForMorphisms),
                   IdFunc );
  end;
  AddIsEqualForMorphisms( cat, equal_morphisms );

  zero_object := function( category )
    local zero;
    zero := ZeroObject( vecspace_cat );
    return QuiverRepresentation( cat, List( Vertices( Q ), v -> zero ), [] );
  end;
  AddZeroObject( cat, zero_object );

  zero_morphism := function( category, R1, R2 )
    return QuiverRepresentationHomomorphism
           ( R1, R2,
             ListN( VectorSpacesOfRepresentation( R1 ),
                    VectorSpacesOfRepresentation( R2 ),
                    ZeroMorphism ) );
  end;
  AddZeroMorphism( cat, zero_morphism );

  # TODO: need IsZeroForMorphisms?

  identity_morphism := function( category, R )
    return QuiverRepresentationHomomorphism
           ( R, R,
             List( VectorSpacesOfRepresentation( R ),
                   IdentityMorphism ) );
  end;
  AddIdentityMorphism( cat, identity_morphism );

  pre_compose := function( category, m1, m2 )
    return QuiverRepresentationHomomorphism
           ( Source( m1 ), Range( m2 ),
             ListN( MapsOfRepresentationHomomorphism( m1 ),
                    MapsOfRepresentationHomomorphism( m2 ),
                    PreCompose ) );
  end;
  AddPreCompose( cat, pre_compose );

  addition := function( category, m1, m2 )
    return QuiverRepresentationHomomorphism
           ( Source( m1 ), Range( m1 ),
             ListN( MapsOfRepresentationHomomorphism( m1 ),
                    MapsOfRepresentationHomomorphism( m2 ),
                    AdditionForMorphisms ) );
  end;
  AddAdditionForMorphisms( cat, addition );

  additive_inverse := function( category, m )
    return QuiverRepresentationHomomorphism
           ( Source( m ), Range( m ),
             List( MapsOfRepresentationHomomorphism( m ),
                   AdditiveInverse ) );
  end;
  AddAdditiveInverseForMorphisms( cat, additive_inverse );

  kernel_emb := function( category, m )
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
    ker := QuiverRepresentation
           ( cat, ker_objs, List( Arrows( Q ), map_for_arrow ) );
    return QuiverRepresentationHomomorphism
           ( ker, Source( m ), emb_maps );
  end;
  AddKernelEmbedding( cat, kernel_emb );

  coker := function( category, m )
    local map_for_arrow;
    map_for_arrow := function( a )
      return CokernelObjectFunctorial
             ( MapForVertex( m, Source( a ) ),
               MapForArrow( Range( m ), a ),
               MapForVertex( m, Target( a ) ) );
    end;
    return QuiverRepresentation
           ( cat,
             List( MapsOfRepresentationHomomorphism( m ),
                   CokernelObject ),
             List( Arrows( Q ),
                   map_for_arrow ) );
  end;
  coker_proj := function( category, m )
    return QuiverRepresentationHomomorphism
           ( Range( m ), coker( CapCategory( m ), m ),
             List( MapsOfRepresentationHomomorphism( m ),
                   CokernelProjection ) );
  end;
  AddCokernelProjection( cat, coker_proj );

  mono_lift := function( category, i, test )
    return QuiverRepresentationHomomorphism
           ( Source( test ), Source( i ),
             ListN( MapsOfRepresentationHomomorphism( i ),
                    MapsOfRepresentationHomomorphism( test ),
                    LiftAlongMonomorphism ) );
  end;
  AddLiftAlongMonomorphism( cat, mono_lift );

  epi_colift := function( category, e, test )
    return QuiverRepresentationHomomorphism
           ( Range( e ), Range( test ),
             ListN( MapsOfRepresentationHomomorphism( e ),
                    MapsOfRepresentationHomomorphism( test ),
                    ColiftAlongEpimorphism ) );
  end;
  AddColiftAlongEpimorphism( cat, epi_colift );

  proj_lift := function( category, pi, epsilon )
    local P, A, B, top_basis, images;
    P := Source( pi );
    A := Range( pi );
    B := Source( epsilon );
    top_basis := TopBasis( P );
    images := List( top_basis,
                    elm -> PreImagesRepresentative( epsilon, ImageElm( pi, elm ) ) );
    return QuiverRepresentationHomomorphismByImages( P, B, top_basis, images );
  end;
  AddProjectiveLift( cat, proj_lift );

  direct_sum := function( category, summands )
    return QuiverRepresentation
           ( cat,
             List( Transpose( List( summands,
                                    VectorSpacesOfRepresentation ) ),
                   DirectSum ),
             List( Transpose( List( summands,
                                    MapsOfRepresentation ) ),
                   DirectSumFunctorial ) );
  end;
  AddDirectSum( cat, direct_sum );

  direct_sum_inj := function( category, summands, i, sum )
    local map_for_vertex;
    map_for_vertex := function( v )
      return InjectionOfCofactorOfDirectSumWithGivenDirectSum
             ( List( summands, R -> VectorSpaceOfRepresentation( R, v ) ),
               i,
               VectorSpaceOfRepresentation( sum, v ) );
    end;
    return QuiverRepresentationHomomorphism
           ( summands[ i ], sum,
             ListN( Vertices( Q ), map_for_vertex ) );
  end;
  AddInjectionOfCofactorOfDirectSumWithGivenDirectSum( cat, direct_sum_inj );
  
  direct_sum_proj := function( category, summands, i, sum )
    local map_for_vertex;
    map_for_vertex := function( v )
      return ProjectionInFactorOfDirectSumWithGivenDirectSum
             ( List( summands, R -> VectorSpaceOfRepresentation( R, v ) ),
               i,
               VectorSpaceOfRepresentation( sum, v ) );
    end;
    return QuiverRepresentationHomomorphism
           ( sum, summands[ i ],
             ListN( Vertices( Q ), map_for_vertex ) );
  end;
  AddProjectionInFactorOfDirectSumWithGivenDirectSum( cat, direct_sum_proj );
  
  AddEpimorphismFromSomeProjectiveObject( cat, { category, m } -> ProjectiveCover( m ) );
  AddMonomorphismIntoSomeInjectiveObject( cat, { category, m } -> InjectiveEnvelope( m ) );
  
  ##
  AddIsWellDefinedForObjects( cat,
    function( category, R )
      local A, relations;
      
      A := AlgebraOfRepresentation( R );
      
      relations := RelationsOfAlgebra( A );
      
      return ForAll( relations, rel -> IsZero( MapForAlgebraElement( R, rel ) ) );
      
  end );
  
  ##
  AddIsWellDefinedForMorphisms( cat,
    function( category, alpha )
      local S, R, arrows;
      
      S := Source( alpha );
      R := Range( alpha );
      arrows := Arrows( QuiverOfRepresentation( S ) );
      
      return ForAll( arrows, arrow ->
                            IsEqualForMorphisms(
                              PreCompose( MapForArrow( S, arrow ), MapForVertex( alpha, Target( arrow ) ) ),
                                PreCompose( MapForVertex( alpha, Source( arrow ) ), MapForArrow( R, arrow ) )
                                               )
                   );
  
  end );

  to_be_finalized := ValueOption( "FinalizeCategory" );
  if to_be_finalized <> false then
    to_be_finalized := true;
  fi;
   
  if to_be_finalized then
    Finalize( cat );
  fi;
  
  return Intern( cat );
    
end );

InstallMethod( ChangeBaseCategory, "for quiver representation category and abelian category",
               [ IsQuiverRepresentationCategory, IsAbelianCategory ],
function( cat, ucat )
  local A;
  A := AlgebraOfCategory( cat );
  return CategoryOfQuiverRepresentationsOverVectorSpaceCategory( A, ucat );
end );

InstallMethod( UnderlyingField, "for quiver representation category",
               [ IsQuiverRepresentationCategory ],
               cat -> UnderlyingField( VectorSpaceCategory( cat ) ) );


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


InstallMethod( SubrepresentationInclusion, "for a represenation and a homogeneous list",
               [ IsQuiverRepresentation, IsHomogeneousList ],
function( R, gens )
  local   A,  Q,  vertices,  num_vert,  queue,  v,  spanningset,  
          temp,  g,  outgoingarrows,  a,  ga,  inclusions,  arrows,  
          maps,  s,  t,  U;

  if not ForAll( gens, g -> g in R ) then
    Error("entered elements are not in the representation <R>,\n");
  fi;
  if Length(gens) = 0 then 
    return ZeroMorphism( ZeroObject( CapCategory( R ) ), R );
  fi;
  A := AlgebraOfRepresentation( R );
  Q := QuiverOfAlgebra( A ); 
  vertices := Vertices( Q ); 
  num_vert := Length( vertices ); 
  queue := [ ] ; 

  # Making the generators uniform.
  for g in gens do
    for v in vertices do
      if not IsZero( PathAction( g, v ) ) then
        Add( queue, [ v , PathAction( g, v ) ] ); 
      fi;
    od;
  od;
  spanningset := List( [1 .. num_vert ], v -> [ ] );
  while not IsEmpty( queue ) do
    temp := Remove( queue, 1);
    v := temp[ 1 ];
    g := temp[ 2 ];
    Add( spanningset[ VertexIndex( v ) ], ElementVector( g, v ) );
    outgoingarrows := OutgoingArrows( v );
    for a in outgoingarrows do
      ga := PathAction( g, a );
      if  not IsZero( ga ) then
        Add( queue, [ Target( a ), ga ] );
      fi;
    od;
  od;

  inclusions := ListN( VectorSpacesOfRepresentation( R ), spanningset, SubspaceInclusion );
  arrows := Arrows( Q );
  maps := [ ]; 
  for a in arrows do
    s := VertexIndex( Source( a ) );
    t := VertexIndex( Target( a ) );
    Add( maps, LiftAlongMonomorphism( inclusions[ t ], PreCompose( inclusions[ s ], MapForArrow( R, a ) ) ) );
  od;
  U := QuiverRepresentation( CapCategory( R ), List( inclusions, Source ), maps );
  
  return QuiverRepresentationHomomorphism( U, R, inclusions ); 
end );

InstallMethod( SupportOfElement, "for a represenation element",
               [ IsQuiverRepresentationElement ],
function( r )
  local   Q,  temp,  vertices;

  Q := QuiverOfRepresentation( RepresentationOfElement( r ) );
  temp := PositionsProperty( ElementVectors( r ), v -> not IsZero( v ) );
  vertices := Vertices( Q );
  
  return vertices{temp};
end
);


InstallMethod( RadicalInclusion, "for a represenation",
               [ IsQuiverRepresentation ],
function( R )
  local   basis,  generators,  b,  a,  temp;

  basis := Basis( R ); 
  generators := [ ];
  for b in basis do
    for a in OutgoingArrows( SupportOfElement( b )[1] ) do
      temp := PathAction( b, a ); 
      if not IsZero( temp ) then
        Add( generators, temp );
      fi;
    od;
  od;
  generators := Unique(generators);

  return SubrepresentationInclusion( R, generators );
end
);


InstallMethod( RadicalOfMorphism, "for a represenation homomorphism",
               [ IsQuiverRepresentationHomomorphism ],
function( f )
  local   R1,  R2,  i1,  i2;

  R1 := Source(f);
  R2 := Range(f);
  i1 := RadicalInclusion( R1 );
  i2 := RadicalInclusion( R2 );
  
  return LiftAlongMonomorphism( i2, PreCompose( i1, f ) );
end
); 

InstallMethod( RadicalFunctor, "for a category of representations",
               [ IsQuiverRepresentationCategory ],
function( C )
  local   rad;

  rad := CapFunctor( "Radical", C, C );
  AddObjectFunction( rad, X -> Source( RadicalInclusion( X ) ) ); 
  AddMorphismFunction( rad, function ( X, f, Y ) return RadicalOfMorphism( f ); end );
  
  return rad;
end
);

InstallMethod( RadicalInclusionTransformation, "for a category of representations",
               [ IsQuiverRepresentationCategory ],
function( C )
  local   rad,  Id,  radembedding;

  rad := RadicalFunctor( C );
  Id := IdentityFunctor( C );
  radembedding := NaturalTransformation( rad, Id );
  AddNaturalTransformationFunction( radembedding, 
                                    function( X, Y, Z ) return RadicalInclusion( Y ); end ); 
  
  return radembedding;
end
);

InstallMethod( TopProjection, "for a represenation",
               [ IsQuiverRepresentation ],
function( R )

  return CokernelProjection( RadicalInclusion( R ) );
end
);

InstallMethod( TopOfMorphism, "for a represenation homomorphism",
               [ IsQuiverRepresentationHomomorphism ],
function( f )
  local   R1,  R2,  p1,  p2;

  R1 := Source(f);
  R2 := Range(f);
  p1 := TopProjection( R1 );
  p2 := TopProjection( R2 );
  
  return ColiftAlongEpimorphism( p1, PreCompose( f, p2 ) );
end
); 

InstallMethod( TopFunctor, "for a category of representations",
               [ IsQuiverRepresentationCategory ],
function( C )
  local   top;

  top := CapFunctor( "Top", C, C );
  AddObjectFunction( top, X -> Range( TopProjection( X ) ) ); 
  AddMorphismFunction( top, function ( X, f, Y ) return TopOfMorphism( f ); end );
  
  return top;
end
);

InstallMethod( TopProjectionTransformation, "for a category of representations",
               [ IsQuiverRepresentationCategory ],
function( C )
  local   top,  Id,  topprojection;

  top := TopFunctor( C );
  Id := IdentityFunctor( C );
  topprojection := NaturalTransformation( Id, top );
  AddNaturalTransformationFunction( topprojection, 
                                    function( X, Y, Z ) return TopProjection( Y ); end ); 
  
  return topprojection;
end
);

InstallMethod( PreImagesRepresentative, "for a representation homomorphism and a representation element",
               [ IsQuiverRepresentationHomomorphism, IsQuiverRepresentationElement ],
function( f, m )
  local   elementvectors,  maps,  preimage;
  
  if not m in Range( f ) then
    Error("the entered element is not in the range for the entered homomorphism,\n");
  fi;
  elementvectors := ElementVectors( m );
  maps := MapsOfRepresentationHomomorphism( f );
  preimage := ListN( maps, elementvectors, PreImagesRepresentative );
  if ForAll( preimage, x -> x <> fail ) then
    return QuiverRepresentationElement( Source( f ), preimage );
  else
    return fail;
  fi;
end
);

InstallMethod( HomFromProjective, "for a representation element and a quiver representation",
               [ IsQuiverRepresentationElement, IsQuiverRepresentation ],
function( r, R )
  local   supp,  A,  k,  n,  B,  mats,  i,  matrix,  b;
  
  supp := SupportOfElement( r );
  if Length( supp ) > 1 then
    Error("the entered quiver representation element is supported in more than one vertex,\n");
  fi;

  A := AlgebraOfRepresentation( R );
  k := FieldOfRepresentation( R );
  n := VertexIndex( supp[1] );
  B := BasisOfProjectives( A )[ n ];
  mats := [ ];
  for i in [ 1 .. Length( B ) ] do
    if Length( B[ i ] ) > 0 and DimensionVector( R )[ i ] > 0 then
      matrix := [ ];
      for b in B[ i ] do
        Add( matrix, AsList( ElementVector( QuiverAlgebraAction( r, b ), i ) ) );
      od;
      mats[i] := matrix;
    fi;
  od;
  
  return QuiverRepresentationHomomorphism( IndecProjRepresentations( A )[ n ], R, mats );
end
);

InstallMethod( MinimalGeneratingSet, "for a quiver representation",
               [ IsQuiverRepresentation ],
function( R )
  local   f;

  f := TopProjection( R );
  return List( BasisVectors( Basis( Range( f ) ) ), x -> PreImagesRepresentative( f, x ) );
end
);

InstallMethod( QuiverRepresentationHomomorphismByImages, 
        "for two quiver representations, a list of generators and a list of images",
        [ IsQuiverRepresentation, IsQuiverRepresentation, IsHomogeneousList, IsHomogeneousList ],
        
function( R1, R2, generators, images )
    local   A1,  Q1,  vertices,  num_vert,  spanofgens,  
            newspanofgens,  queue,  addvector,  g,  v,  temp,  p,  a,  
            hommatrices,  dim,  basis,  paths,  gens,  matrix,  
            basischangemat,  f;
    
    A1 := AlgebraOfRepresentation( R1 );  
    if A1 <> AlgebraOfRepresentation( R2 ) then
        Error( "The entered representations are not over the same algebra," );
    fi;
    if Length( generators ) <> Length( images ) then
        Error( "The number of generators and the number of images are different," );
    fi;
    if not ForAll( generators, g -> g in R1 ) then
        Error( "The entered generators are not in the first representation," );
    fi;
    if not ForAll( images, m -> m in R2 ) then
        Error( "The entered images are not in the second representation," );
    fi;
    Q1 := QuiverOfAlgebra( A1 ); 
    vertices := Vertices( Q1 ); 
    num_vert := Length( vertices ); 
    spanofgens := List( vertices, v -> [ [ ], [ ], [ ] ] );  # Basis, paths, generators 
    newspanofgens := List( vertices, v -> [ ] );
    queue := [ ];    
    addvector := function( p, g )
        local   target,  vec,  temp;
        
        target := Target( p );
        if Length( spanofgens[ VertexIndex( target ) ][ 1 ] ) = VertexDimension( R1, target ) then
            return;
        fi;
        vec := AsList( ElementVector( PathAction( g, p ), target ) ); 
        temp := Concatenation( spanofgens[ VertexIndex( target ) ][ 1 ], [ vec ] ); 
        if RankMat( temp ) = Length( temp ) then 
            Add( spanofgens[ VertexIndex( target ) ][ 1 ], vec );
            Add( spanofgens[ VertexIndex( target ) ][ 2 ], p );
            Add( spanofgens[ VertexIndex( target ) ][ 3 ], g );
            Add( queue, [ p , g ] ); 
        fi;        
    end;
    
    for g in generators do
        for v in vertices do
            if not IsZero( PathAction( g, v ) ) then
                addvector( v, g );
            fi;
        od;
    od;
    while not IsEmpty( queue ) do
        temp := Remove( queue, 1 );
        p := temp[ 1 ];
        g := temp[ 2 ];
        for a in OutgoingArrows( Target( p ) ) do
            addvector( ComposePaths( p, a ), g );
        od;
    od;
    if DimensionVector( R1 ) <> List( spanofgens, s -> Length( s[ 1 ] ) ) then
        Error( "The entered generators doesn't generate the source of the homomorphism,\n" ); 
    fi;
    hommatrices := [ ];
    for v in vertices do
        dim := VertexDimension( R1, v ); 
        if dim > 0 then
            temp := spanofgens[  VertexIndex( v ) ];
            basis := temp[ 1 ];
            paths := temp[ 2 ];
            gens := temp[ 3 ];
            matrix := List( [ 1..Length( gens ) ], i -> PathAction( images[ Position( generators, gens[ i ] ) ], paths[ i ] ) );
            matrix := List( matrix, m -> AsList( ElementVector( m, v ) ) );
            basischangemat := temp[ 1 ]^( -1 );            
            hommatrices[ VertexIndex( v ) ] := basischangemat * matrix; 
        fi;
    od;
    f := QuiverRepresentationHomomorphism( R1, R2, hommatrices );
    if ForAll( [ 1..Length( generators ) ], i -> ImageElm( f, generators[ i ] ) = images[ i ] ) then
        return f;
    else
        Error( "The generators and the images are inconsistent," );
    fi;
end
  );

##
InstallOtherMethod( InverseOp,
    [ IsQuiverRepresentationHomomorphism ],
function( m )
  local maps;
  if not IsIsomorphism( m ) then
    Error( "The quiver representation homomorphism is not isomorphism" );
  fi;
  maps := List( MapsOfRepresentationHomomorphism( m ), Inverse );
  return QuiverRepresentationHomomorphism( Range( m ), Source( m ), maps );
end );

##
InstallMethod( Display,
               [ IsQuiverRepresentation ],
    function( R )
    local q, v_nr, a_nr, i, V;
    
    q := QuiverOfRepresentation( R );
    v_nr := NumberOfVertices( q );
    a_nr := NumberOfArrows( q );
    
    Print( "A representation over the algebra ", String( AlgebraOfRepresentation( R ) ), " given by the data:\n\n"  );
    for i in [ 1 .. v_nr ] do 
    Print( TextAttr.underscore, TextAttr.2, "For vertex (", String( Vertex( q, i ) ), "):", TextAttr.reset, "\n" );
    V := VectorSpaceOfRepresentation( R, i );
    Print( String( V ), " of dimension ", Dimension( V ) );
    Print("\n\n" );
    od;
    
    for i in [ 1 .. a_nr ] do 
    Print( TextAttr.underscore, TextAttr.2, "For arrow (", String( Arrow( q, i ) ), "):", TextAttr.reset, "\n" );
    Display( MapForArrow( R, i ) );
    Print("\n" );
    od;
    
end );

##
InstallMethod( Display,
               [ IsQuiverRepresentationHomomorphism ],
    function( m )
    local q, v_nr, map, i;
    
    q := QuiverOfRepresentation( Source( m ) );
    v_nr := NumberOfVertices( q );
    
    Print( "A representation homomorphism over the algebra ", String( AlgebraOfRepresentation( Source( m ) ) ), " given by the data:\n\n"  );
    for i in [ 1 .. v_nr ] do 
    Print( TextAttr.underscore, TextAttr.2, "For vertex (", String( Vertex( q, i ) ), "):", TextAttr.reset,"\n" );
    map := MapForVertex( m, i );
    Display( map );
    Print("\n\n" );
    od;
    
end );


InstallMethod( TensorProductProjectionFunctors, "for quiver representation category",
               [ IsQuiverRepresentationCategory ],
function( cat )
  local T, algebras, quiver_incs, projs, s, A;
  # cat: representations over A \tensor B
  # result: projs, list of two lists
  # projs[ 1 ]: projections in first factor
  #             list of one functor cat -> rep(A) for each vertex in B
  # projs[ 2 ]: projections in second factor
  #             list of one functor cat -> rep(B) for each vertex in A
  T := AlgebraOfCategory( cat );
  if not IsTensorProductOfAlgebras( T ) then
    Error( "representation category is not over tensor algebra" );
  fi;
  algebras := TensorProductFactors( T );
  if Length( algebras ) <> 2 then
    Error( "representation category over tensor product of ", Length( algebras ), " algebras; ",
           "should be 2" );
  fi;
  quiver_incs := ProductQuiverInclusions( QuiverOfAlgebra( T ) );
  projs := [];
  for s in [ 1, 2 ] do
    A := algebras[ s ];
    projs[ s ] := List( quiver_incs[ s ],
                        inc -> RestrictionFunctor( inc, cat,
                                                   CategoryOfQuiverRepresentations( A ) ) );
  od;
  return projs;
  #TODO test
end );


InstallMethod( TensorProductProjectionNaturalTransformations,
               [ IsQuiverRepresentationCategory ],
function( cat )
  local T, algebras, Q_T, quiver_incs, projs, nts, 
        make_natural_transformation, s, t, B;
  T := AlgebraOfCategory( cat );
  if not IsTensorProductOfAlgebras( T ) then
    Error( "representation category is not over tensor algebra" );
  fi;
  algebras := TensorProductFactors( T );
  if Length( algebras ) <> 2 then
    Error( "representation category over tensor product of ", Length( algebras ), " algebras; ",
           "should be 2" );
  fi;
  # cat: representations over A \tensor B
  # result: nts, list of two lists
  # nts[ 1 ]: natural transformations between projections in first factor
  #           list of one nat transf projs[ 1 ][ i ] -> projs[ 1 ][ j ]
  #                   for each arrow a : i -> j in B
  # nts[ 2 ]: natural transformations between projections in second factor
  #           list of one nat transf projs[ 2 ][ i ] -> projs[ 2 ][ j ]
  #                   for each arrow a : i -> j in A
  Q_T := QuiverOfAlgebra( T );
  quiver_incs := ProductQuiverInclusions( Q_T );
  projs := TensorProductProjectionFunctors( cat );
  nts := [];

  make_natural_transformation := function( s, a )
    local t, A, i, j, nt, ntfun;
    t := 3 - s;
    A := algebras[ s ];
    i := VertexIndex( Source( a ) );
    j := VertexIndex( Target( a ) );
    nt := NaturalTransformation( projs[ s ][ i ], projs[ s ][ j ] );
    ntfun := function( proj_i_R, R, proj_j_R )
      local maps;
      maps := List( Vertices( QuiverOfAlgebra( A ) ),
                    v -> MapForArrow( R, PathInProductQuiver( Q_T, [ s, t ], [ v, a ] ) ) );
      return QuiverRepresentationHomomorphism( proj_i_R, proj_j_R, maps );
    end;
    AddNaturalTransformationFunction( nt, ntfun );
    return nt;
  end;

  for s in [ 1, 2 ] do
    t := 3 - s;
    B := algebras[ t ];
    nts[ s ] := List( Arrows( QuiverOfAlgebra( B ) ),
                      a -> make_natural_transformation( s, a ) );
  od;
  return nts;

  # for s in [ 1, 2 ] do
  #   t := 3 - s;
  #   A := algebras[ s ];
  #   B := algebras[ t ];
  #   nts[ s ] := [];
  #   for a in Arrows( QuiverOfAlgebra( B ) ) do
  #     i := VertexNumber( Source( a ) );
  #     j := VertexNumber( Target( a ) );
  #     nt := NaturalTransformation( projs[ s ][ i ], projs[ s ][ j ] );
  #     ntfun := function( proj_i_R, R, proj_j_R )
  #       local maps;
  #       maps := List( Vertices( QuiverOfAlgebra( A ) ),
  #                     v -> MapForArrow( R, PathInProductQuiver( Q_T, [ s, t ], [ v, a ] ) ) );
  #       return QuiverRepresentationHomomorphism( proj_i_R, proj_j_R, maps );
  #     end;
  #     AddNaturalTransformationFunction( nt, ntfun );
  #     Add( nts[ s ], nt );
  #   od;
  # od;    
  # return nts;
  # TODO test
end );


InstallMethod( AsLayeredRepresentation, "for positive integer and quiver representation homomorphism",
               [ IsPosInt, IsQuiverRepresentation ],
function( s, R )
  return ApplyFunctor( AsLayeredRepresentationFunctor( s, CapCategory( R ) ),
                       R );
end );


InstallMethod( AsLayeredRepresentationHomomorphism, "for positive integer and quiver representation homomorphism",
               [ IsPosInt, IsQuiverRepresentationHomomorphism ],
function( s, f )
  return ApplyFunctor( AsLayeredRepresentationFunctor( s, CapCategory( f ) ),
                       f );
end );


InstallMethod( AsLayeredRepresentationFunctor, "for positive integer and quiver representation category",
               [ IsPosInt, IsQuiverRepresentationCategory ],
function( s, fcat )
  local T, algebras, t, A, B, ucat, lcat, projs, proj_nts, layered, 
        object_fun, morphism_fun;

  if s = 1 and HasAsLayeredRepresentationFunctor1( fcat ) then
    return AsLayeredRepresentationFunctor1( fcat );
  elif s = 2 and HasAsLayeredRepresentationFunctor2( fcat ) then
    return AsLayeredRepresentationFunctor2( fcat );
  fi;

  T := AlgebraOfCategory( fcat );
  if not IsTensorProductOfAlgebras( T ) then
    Error( "representation category is not over tensor algebra" );
  fi;
  algebras := TensorProductFactors( T );
  if Length( algebras ) <> 2 then
    Error( "representation category over tensor product of ", Length( algebras ), " algebras; ",
           "should be 2" );
  fi;
  if not s in [ 1, 2 ] then
    Error( "tensor factor number must be either 1 or 2, not ", s );
  fi;

  t := 3 - s;
  A := algebras[ s ];
  B := algebras[ t ];
  # Q_A := QuiverOfAlgebra( A );
  # Q_B := QuiverOfAlgebra( B );
  # Q_T := QuiverOfAlgebra( T );
  # prod_path := function( path_a, path_b )
  #   local l;
  #   l := [];
  #   l[ s ] := path_a;
  #   l[ t ] := path_b;
  #   return PathInProductQuiver( Q_T, l );
  # end;
  ucat := CategoryOfQuiverRepresentations( A );
  lcat := CategoryOfQuiverRepresentationsOverVectorSpaceCategory( B, ucat );

  projs := TensorProductProjectionFunctors( fcat )[ s ];
  proj_nts := TensorProductProjectionNaturalTransformations( fcat )[ s ];
  
  layered := CapFunctor( "lrep", fcat, lcat );
  object_fun := function( R )
    return QuiverRepresentation( lcat,
                                 List( projs, P -> ApplyFunctor( P, R ) ),
                                 List( proj_nts, N -> ApplyNaturalTransformation( N, R ) ) );
  end;
  morphism_fun := function( S1, f, S2 )
    return QuiverRepresentationHomomorphism( S1, S2,
                                             List( projs, P -> ApplyFunctor( P, f ) ) );
  end;
  AddObjectFunction( layered, object_fun );
  AddMorphismFunction( layered, morphism_fun );
  return layered;
end );


InstallMethod( AsLayeredRepresentationFunctor1, "for quiver representation category",
               [ IsQuiverRepresentationCategory ],
               fcat -> AsLayeredRepresentationFunctor( 1, fcat ) );

InstallMethod( AsLayeredRepresentationFunctor2, "for quiver representation category",
               [ IsQuiverRepresentationCategory ],
               fcat -> AsLayeredRepresentationFunctor( 2, fcat ) );

InstallMethod( AsLayeredRepresentationElement, "for positive integer and quiver representation element",
               [ IsPosInt, IsQuiverRepresentationElement ],
function( s, e )
  local R, T, Qt, incs, lR, elems, v, i, e_i;
  R := RepresentationOfElement( e );
  T := AlgebraOfRepresentation( R );
  if not IsTensorProductOfAlgebras( T ) then
    Error( "representation is not over tensor algebra" );
  fi;
  Qt := QuiverOfAlgebra( T );
  incs := ProductQuiverInclusions( Qt )[ s ];
  lR := AsLayeredRepresentation( s, R );
  elems := [];
  for v in Vertices( QuiverOfRepresentation( lR ) ) do
    i := VertexIndex( v );
    e_i := RestrictQuiverRepresentationElement( e, incs[ i ], VectorSpaceOfRepresentation( lR, i ) );
    Add( elems, e_i );
  od;
  return QuiverRepresentationElement( lR, elems );
end );

InstallMethod( AsFlatRepresentation, "for positive integer and quiver representation",
               [ IsPosInt, IsQuiverRepresentation ],
function( s, R )
  local t, cat, ucat, A, B, T, Qa, Qb, Qt, objs, v, factors, va, vb, 
        obj_v, morphisms, a, aa, ab, morphism_a;
  if not s in [ 1, 2 ] then
    Error( "tensor factor number must be either 1 or 2, not ", s );
  fi;
  t := 3 - s;
  cat := CapCategory( R );
  ucat := VectorSpaceCategory( cat );
  if not IsQuiverRepresentationCategory( ucat ) then
    return R;
  fi;
  A := AlgebraOfCategory( ucat );
  B := AlgebraOfCategory( cat );
  if s = 1 then
    T := TensorProductOfAlgebras( A, B );
  else
    T := TensorProductOfAlgebras( B, A );
  fi;
  Qa := QuiverOfAlgebra( A );
  Qb := QuiverOfAlgebra( B );
  Qt := QuiverOfAlgebra( T );
  objs := [];
  for v in Vertices( Qt ) do
    factors := ProductPathFactors( v );
    va := factors[ s ];
    vb := factors[ t ];
    obj_v := VectorSpaceOfRepresentation( VectorSpaceOfRepresentation( R, vb ),
                                          va );
    Add( objs, obj_v );
  od;
  morphisms := [];
  for a in Arrows( Qt ) do
    factors := ProductPathFactors( a );
    aa := factors[ s ];
    ab := factors[ t ];
    if IsArrow( aa ) then
      morphism_a := MapForArrow( VectorSpaceOfRepresentation( R, ab ), aa );
    else
      morphism_a := MapForVertex( MapForArrow( R, ab ), aa );
    fi;
    Add( morphisms, morphism_a );
  od;
  return QuiverRepresentation( CategoryOfQuiverRepresentations( T ),
                               objs, morphisms );
end );

InstallMethod( AsFlatRepresentationElement, "for positive integer and quiver representation element",
               [ IsPosInt, IsQuiverRepresentationElement ],
function( s, e )
  local t, R, flat_R, T, Qt, vectors, v, factors, va, vb, vector;
  if not s in [ 1, 2 ] then
    Error( "tensor factor number must be either 1 or 2, not ", s );
  fi;
  t := 3 - s;
  R := RepresentationOfElement( e );
  flat_R := AsFlatRepresentation( s, R );
  T := AlgebraOfRepresentation( flat_R );
  Qt := QuiverOfAlgebra( T );
  vectors := [];
  for v in Vertices( Qt ) do
    factors := ProductPathFactors( v );
    va := factors[ s ];
    vb := factors[ t ];
    vector := ElementVector( ElementVector( e, vb ), va );
    Add( vectors, vector );
  od;
  return QuiverRepresentationElement( flat_R, vectors );
end );


#######################################################################
##
#O  FromEndRToHomRR( <R>, <mat> )
##
##  This function gives a translation from an element in 
##  EndomorphismAlgebra( <R> ) to an endomorphism of the representation  R. 
##
InstallMethod ( FromEndRToHomRR, 
"for a representation and an element in EndomorphismAlgebra", true,
[ IsQuiverRepresentation, IsMatrix ], 0,
function( R, mat )
  local K, dim_vect, maps, i, r;

  K := LeftActingDomain( R ); 
  dim_vect := DimensionVector( R );
  
  maps := [ ];
  r := 1;
  for i in [ 1..Length( dim_vect ) ] do
    if dim_vect[ i ] <> 0 then 
      maps[ i ] := mat{ [ r..r + dim_vect[ i ] - 1 ] }{ [ r..r + dim_vect[ i ] - 1 ] };
      r := r + dim_vect[ i ];
    fi;
  od;
  
  return QuiverRepresentationHomomorphism( R, R, maps );
end
  );

#######################################################################
##
#O  FromHomRRToEndR( <f> )
##
##  This function gives a translation from endomorphisms of a 
##  representation  M  to the corresponding enodomorphism represented 
##  in the algebra EndomorphismAlgebra( M ). 
##
InstallMethod ( FromHomRRToEndR, 
"for an endomorphism to an element of EndomorphismAlgebra",
[ IsQuiverRepresentationHomomorphism ],
function( f )
    local K, dim_vect, matrices, end_f, r, j, matrix;

    K := FieldOfRepresentation( Source( f ) );
    dim_vect := DimensionVector( Source( f ) );
    matrices := MatricesOfRepresentationHomomorphism( f );
    end_f := NullMat( Dimension( Source( f ) ), Dimension( Source( f ) ), K );
    r := 1; 
    for j in [ 1..Length( dim_vect ) ] do 
      if dim_vect[ j ] <> 0 then
          matrix := RowsOfMatrix( matrices[ j ] );
        end_f{ [ r..r + dim_vect[ j ] - 1 ] }{ [ r..r + dim_vect[ j ] - 1 ] } := matrix;
        r := r + dim_vect[ j ];
      fi;
    od; 

    return end_f;
end
);

#######################################################################
##
#A  EndomorphismAlgebra( <R> )
##
##  This function computes endomorphism ring of the module  <R>  and
##  representing it as an general GAP algebra. The algorithm it uses is
##  based purely on linear algebra.
##
InstallMethod( EndomorphismAlgebra,
"for a representations of a quiver",
[ IsQuiverRepresentation ], 0,
function( R )
    local   EndR,  A,  F,  dim_R,  alglist,  i,  B,  maps,  totaldim,  
            r,  gen,  j,  tempmat;

  EndR := BasisVectors( Basis( Hom( R, R ) ) );
  A := AlgebraOfRepresentation( R ); 
  F := LeftActingDomain( A );
  dim_R := DimensionVector( R );
  alglist := [ ];
  for i in [ 1..Length( dim_R ) ] do 
    if dim_R[ i ] <> 0 then 
      Add( alglist, MatrixAlgebra( F, dim_R[ i ] ) );
    fi;
  od;
  B := DirectSumOfAlgebras( alglist ); 
  
  maps := [];
  totaldim := Sum( dim_R );
  for i in [ 1..Length( EndR ) ] do
    maps[ i ] := NullMat( totaldim, totaldim, F );
    r := 1;
    gen := MapsOfRepresentationHomomorphism( EndR[ i ] );
    for j in [ 1..Length( dim_R ) ] do
      if dim_R[ j ] <> 0 then
        tempmat := ColsOfMatrix( LeftMatrixOfLinearTransformation( gen[ j ] ) );
        maps[ i ]{ [ r..r + dim_R[ j ] - 1 ] }{ [ r..r + dim_R[ j ] - 1 ] } := tempmat;
      fi;
      r := r + dim_R[ j ];
    od; 
  od;

  return SubalgebraWithOne( B, maps, "basis" ); 
end
  );

#######################################################################
##
#A  AnnihilatorOfRepresentation( <R> )
##
##  Given a representation  R  over a (quotient of a) path algebra  A, this 
##  function computes the annihilator of  R  as an ideal in  A. 
##
InstallMethod ( AnnihilatorOfRepresentation, 
"for a QuiverRepresentation",
[ IsQuiverRepresentation ],
function( R )

  local A, BR, BA, matrix, a, temp, r, solutions, annihilator;

  A := AlgebraOfRepresentation( R );
  if not IsFiniteDimensional( A ) then
    Error( "The representation is not over a finite dimensional algebra.\n" );
  fi;
  #   
  #  If the representation  R  is zero, return the whole algebra.
  #
  if Dimension( R ) = 0 then 
    return Ideal( A, GeneratorsOfAlgebra( A ) );
  fi;
  BR := BasisVectors( Basis( R ) );
  #
  #  Setting things up right according to the input.
  #
  BA := BasisVectors( Basis( A ) );
  #
  #  Computing the linear system to solve in order to find the annihilator
  #  of the representation R.
  #
  matrix := [ ];
  for a in BA do
    temp := [ ];
    for r in BR do
      Add(temp, AsList( AsVector( QuiverAlgebraAction( r, a ) ) ) );
    od;
    Add( matrix, temp );
  od;
  
  matrix := List( matrix, x -> Flat( x ) );
  #
  #  Finding the solutions of the linear system, and creating the solutions
  #  as elements of the algebra  A.
  #
  solutions := NullspaceMat( matrix );
  annihilator := List( solutions, x -> LinearCombination( BA, x ) );
  
  return Ideal( A, annihilator );
end
);

#######################################################################
##
#O  IntersectionOfRepresentations( <args> )
##
##                                 f_i             
##  Given subrepresentations  R_i -----> X  for i = 1,...,n of a repre-
##  sentation  X  by  n  monomorphism  f_i, this function computes 
##  the intersection of the images of all the  f_i.
##  
InstallMethod ( IntersectionOfRepresentations, 
"for a list of IsQuiverRepresentationHomomorphisms",
[ IsDenseList ], 
function( list )

  local A, f, U, gprime;
#
#   Checking the input
#
    if IsEmpty( list ) then 
        Error( "<list> must be non-empty" ); 
    fi;
    if not ForAll( list, IsQuiverRepresentationHomomorphism ) then 
      Error( "all entries in <list> must be homomorphisms of representations over some quiver algebra.\n" );
    fi;
    A := AlgebraOfRepresentation( Source( list[ 1 ] ) );
    for f in list do
        if AlgebraOfRepresentation( Source( f ) ) <> A then
            Error( "all entries in <list> must be homomorphisms of representations over the same algebra.\n" );
        fi;
    od;
    if not ForAll( list, IsMonomorphism ) then 
        Error( "not all the arguments are monomorphisms.\n" );
    fi;
    U := Range( list[ 1 ] );
    if not ForAll( list, x -> Range( x ) = U ) then 
        Error( "must have submodules of the same representation.\n" );
    fi;
#
#   Doing the computations.
#
    gprime := ProjectionInFactorOfFiberProduct( list, 1 );

    return PreCompose( gprime, list[ 1 ] );
end
);

InstallMethod( DimensionVectorPartialOrder, 
"for two QuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( R1, R2) 

  local L1, L2;
  
  L1 := DimensionVector( R1 );
  L2 := DimensionVector( R2 );
  
  return ForAll( L2 - L1, x -> ( x >= 0 ) );
end
  );

#######################################################################
##
#O  IsomorphismOfRepresentations( <M>, <N> )
##
##  Given two modules  <M>  and  <N>  over a (quotient of a) path algebra
##  this function return an isomorphism from  <M>  to  <N>  is the two
##  modules are isomorphic, and false otherwise.
##
InstallMethod ( IsomorphismOfRepresentations, 
"for two IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ], 
function( R, S )

    local   K,  dim_R,  mats,  i,  splittingmaps,  maps,  fn,  fsplit,  
            gn,  gsplit,  f_alpha,  f_beta,  g_alpha,  g_beta,  q,  B,  
            genImages,  delta,  recursiveIOM;
    # 
    # If  R  and  S are not representations over the same algebra, 
    # return an error message.
    #
    if AlgebraOfRepresentation( R ) <> AlgebraOfRepresentation( S ) then 
        return false;
    fi;
    #
    # If the dimension vectors of  R  and  S  are different, they
    # are not isomorphic, so return  false.
    #
    if DimensionVector( R ) <> DimensionVector( S ) then
        return false;
    fi;
    #
    # If both  R  and  S  are the zero module, return the zero map.
    #
    if Dimension( R ) = 0 and Dimension( S ) = 0 then
        return ZeroMorphism( R, S );
    fi;
    #
    # If  R  and  S  are the same representations (i.e. with the 
    # same vector spaces and defining matrices, then return the 
    # "identity homomorphism". 
    #
    if R = S then 
        K := FieldOfRepresentation( R );
        dim_R := DimensionVector( R );
        mats := [];
        for i in [ 1..Length( dim_R ) ] do
            if dim_R[ i ] = 0 then
                Add( mats, NullMat( 1, 1, K ) );
            else
                Add( mats, IdentityMat( dim_R[ i ], K ) );
            fi;
        od;
        
        return QuiverRepresentation( R, S, mats );
    fi;
    #
    # Finds a homomorphism from  R  to  S  and corresponding homomorphism
    # from  S  to  R  identifying a common non-zero direct summand of  R  
    # and  S, if such exists. Otherwise it returns false. 
    #
    splittingmaps := function( R, S) 
        local   HomRS,  HomSR,  rs,  sr,  m,  n,  l,  zero,  j,  i,  
                temp,  comp,  f,  fsr,  srf, k;
            
        HomRS := BasisVectors( Basis( Hom( R, S ) ) );
        HomSR := BasisVectors( Basis( Hom( S, R ) ) );
        rs := Length( HomRS );
        sr := Length( HomSR );
      
        if rs = 0 or sr = 0 then 
            return false;
        fi;
      
        m := Maximum( DimensionVector( R ) );
        n := Maximum( DimensionVector( S ) );
        if n = m then
            l := n;
        else
            l := Minimum( [ n, m ] ) + 1;
        fi;
      
        zero := ZeroMorphism( R, R );
      
        for j in [ 1..sr ] do
            for i in [ 1..rs ] do
                if l > 1 then  # because hom^0*hom => error!
                    temp := PreCompose( HomRS[ i ], HomSR[ j ] );
                    comp := [];
                    for k in [ 1..l-1] do
                        Add( comp, temp );
                    od;
                    f := PreCompose( PreCompose( comp ), HomRS[ i ] );
                else 
                    f := HomRS[ i ];
                fi;
                
                fsr := PreCompose( f, HomSR[ j ] );
              
                if fsr <> zero then
                    srf := PreCompose( HomSR[ j ], f ); 
                    return [ fsr, srf, HomRS[ i ] ];
                fi;
            od;
        od;

        return false;
    end; 
    
    maps := splittingmaps( R, S );
    #
    # No common non-zero direct summand,  R  and  S  are not isomorphic,
    # return false.
    #
    if maps = false then
        return false;
    fi;
    #
    # R  and  S  has a common non-zero direct summand, find the splitting of
    # the inclusion of this common direct summand in  R  and in  S.
    #
    fn := ImageEmbedding( maps[ 1 ] );
    fsplit := RightInverseOfHomomorphism( fn );
    gn := ImageEmbedding( maps[ 2 ] );
    gsplit := RightInverseOfHomomorphism( gn );    
    #
    # Find the idempotents corresponding to these direct summands.
    #
    f_alpha := CoastrictionToImage( PreCompose( fsplit, fn ) );
    f_beta := KernelEmbedding( PreCompose( fsplit, fn ) );    
    g_alpha := ImageEmbedding( PreCompose( gsplit, gn ) );
    g_beta := KernelEmbedding( PreCompose( gsplit, gn ) );
    #
    # If this direct summand is all of  R  and  S, return the appropriate
    # isomorphism.
    #
    if Dimension( Source( f_beta ) ) = 0 and Dimension( Source( g_beta ) ) = 0 then
       return PreCompose( maps[ 1 ], maps[ 3 ] ); 
    fi;
    #
    # If this direct summand is not all of  R  and  S, recursively construct
    # a possible isomorphism between the complements of this common direct 
    # summand in  R  and in  S.
    #
    q := PreCompose( [ f_alpha, maps[ 1 ], maps[ 3 ] ] );
    
    B := BasisVectors( Basis( Source( q ) ) );
    genImages := List( B, x -> PreImagesRepresentative( g_alpha, ImageElm( q, x ) ) );
    
    delta := QuiverRepresentationHomomorphismByImages( Source( f_alpha ), Source( g_alpha ), genImages );
    recursiveIOM := IsomorphismOfRepresentations( Source( f_beta ), Source( g_beta ) );
   
    if recursiveIOM <> false then
        return PreCompose( [ RightInverseOfHomomorphism( f_alpha ), delta, g_alpha ] ) + 
               PreCompose( [ RightInverseOfHomomorphism( f_beta ), recursiveIOM, g_beta ] );        
    else 
        return false;
    fi;
end
  ); # IsomorphismOfModules

#######################################################################
##
#O  SumOfSubobjects( <args> )
##                        f_i             
##  Given subobjects M_i -----> X  for i = 1,...,n of an object  X  by
##  n  monomorphism  f_i, this function computes the sum of the images
##  of  f_i  in the sense that it computes the subobject \sum_i M_i
##  generated by the images of  f_i.  It returns the maps 
##  \sum_i M_i ---> X,  and M_i ---> \sum_i M_i.
##
InstallMethod ( SumOfSubobjects, 
"for a list IsFieldCategoryMorphism",
[ IsDenseList ], 
function( list )

    local   A,  f,  sources,  inclusions;
#
#   Checking the input
#
    if IsEmpty( list ) then 
        Error( "<list> must be non-empty" ); 
    fi;
    if not ForAll( list, IsFieldCategoryMorphism ) then
        Error( "Not all the arguments are IsFieldCategoryMorphism.\n" );
    fi;
    A := CapCategory( Source( list[ 1 ] ) );
    for f in list do
        if CapCategory( Source( f ) ) <> A then
            Error( "All entries in <list> must be in the same category.\n" );
        fi;
    od;
    if not ForAll( list, IsMonomorphism ) then 
        Error("Not all the arguments are monomorphisms.\n");
    fi;
    A := Range( list[ 1 ] );
    if not ForAll( list, x -> Range( x ) = A ) then
        Error( "must have subobjects of the same object.\n" );
    fi;   
    
#
#   Doing the computations
#
    f := UniversalMorphismFromDirectSum( list ); 
    sources := List( list, x -> Source( x ) );
    inclusions := List( [ 1..Length( sources ) ], i -> InjectionOfCofactorOfDirectSum( sources, i ) );
    
    return [ f, List( inclusions, i -> PreCompose( i, f ) ) ];
end
);

#######################################################################
##
#O  HomFactoringThroughProj( <M>, <N> )
##
##  Given two representations  M  and  N  over a finite dimensional quotient
##  of a path algebra, this function computes a basis of the homomorphisms
##  that factor through a projective representation.
##
InstallMethod ( HomFactoringThroughProj,
"for two IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( M, N )

    local   f,  HomMP_N,  homthroughproj;
    #
    # Testing input
    #
    if AlgebraOfRepresentation( M ) <> AlgebraOfRepresentation(N) then
        Error("the entered representations are not over the same algebra,");
    fi;
    #
    # if one representation is zero, return the empty list
    #
    if Dimension( M ) = 0 or Dimension( N ) = 0 then
        return [];
    fi;
    #
    # The maps factoring through projectives are those factoring through
    # the projective cover of  N.
    #
    f := ProjectiveCover( N );
    HomMP_N := BasisVectors( Basis( Hom( M, Source( f ) ) ) );
    homthroughproj := List( HomMP_N, x -> PreCompose( x, f ) );
    homthroughproj := SubspaceInclusion( Hom( M, N ), homthroughproj );
    
    return homthroughproj;
end
);

#######################################################################
##
#O  StableEndomorphismAlgebraModuloProj( <R> )
##
##  Given a representation  R  over a finite dimensional quotient of a
##  path algebra, this function computes the homomorphism from
##  End(R) ----> End(R)/P(R,R), where P(R,R) is the ideal in End(R)
##  generated by the morphisms factoring through projective modules.
##  If  R  is the zero representation, then an empty list is returned.
##
InstallMethod ( StableEndomorphismAlgebraModuloProj,
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ],
function( R )

    local EndR, factorproj, I;
    
    if Dimension( R ) = 0 then 
        return [ ];
    fi;
    EndR := EndomorphismAlgebra( R );
    factorproj := HomFactoringThroughProj( R, R );
    factorproj := List( BasisVectors( Basis( Source( factorproj ) ) ), x -> 
                        FromHomRRToEndR( ImageElm( factorproj, x ) ) );
    I :=  Ideal( EndR, factorproj );
    
    return NaturalHomomorphismByIdeal( EndR, I );
end
);
