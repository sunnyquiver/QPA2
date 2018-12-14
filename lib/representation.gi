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
    if IsBound( vectors[ i ] ) then
      vectors[ i ] := Vector( space, vectors[ i ] );
    else
      vectors[ i ] := Zero( space );
    fi;
  od;
  return QuiverRepresentationElementNC( R, vectors );
end );

InstallMethod( QuiverRepresentationElementNC, "for quiver representation and collection",
               [ IsQuiverRepresentation, IsDenseList ],
function( R, vectors )
  local type, elem, V, vector;
  type := NewType( FamilyOfQuiverRepresentationElements,
                   IsQuiverRepresentationElement and IsQuiverRepresentationElementRep );
  elem := rec();
  V := AsQPAVectorSpace( R );
  vector := Vector( V, Concatenation( List( vectors, AsList ) ) );
  ObjectifyWithAttributes( elem, type,
                           RepresentationOfElement, R,
                           ElementVectors, vectors,
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
  return QuiverRepresentationElement( R, [ Target( p ) ], [ target_vec ] );
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
  return QuiverRepresentationHomomorphismNC
  ( Source( m ), Range( m ), c*List( Vertices( Q ), v -> MapForVertex( m, v ) ) );
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
      source := objects[ VertexNumber( Source( a ) ) ];
      range := objects[ VertexNumber( Target( a ) ) ];
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
      matrices[ ArrowNumber( arrows[ i ] ) ] := matrices_for_arrows[ i ];
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
  if IsQuotientOfPathAlgebra( A ) then
    R := QuiverRepresentation
         ( CategoryOfQuiverRepresentations( PathAlgebra( A ) ),
           objects, morphisms );
    return AsRepresentationOfQuotientAlgebra( R, A );
  fi;

  Q := QuiverOfAlgebra( A );
  vertices := Vertices( Q );
  num_vertices := Length( vertices );
  arrows := Arrows( Q );
  num_arrows := Length( arrows );
  vecspace_cat := VectorSpaceCategory( cat );

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
      morphisms[ ArrowNumber( arrows[ i ] ) ] := morphisms_for_arrows[ i ];
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
      source := objects[ VertexNumber( Source( a ) ) ];
      target := objects[ VertexNumber( Target( a ) ) ];
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
  local kQ, rels, rel, R_;
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
  R_ := QuiverRepresentationNC
        ( CategoryOfQuiverRepresentations( A ),
          VectorSpacesOfRepresentation( R ),
          MapsOfRepresentation( R ) );
  SetAsRepresentationOfPathAlgebra( R_, R );
  return R_;
end );

InstallMethod( AsRepresentationOfPathAlgebra, "for quiver representation",
               [ IsQuiverRepresentation ],
function( R )
  local A;
  A := AlgebraOfRepresentation( R );
  if IsPathAlgebra( A ) then
    return R;
  else
    return QuiverRepresentationNC
           ( PathAlgebra( A ),
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
                         morphisms[ VertexNumber( Target( a ) ) ] );
    comp2 := PreCompose( morphisms[ VertexNumber( Source( a ) ) ],
                         MapForArrow( R2, a ) );
    if not IsEqualForMorphisms( comp1, comp2 ) then
      Error( "maps for representation homomorphism do not commute with maps for arrow ", a );
    fi;
  od;

  return QuiverRepresentationHomomorphismNC( R1, R2, morphisms );
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

InstallMethod( IsZero, [ IsQuiverRepresentationHomomorphism ],
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

InstallMethod( \+,
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
               [ IsQuiverRepresentationHomomorphism, IsVertex ],
function( f, v )
  return MapsOfRepresentationHomomorphism( f )[ VertexNumber( v ) ];
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


InstallMethod( CategoryOfQuiverRepresentations, "for quiver algebra",
               [ IsQuiverAlgebra ],
function( A )
  return CategoryOfQuiverRepresentationsOverVectorSpaceCategory
         ( A, UnderlyingCategoryForRepresentations( A ) );
end );

InstallMethod( CategoryOfQuiverRepresentationsOverVectorSpaceCategory,
               "for quiver algebra and vector space category",
               [ IsQuiverAlgebra, IsAbelianCategory ],
function( A, vecspace_cat )
  local Q, cat, equal_objects, equal_morphisms, zero_object, zero_morphism,
        identity_morphism, pre_compose, addition, additive_inverse,
        kernel, kernel_emb, coker, coker_proj,
        mono_lift, epi_colift,
        direct_sum, direct_sum_inj, direct_sum_proj, to_be_finalized;

  Q := QuiverOfAlgebra( A );

  cat := CreateCapCategory( Concatenation( "quiver representations over ", String( A ) ) );
  SetFilterObj( cat, IsQuiverRepresentationCategory );
  SetAlgebraOfCategory( cat, A );
  SetVectorSpaceCategory( cat, vecspace_cat );

  SetIsAbelianCategory( cat, true );
  SetIsAbelianCategoryWithEnoughProjectives( cat, true );

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
    local zero;
    zero := ZeroObject( vecspace_cat );
    return QuiverRepresentation( cat, List( Vertices( Q ), v -> zero ), [] );
  end;
  AddZeroObject( cat, zero_object );

  zero_morphism := function( R1, R2 )
    return QuiverRepresentationHomomorphism
           ( R1, R2,
             ListN( VectorSpacesOfRepresentation( R1 ),
                    VectorSpacesOfRepresentation( R2 ),
                    ZeroMorphism ) );
  end;
  AddZeroMorphism( cat, zero_morphism );

  # TODO: need IsZeroForMorphisms?

  identity_morphism := function( R )
    return QuiverRepresentationHomomorphism
           ( R, R,
             List( VectorSpacesOfRepresentation( R ),
                   IdentityMorphism ) );
  end;
  AddIdentityMorphism( cat, identity_morphism );

  pre_compose := function( m1, m2 )
    return QuiverRepresentationHomomorphism
           ( Source( m1 ), Range( m2 ),
             ListN( MapsOfRepresentationHomomorphism( m1 ),
                    MapsOfRepresentationHomomorphism( m2 ),
                    PreCompose ) );
  end;
  AddPreCompose( cat, pre_compose );

  addition := function( m1, m2 )
    return QuiverRepresentationHomomorphism
           ( Source( m1 ), Range( m1 ),
             ListN( MapsOfRepresentationHomomorphism( m1 ),
                    MapsOfRepresentationHomomorphism( m2 ),
                    AdditionForMorphisms ) );
  end;
  AddAdditionForMorphisms( cat, addition );

  additive_inverse := function( m )
    return QuiverRepresentationHomomorphism
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
    ker := QuiverRepresentation
           ( cat, ker_objs, List( Arrows( Q ), map_for_arrow ) );
    return QuiverRepresentationHomomorphism
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
    return QuiverRepresentation
           ( cat,
             List( MapsOfRepresentationHomomorphism( m ),
                   CokernelObject ),
             List( Arrows( Q ),
                   map_for_arrow ) );
  end;
  coker_proj := function( m )
    return QuiverRepresentationHomomorphism
           ( Range( m ), coker( m ),
             List( MapsOfRepresentationHomomorphism( m ),
                   CokernelProjection ) );
  end;
  AddCokernelProjection( cat, coker_proj );

  mono_lift := function( i, test )
    return QuiverRepresentationHomomorphism
           ( Source( test ), Source( i ),
             ListN( MapsOfRepresentationHomomorphism( i ),
                    MapsOfRepresentationHomomorphism( test ),
                    LiftAlongMonomorphism ) );
  end;
  AddLiftAlongMonomorphism( cat, mono_lift );

  epi_colift := function( e, test )
    return QuiverRepresentationHomomorphism
           ( Range( e ), Range( test ),
             ListN( MapsOfRepresentationHomomorphism( e ),
                    MapsOfRepresentationHomomorphism( test ),
                    ColiftAlongEpimorphism ) );
  end;
  AddColiftAlongEpimorphism( cat, epi_colift );

  direct_sum := function( summands )
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

  direct_sum_inj := function( summands, i, sum )
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
  
  direct_sum_proj := function( summands, i, sum )
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
  
  AddEpimorphismFromSomeProjectiveObject( cat, ProjectiveCover );

  # The object/morphism constructors check whether the input is well-defined or not.
  # So if it has been created then it is well-defined
  AddIsWellDefinedForObjects( cat, ReturnTrue );
  AddIsWellDefinedForMorphisms( cat, ReturnTrue );

  to_be_finalized := ValueOption( "FinalizeCategory" );
  if to_be_finalized <> false then
    to_be_finalized := true;
  fi;
   
  if to_be_finalized then
    Finalize( cat );
  fi;
  
  return cat;
    
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
    Add( spanningset[ VertexNumber( v ) ], ElementVector( g, v ) );
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
    s := VertexNumber( Source( a ) );
    t := VertexNumber( Target( a ) );
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
  n := VertexNumber( supp[1] );
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

InstallMethod( ProjectiveCover, "for a quiver representation",
               [ IsQuiverRepresentation ],
function( R )
  local   mingen,  maps,  PR,  projections;

  if Sum( DimensionVector( R ) ) = 0 then
    return ZeroMorphism( ZeroObject( CapCategory( R ) ), R);
  else
    mingen := MinimalGeneratingSet( R );
    maps := List( mingen, x -> HomFromProjective( x, R ) );
    return UniversalMorphismFromDirectSum( maps );
  fi;
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
        if Length( spanofgens[ VertexNumber( target ) ][ 1 ] ) = VertexDimension( R1, target ) then
            return;
        fi;
        vec := AsList( ElementVector( PathAction( g, p ), target ) ); 
        temp := Concatenation( spanofgens[ VertexNumber( target ) ][ 1 ], [ vec ] ); 
        if RankMat( temp ) = Length( temp ) then 
            Add( spanofgens[ VertexNumber( target ) ][ 1 ], vec );
            Add( spanofgens[ VertexNumber( target ) ][ 2 ], p );
            Add( spanofgens[ VertexNumber( target ) ][ 3 ], g );
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
            temp := spanofgens[  VertexNumber( v ) ];
            basis := temp[ 1 ];
            paths := temp[ 2 ];
            gens := temp[ 3 ];
            matrix := List( [ 1..Length( gens ) ], i -> PathAction( images[ Position( generators, gens[ i ] ) ], paths[ i ] ) );
            matrix := List( matrix, m -> AsList( ElementVector( m, v ) ) );
            basischangemat := temp[ 1 ]^( -1 );            
            hommatrices[ VertexNumber( v ) ] := basischangemat * matrix; 
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
InstallMethod( InverseOp,
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


InstallMethod( AsLayeredRepresentation,
               [ IsPosInt, IsQuiverRepresentation ],
function( s, R )
  local T, algebras, t, A, B, paths, Qa, Qb, Qt, catA, reps, w, objs, 
        maps, rep, morphisms, b, source, range, morphism, cat;
  T := AlgebraOfRepresentation( R );
  if not IsTensorProductOfAlgebras( T ) then
    Error( "representation is not over tensor algebra" );
  fi;
  algebras := TensorProductFactors( T );
  if Length( algebras ) <> 2 then
    Error( "representation over tensor product of ", Length( algebras ), " algebras; ",
           "should be 2" );
  fi;
  if not s in [ 1, 2 ] then
    Error( "tensor factor number must be either 1 or 2, not ", s );
  fi;
  t := 3 - s;
  A := algebras[ s ];
  B := algebras[ t ];
  paths := function( path_a, path_b )
    local l;
    l := [];
    l[ s ] := path_a;
    l[ t ] := path_b;
    return l;
  end;
  # construct a representation over B,
  # consisting of A-representations
  Qa := QuiverOfAlgebra( A );
  Qb := QuiverOfAlgebra( B );
  Qt := QuiverOfAlgebra( T );
  catA := CategoryOfQuiverRepresentations( A );
  reps := [];
  for w in Vertices( Qb ) do
    objs := List( Vertices( Qa ),
                  v -> VectorSpaceOfRepresentation( R, PathInProductQuiver( Qt, paths( v, w ) ) ) );
    maps := List( Arrows( Qa ),
                  a -> MapForArrow( R, PathInProductQuiver( Qt, paths( a, w ) ) ) );
    rep := QuiverRepresentation( catA, objs, maps );
    Add( reps, rep );
  od;
  morphisms := [];
  for b in Arrows( Qb ) do
    maps := List( Vertices( Qa ),
                  v -> MapForArrow( R, PathInProductQuiver( Qt, paths( v, b ) ) ) );
    source := reps[ VertexNumber( Source( b ) ) ];
    range := reps[ VertexNumber( Target( b ) ) ];
    morphism := QuiverRepresentationHomomorphism( source, range, maps );
    Add( morphisms, morphism );
  od;
  cat := CategoryOfQuiverRepresentationsOverVectorSpaceCategory( B, catA );
  return QuiverRepresentation( cat, reps, morphisms );
end );


InstallMethod( AsFlatRepresentation, "for quiver representation",
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
#O  DecomposeRepresentation( <R> )
##
##  Given a representation  <R>  this function computes a list of 
##  representations  L  such that  <R>  is isomorphic to the direct 
##  sum of the representations on the list  L. 
##
InstallMethod( DecomposeRepresentation, 
"for a quiver representation", true,
[ IsQuiverRepresentation ], 0, 
function( R )

  local endo, idemmaps;

  endo := EndomorphismAlgebra( R );
  idemmaps := CompleteSetOfPrimitiveIdempotents( endo );
  idemmaps := List( idemmaps, x -> FromEndRToHomRR( R, x ) );
    
  return List( idemmaps, x -> ImageObject( x ) );
end
  );
