BindGlobal( "FamilyHomSpaces",
        NewFamily( "hom spaces" ) );
BindGlobal( "FamilyOfQuiverRepresentationHomSpaces",
        CollectionsFamily( FamilyOfQuiverRepresentationHomomorphisms ) );
DeclareRepresentation( "IsHomSpaceRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [] );

InstallMethod( Hom,
               [ IsFieldCategoryObject, IsFieldCategoryObject ],
function( X, Y )
    local   cat,  F,  vcat,  fam,  hom_space_type,  type,  hom;
  cat := CapCategory( X );
  if not IsIdenticalObj( cat, CapCategory( Y ) ) then
    Error( "objects from different categories" );
  fi;
  F := UnderlyingField( cat );
  vcat := CategoryOfVectorSpaces( F );
  fam := FamilyHomSpaces;
  if IsVectorSpaceCategory( cat ) then
      hom_space_type := IsVectorSpaceHomSpace;
      fam := CollectionsFamily( FamilyOfLinearTransformations );
  elif IsQuiverRepresentationCategory( cat ) then
      hom_space_type := IsQuiverRepresentationHomSpace;
      fam := CollectionsFamily( FamilyOfQuiverRepresentationHomomorphisms );
  elif IsLeftQuiverModuleCategory( cat ) then
      hom_space_type := IsLeftQuiverModuleHomSpace;
      fam := CollectionsFamily( FamilyOfQuiverModuleHomomorphisms );
  elif IsRightQuiverModuleCategory( cat ) then
      hom_space_type := IsRightQuiverModuleHomSpace;
      fam := CollectionsFamily( FamilyOfQuiverModuleHomomorphisms );
  elif IsQuiverBimoduleCategory( cat ) then
      hom_space_type := IsQuiverBimoduleHomSpace;
      fam := CollectionsFamily( FamilyOfQuiverModuleHomomorphisms );
  elif IsStableCategoryModuloProjectives( cat ) then
      hom_space_type := IsStableHomSpaceModuloProjectives;
      fam := CollectionsFamily( FamilyOfStableMorphisms );
  else
    hom_space_type := IsHomSpace; # should not happen
  fi;
  type := NewType( fam, hom_space_type and IsHomSpaceRep );
  hom := rec();
  ObjectifyWithAttributes( hom, type,
                           HomCategory, cat,
                           Source, X,
                           Range, Y );
  Add( vcat, hom );
  return Intern( hom );
end );

InstallMethod( Zero, [ IsHomSpace ],
function( hom )
  return ZeroMorphism( Source( hom ), Range( hom ) );
end );

InstallMethod( HomFunctor, "for field category",
               [ IsFieldCategory ],
function( cat )
  local hom, morphism_fun;
  hom := CapFunctor( "Hom",
                     [ [ cat, true ], [ cat, false ] ],
                     CategoryOfVectorSpaces( UnderlyingField( cat ) ) );
  AddObjectFunction( hom, Hom );
  morphism_fun := function( source, f1, f2, range )
    return Hom( f1, f2 );
  end;
  AddMorphismFunction( hom, morphism_fun );
  return hom;
end );


####### Hom spaces for vector spaces #######

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
                           l -> LinearTransformation( V1, V2, l ) );
  fi;
  basis := rec();
  ObjectifyWithAttributes( basis,
                           NewType( FamilyOfVectorSpaceBases,
                                    IsBasis and IsVectorSpaceBasisRep ),
                           BasisVectors, basis_vectors,
                           UnderlyingLeftModule, hom,
                           IsCanonicalBasis, true );
  return basis;
end );
  
InstallMethod( Dimension, [ IsVectorSpaceHomSpace ],
function( hom )
  # for vector spaces, it is easy to find the dimension
  # of the Hom space without computing a basis
  return Dimension( Source( hom ) ) * Dimension( Range( hom ) );
end );


####### Hom spaces for quiver representations #######

InstallMethod( CanonicalBasis, [ IsQuiverRepresentationHomSpace ],
function( hom )
  local ker, ker_basis, vertex_spaces, vertex_projection_maps, 
        ker_to_morphism, basis_morphisms, basis;

  # ker := KernelEmbedding( HomSpaceVertexToArrowMap( hom ) );
  # ker_basis := Basis( Source( ker ) );
  #
  # vertex_spaces := VertexHomSpaces( hom );
  # vertex_projection_maps := List( [ 1 .. Length( vertex_spaces ) ],
  #                                 i -> ProjectionInFactorOfDirectSum( vertex_spaces, i ) );
  #
  # ker_to_morphism := function( v )
  #   local vertex_sum_elem, vertex_projections;
  #   vertex_sum_elem := ImageElm( ker, v );
  #   vertex_projections := List( vertex_projection_maps,
  #                               m -> ImageElm( m, vertex_sum_elem ) );
  #   return QuiverRepresentationHomomorphism
  #          ( Source( hom ), Range( hom ), vertex_projections );
  # end;
  #
  # basis_morphisms := List( ker_basis, ker_to_morphism );

  ## Above: Compute basis by using underlying category
  ## (commented out, since it is very slow)
  ## Below: Compute basis by combining matrices
  ## (not very readable, but much faster than the above)

  basis_morphisms := BasisOfHom( Source( hom ), Range( hom ) );

  basis := rec();
  ObjectifyWithAttributes( basis,
                           NewType( FamilyOfVectorSpaceBases,
                                    IsBasis and IsVectorSpaceBasisRep ),
                           BasisVectors, basis_morphisms,
                           UnderlyingLeftModule, hom,
                           IsCanonicalBasis, true );
  return basis;
end );

InstallMethod( AsList, [ IsQuiverRepresentationHomomorphism ],
function( f )
  local hom, ker, maps, vertex_spaces, n, maps_in_sum;
  hom := SpaceContainingVector( f );
  ker := KernelEmbedding( HomSpaceVertexToArrowMap( hom ) );
  maps := MapsOfRepresentationHomomorphism( f );
  vertex_spaces := VertexHomSpaces( hom );
  n := Length( vertex_spaces );
  maps_in_sum := List( [ 1 .. n ],
                       i -> ImageElm( InjectionOfCofactorOfDirectSum( vertex_spaces, i ),
                                      maps[ i ] ) );
  return AsList( PreImagesRepresentative( ker, Sum( maps_in_sum ) ) );
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
      if v = Source( a ) and v = Target( a ) then
        m := Hom( MapForArrow( R1, a ),
                  VectorSpaceOfRepresentation( R2, v ) )
             - Hom( VectorSpaceOfRepresentation( R1, v ),
                    MapForArrow( R2, a ) );
      elif v = Source( a ) then
        m := - Hom( VectorSpaceOfRepresentation( R1, v ),
                    MapForArrow( R2, a ) );
        # f |-> PreCompose( f, MapForArrow( R2, a ) )
      elif v = Target( a ) then
        m := Hom( MapForArrow( R1, a ),
                  VectorSpaceOfRepresentation( R2, v ) );
        # g |-> PreCompose( MapForArrow( R1, a ), g )
      else
        m := ZeroMorphism( vertex_spaces[ VertexIndex( v ) ],
                           arrow_spaces[ ArrowIndex( a ) ] );
      fi;
      Add( morphisms_v, m );
    od;
    Add( morphisms, morphisms_v );
  od;
  
  return MorphismBetweenDirectSums( VertexHomSpaces( hom ),
                                    morphisms,
                                    ArrowHomSpaces( hom ) );
end );



####### Hom spaces for quiver modules #######

DeclareSideOperations( IsQuiverModuleHomSpace,
                       IsLeftQuiverModuleHomSpace, IsRightQuiverModuleHomSpace,
                       IsQuiverBimoduleHomSpace );

InstallMethod( UnderlyingRepresentationHomSpace, "for quiver module hom space",
               [ IsQuiverModuleHomSpace ],
function( hom )
  return Hom( UnderlyingRepresentation( Source( hom ) ),
              UnderlyingRepresentation( Range( hom ) ) );
end );

InstallMethod( Side, "for quiver module hom space",
               [ IsQuiverModuleHomSpace ],
               hom -> Side( Source( hom ) ) );

InstallMethod( MorphismByFunction, "for quiver module hom space and function",
               [ IsQuiverModuleHomSpace, IsFunction ],
function( hom, f )
  return QuiverModuleHomomorphism( Source( hom ), Range( hom ), f );
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
                           UnderlyingLeftModule, hom,
                           IsCanonicalBasis, true );
  return basis;
end );

InstallMethod( AsList, [ IsQuiverModuleHomomorphism ],
               m -> AsList( UnderlyingRepresentationHomomorphism( m ) ) );
