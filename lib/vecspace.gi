DeclareRepresentation( "IsVectorSpaceRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [ ] );
DeclareRepresentation( "IsVectorSpaceBasisRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [ ] );
DeclareRepresentation( "IsVectorRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [ "type", "field", "entries" ] );
DeclareRepresentation( "IsLinearTransformationRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [ ] );

BindGlobal( "FamilyOfRowVectors",
            NewFamily( "row vectors" ) );
BindGlobal( "FamilyOfColVectors",
            NewFamily( "col vectors" ) );
BindGlobal( "FamilyOfRowVectorSpaces",
            CollectionsFamily( FamilyOfRowVectors ) );
BindGlobal( "FamilyOfColVectorSpaces",
            CollectionsFamily( FamilyOfColVectors ) );
BindGlobal( "FamilyOfVectorSpaceBases",
            NewFamily( "vector space bases" ) );
BindGlobal( "FamilyOfLinearTransformations",
            NewFamily( "linear transformations" ) );

InstallMethod( MakeQPAVector, [ IsString, IsField, IsDenseList ],
function( type_str, F, entries )
  local type, space, v;
  v := rec( type := type_str,
            field := F,
            entries := entries );
  if type_str = "row" then
    type := NewType( FamilyOfRowVectors,
                     IsRowVector and IsQPAVector and IsVectorRep );
    space := RowVectorSpace( F, Length( entries ) );
  elif type_str = "col" then
    type := NewType( FamilyOfColVectors,
                     IsColVector and IsQPAVector and IsVectorRep );
    space := ColVectorSpace( F, Length( entries ) );
  else
    Error( "type must be \"row\" or \"col\"" );
  fi;
  ObjectifyWithAttributes( v, type,
                           SpaceContainingVector, space );
  return v;
end );

InstallMethod( RowVector, [ IsField, IsDenseList ],
function( F, entries )
  return MakeQPAVector( "row", F, entries );
end );

InstallMethod( ColVector, [ IsField, IsDenseList ],
function( F, entries )
  return MakeQPAVector( "col", F, entries );
end );

InstallMethod( String, [ IsQPAVector ],
function( v )
  return Concatenation( "<", v!.type, " vector ", String( v!.entries ), ">" );
end );

InstallMethod( ViewObj, [ IsQPAVector ],
function( v )
  Print( String( v ) );
end );

InstallMethod( \[\], [ IsQPAVector, IsPosInt ],
function( v, i )
  return v!.entries[ i ];
end );

InstallMethod( Length, [ IsQPAVector ],
function( v )
  return Length( v!.entries );
end );

InstallMethod( \+, [ IsQPAVector, IsQPAVector ],
function( v1, v2 )
  if SpaceContainingVector( v1 ) <> SpaceContainingVector( v2 ) then
    Error( "vectors from different vector spaces" );
  fi;
  return MakeQPAVector( v1!.type, v1!.field, v1!.entries + v2!.entries );
end );

InstallMethod( \*, [ IsMultiplicativeElement, IsQPAVector ],
function( a, v )
  if not a in v!.field then
    Error( "scalar not from appropriate field" );
  fi;
  return MakeQPAVector( v!.type, v!.field, v!.entries );
end );

InstallMethod( \in, [ IsQPAVector, IsQPAVectorSpace ],
function( v, V )
  return SpaceContainingVector( v ) = V;
end );

InstallMethod( \=, [ IsQPAVector, IsQPAVector ],
function( v1, v2 )
  return SpaceContainingVector( v1 ) = SpaceContainingVector( v2 ) and
         v1!.entries = v2!.entries;
end );

InstallMethod( MakeQPAVectorSpace, [ IsString, IsField, IsInt ],
function( type_str, F, dim )
  local type, space, cat;
  space := rec( type := type_str );
  if type_str = "row" then
    type := NewType( FamilyOfRowVectorSpaces,
                     IsRowVectorSpace and IsQPAVectorSpace and IsVectorSpaceRep );
    cat := CategoryOfRowSpaces( F );
  elif type_str = "col" then
    type := NewType( FamilyOfColVectorSpaces,
                     IsColVectorSpace and IsQPAVectorSpace and IsVectorSpaceRep );
    cat := CategoryOfColSpaces( F );
  else
    Error( "type must be \"row\" or \"col\"" );
  fi;
  ObjectifyWithAttributes( space, type,
                           LeftActingDomain, F,
                           Dimension, dim );
  Add( cat, space );
  return space;
end );

InstallMethod( RowVectorSpace, [ IsField, IsInt ],
function( F, dim )
  return MakeQPAVectorSpace( "row", F, dim );
end );

InstallMethod( ColVectorSpace, [ IsField, IsInt ],
function( F, dim )
  return MakeQPAVectorSpace( "col", F, dim );
end );

InstallMethod( \=, [ IsQPAVectorSpace, IsQPAVectorSpace ],
function( V1, V2 )
  return V1!.type = V2!.type and
         LeftActingDomain( V1 ) = LeftActingDomain( V2 ) and
         Dimension( V1 ) = Dimension( V2 );
end );

InstallMethod( Zero, [ IsQPAVectorSpace ],
function( V )
  local F;
  F := LeftActingDomain( V );
  return MakeQPAVector( V!.type, F,
                        List( [ 1 .. Dimension( V ) ], i -> Zero( F ) ) );
end );

InstallMethod( CanonicalBasis, [ IsQPAVectorSpace ],
function( V )
  local basis, basis_vectors;
  if Dimension( V ) = 0 then
    basis_vectors := [];
  else
    basis_vectors := List( IdentityMat( Dimension( V ) ),
                           v -> MakeQPAVector( V!.type, LeftActingDomain( V ), v ) );
  fi;
  basis := rec();
  ObjectifyWithAttributes( basis,
                           NewType( FamilyOfVectorSpaceBases,
                                    IsBasis and IsVectorSpaceBasisRep ),
                           BasisVectors, basis_vectors,
                           UnderlyingLeftModule, V );
  return basis;
end );

InstallMethod( Basis, [ IsQPAVectorSpace ], CanonicalBasis );

InstallMethod( Coefficients,
               [ IsBasis and IsVectorSpaceBasisRep,
                 IsQPAVector ],
function( B, v )
  return v!.entries;
end );

InstallMethod( MakeLinearTransformation, [ IsString, IsField, IsMatrix ],
function( type_str, F, matrix )
  local type, cat, mat, dims, source_dim, range_dim;
  mat := rec( type := type_str,
              field := F );
  dims := DimensionsMat( matrix );
  if type_str = "row" then
    type := NewType( FamilyOfLinearTransformations,
                     IsLinearTransformationOfRowSpaces and IsLinearTransformationRep );
    cat := CategoryOfRowSpaces( F );
    source_dim := dims[ 1 ];
    range_dim := dims[ 2 ];
  elif type_str = "col" then
    type := NewType( FamilyOfLinearTransformations,
                     IsLinearTransformationOfColSpaces and IsLinearTransformationRep );
    cat := CategoryOfColSpaces( F );
    source_dim := dims[ 2 ];
    range_dim := dims[ 1 ];
  else
    Error( "type must be \"row\" or \"col\"" );
  fi;
  ObjectifyWithAttributes( mat, type,
                           Source, MakeQPAVectorSpace( type_str, F, source_dim ),
                           Range, MakeQPAVectorSpace( type_str, F, range_dim ),
                           MatrixOfLinearTransformation, matrix,
                           IsMapping, true );
  Add( cat, mat );
  return mat;
end );

InstallMethod( LinearTransformationOfRowSpaces, [ IsField, IsMatrix ],
function( F, matrix )
  return MakeLinearTransformation( "row", F, matrix );
end );

InstallMethod( LinearTransformationOfColSpaces, [ IsField, IsMatrix ],
function( F, matrix )
  return MakeLinearTransformation( "col", F, matrix );
end );

# InstallMethod( MatElm, [ IsRowMatrix, IsPosInt, IsPosInt ],
# function( m, i, j )
#   return m!.matrix[ i ][ j ];
# end );

# InstallMethod( MatElm, [ IsColMatrix, IsPosInt, IsPosInt ],
# function( m, i, j )
#   return m!.matrix[ j ][ i ];
# end );

InstallMethod( String, [ IsLinearTransformation ],
function( m )
  return Concatenation( "linear transformation of ", m!.type, " spaces: ",
                        String( Dimension( Source( m ) ) ), "->",
                        String( Dimension( Range( m ) ) ) );
  # local mat, sep, dims, rows, entry_len;
  # mat := MatrixOfLinearTransformation( m );
  # if m!.type = "row" then
  #   sep := "_";
  # else
  #   sep := "|";
  # fi;
  # dims := DimensionsMat( mat );
  # if dims[ 1 ] = 0 or dims[ 2 ] = 0 then
  #   return Concatenation( "<", JoinStringsWithSeparator( dims, "x" ), " ",
  #                         m!.type, " matrix>" );
  # fi;
  # entry_len := Maximum( List( m!.matrix,
  #                             r -> Maximum( List( r, x -> Length( String( x ) ) ) ) ) );
  # rows := List( [ 1 .. dims[ 1 ] ],
  #               i -> Concatenation
  #                    ( sep,
  #                      JoinStringsWithSeparator
  #                      ( List( [ 1 .. dims[ 2 ] ],
  #                              j -> String( MatElm( m, i, j ),
  #                                           entry_len ) ),
  #                        sep ),
  #                      sep ) );
  # return JoinStringsWithSeparator( rows, "\n" );
end );

InstallMethod( ViewObj, [ IsLinearTransformation ],
function( m )
  Print( String( m ) );
end );

InstallMethod( \+, [ IsLinearTransformation, IsLinearTransformation ],
function( m1, m2 )
  local mat1, mat2;
  if m1!.type <> m2!.type then
    Error( "cannot add linear transformations of different types" );
  elif m1!.field <> m2!.field then
    Error( "cannot add linear transformations over different fields" );
  elif Source( m1 ) <> Source( m2 ) then
    Error( "cannot add linear transformations with different source spaces" );
  elif Range( m1 ) <> Range( m2 ) then
    Error( "cannot add linear transformations with different range spaces" );
  fi;
  mat1 := MatrixOfLinearTransformation( m1 );
  mat2 := MatrixOfLinearTransformation( m2 );
  return MakeLinearTransformation( m1!.type, m1!.field, mat1 + mat2 );
end );

InstallMethod( ImageElm, [ IsLinearTransformationOfRowSpaces, IsQPAVector and IsRowVector ],
function( m, v )
  if not v in Source( m ) then
    Error( "vector not in source vector space" );
  fi;
  return MakeQPAVector( v!.type, v!.field,
                        v!.entries * MatrixOfLinearTransformation( m ) );
end );

InstallMethod( ImageElm, [ IsLinearTransformationOfColSpaces, IsQPAVector and IsColVector ],
function( m, v )
  if not v in Source( m ) then
    Error( "vector not in source vector space" );
  fi;
  return MakeQPAVector( v!.type, v!.field,
                        MatrixOfLinearTransformation( m ) * v!.entries );
end );

InstallMethod( VectorSpaceInCategory, [ IsVectorSpaceCategory, IsInt ],
function( cat, dim )
  return VectorSpaceConstructor( cat )( dim );
end );

InstallMethod( LinearTransformationInCategory, [ IsVectorSpaceCategory, IsMatrix ],
function( cat, matrix )
  return LinearTransformationConstructor( cat )( matrix );
end );

InstallMethod( MakeQPAVectorSpaceCategory, [ IsString, IsField ],
function( type_str, F )
  local cat, make_vecspace, make_morphism,
        equal_objects, equal_morphisms,
        zero_object, zero_morphism, is_zero_morphism, identity_morphism,
        pre_compose, addition, additive_inverse,
        kernel_emb, kernel_emb_mat, coker_proj, coker_proj_mat,
        preimage, mono_lift, epi_colift,
        direct_sum, direct_sum_morphism_1, direct_sum_morphism_2;

  if type_str <> "row" and type_str <> "col" then
    Error( "type must be \"row\" or \"col\"" );
  fi;

  make_vecspace := dim -> MakeQPAVectorSpace( type_str, F, dim );
  make_morphism := mat -> MakeLinearTransformation( type_str, F, mat );

  cat := CreateCapCategory( Concatenation( type_str, " spaces over ", String( F ) ) );
  SetFilterObj( cat, IsVectorSpaceCategory );
  SetVectorSpaceConstructor( cat, make_vecspace );
  SetLinearTransformationConstructor( cat, make_morphism );

  SetIsAbelianCategory( cat, true );

  equal_objects := function( V1, V2 )
    return Dimension( V1 ) = Dimension( V2 );
  end;
  AddIsEqualForObjects( cat, equal_objects );
                        
  equal_morphisms := function( m1, m2 )
    return MatrixOfLinearTransformation( m1 ) = MatrixOfLinearTransformation( m2 );
  end;
  AddIsEqualForMorphisms( cat, equal_morphisms );

  zero_object := function()
    return make_vecspace( 0 );
  end;
  AddZeroObject( cat, zero_object );

  if type_str = "row" then
    zero_morphism := function( V1, V2 )
      return make_morphism( MakeZeroMatrix( Dimension( V1 ), Dimension( V2 ), F ) );
    end;
  else
    zero_morphism := function( V1, V2 )
      return make_morphism( MakeZeroMatrix( Dimension( V2 ), Dimension( V1 ), F ) );
    end;
  fi;
  AddZeroMorphism( cat, zero_morphism );

  is_zero_morphism := function( m )
    return IsZero( MatrixOfLinearTransformation( m ) );
  end;
  AddIsZeroForMorphisms( cat, is_zero_morphism );

  identity_morphism := function( V )
    return make_morphism( IdentityMatrix( Dimension( V ), F ) );
  end;
  AddIdentityMorphism( cat, identity_morphism );

  if type_str = "row" then
    pre_compose := function( m1, m2 )
      return make_morphism( MatrixOfLinearTransformation( m1 ) *
                            MatrixOfLinearTransformation( m2 ) );
    end;
  else
    pre_compose := function( m1, m2 )
      return make_morphism( MatrixOfLinearTransformation( m2 ) *
                            MatrixOfLinearTransformation( m1 ) );
    end;
  fi;
  AddPreCompose( cat, pre_compose );

  addition := function( m1, m2 )
    return make_morphism( MatrixOfLinearTransformation( m1 ) +
                          MatrixOfLinearTransformation( m2 ) );
  end;
  AddAdditionForMorphisms( cat, addition );

  additive_inverse := function( m )
    return make_morphism( - MatrixOfLinearTransformation( m ) );
  end;
  AddAdditiveInverseForMorphisms( cat, additive_inverse );

  if type_str = "row" then
    kernel_emb_mat := TriangulizedNullspaceMat;
    coker_proj_mat := m -> TransposedMat( TriangulizedNullspaceMat( TransposedMat( m ) ) );
  else
    kernel_emb_mat := m -> TransposedMat( TriangulizedNullspaceMat( TransposedMat( m ) ) );
    coker_proj_mat := TriangulizedNullspaceMat;
  fi;

  kernel_emb := function( m )
    local kernel_mat;
    kernel_mat := kernel_emb_mat( MatrixOfLinearTransformation( m ) );
    if kernel_mat = [] then
      return zero_morphism( zero_object(), Source( m ) );
    else
      return make_morphism( kernel_mat );
    fi;
  end;
  AddKernelEmbedding( cat, kernel_emb );

  coker_proj := function( m )
    local coker_mat;
    coker_mat := coker_proj_mat( MatrixOfLinearTransformation( m ) );
    if coker_mat = [] then
      return zero_morphism( Range( m ), zero_object() );
    else
      return make_morphism( coker_mat );
    fi;
  end;
  AddCokernelProjection( cat, coker_proj );

  if type_str = "row" then
    preimage := function( m, v )
      return SolutionMat( MatrixOfLinearTransformation( m ), v!.entries );
    end;
  else
    preimage := function( m, v )
      return SolutionMat( TransposedMat( MatrixOfLinearTransformation( m ) ), v!.entries );
    end;
  fi;

  mono_lift := function( i, test )
    local matrix;
    if IsZero( Source( i ) ) or IsZero( Source( test ) ) then
      return zero_morphism( Source( test ), Source( i ) );
    fi;
    matrix := List( Basis( Source( test ) ),
                    v -> preimage( i, ImageElm( test, v ) ) );
    if type_str = "col" then
      matrix := TransposedMat( matrix );
    fi;
    return make_morphism( matrix );
  end;
  AddLiftAlongMonomorphism( cat, mono_lift );

  epi_colift := function( e, test )
    local matrix;
    if IsZero( Range( e ) ) or IsZero( Range( test ) ) then
      return zero_morphism( Range( e ), Range( test ) );
    fi;
    matrix := List( Basis( Range( e ) ),
                    v -> ImageElm( test, preimage( e, v ) ) );
    if type_str = "col" then
      matrix := TransposedMat( matrix );
    fi;
    return make_morphism( matrix );
  end;
  AddColiftAlongEpimorphism( cat, epi_colift );

  direct_sum := function( summands )
    return make_vecspace( Sum( List( summands, Dimension ) ) );
  end;
  AddDirectSum( cat, direct_sum );

  direct_sum_morphism_1 := function( summands, i, sum )
    local n, summands_before, summands_after, dim_before, dim_after, dim_i,
          m1, m2, m3, matrix;
    n := Length( summands );
    summands_before := summands{ [ 1 .. ( i - 1 ) ] };
    summands_after := summands{ [ ( i + 1 ) .. n ] };
    dim_before := Sum( List( summands_before, Dimension ) );
    dim_after := Sum( List( summands_after, Dimension ) );
    dim_i := Dimension( summands[ i ] );
    m1 := MakeZeroMatrix( dim_i, dim_before, F );
    m2 := IdentityMatrix( dim_i, F );
    m3 := MakeZeroMatrix( dim_i, dim_after, F );
    matrix := StackMatricesHorizontally( [ m1, m2, m3 ] );
    return make_morphism( matrix );
  end;

  direct_sum_morphism_2 := function( summands, i, sum )
    local n, summands_before, summands_after, dim_before, dim_after, dim_i,
          m1, m2, m3, matrix;
    n := Length( summands );
    summands_before := summands{ [ 1 .. ( i - 1 ) ] };
    summands_after := summands{ [ ( i + 1 ) .. n ] };
    dim_before := Sum( List( summands_before, Dimension ) );
    dim_after := Sum( List( summands_after, Dimension ) );
    dim_i := Dimension( summands[ i ] );
    m1 := MakeZeroMatrix( dim_before, dim_i, F );
    m2 := IdentityMatrix( dim_i, F );
    m3 := MakeZeroMatrix( dim_after, dim_i, F );
    matrix := StackMatricesVertically( [ m1, m2, m3 ] );
    return make_morphism( matrix );
  end;

  if type_str = "row" then
    AddInjectionOfCofactorOfDirectSumWithGivenDirectSum( cat, direct_sum_morphism_1 );
    AddProjectionInFactorOfDirectSumWithGivenDirectSum( cat, direct_sum_morphism_2 );
  else
    AddInjectionOfCofactorOfDirectSumWithGivenDirectSum( cat, direct_sum_morphism_2 );
    AddProjectionInFactorOfDirectSumWithGivenDirectSum( cat, direct_sum_morphism_1 );
  fi;
  
  Finalize( cat );
  DeactivateCachingOfCategory( cat );

  return cat;
end );

InstallMethod( CategoryOfRowSpaces, [ IsField ],
               F -> MakeQPAVectorSpaceCategory( "row", F ) );

InstallMethod( CategoryOfColSpaces, [ IsField ],
               F -> MakeQPAVectorSpaceCategory( "col", F ) );

InstallMethod( StackMatricesHorizontally, [ IsDenseList ],
function( matrices )
  return Iterated( matrices, StackMatricesHorizontally );
end );

InstallMethod( StackMatricesHorizontally, [ IsMatrix, IsMatrix ],
function( m1, m2 )
  local dim1, dim2;
  dim1 := DimensionsMat( m1 );
  dim2 := DimensionsMat( m2 );
  if dim1[ 1 ] <> dim2[ 1 ] then
    Error( "matrices of different height: ", dim1[ 1 ], " and ", dim2[ 1 ] );
  fi;
  if IsEmptyMatrix( m1 ) then
    return m2;
  elif IsEmptyMatrix( m2 ) then
    return m1;
  fi;
  return ListN( m1, m2, Concatenation );
end );

InstallMethod( StackMatricesVertically, [ IsDenseList ],
function( matrices )
  return Iterated( matrices, StackMatricesVertically );
end );

InstallMethod( StackMatricesVertically, [ IsMatrix, IsMatrix ],
function( m1, m2 )
  local dim1, dim2;
  dim1 := DimensionsMat( m1 );
  dim2 := DimensionsMat( m2 );
  if dim1[ 2 ] <> dim2[ 2 ] then
    Error( "matrices of different width: ", dim1[ 2 ], " and ", dim2[ 2 ] );
  fi;
  if IsEmptyMatrix( m1 ) then
    return m2;
  elif IsEmptyMatrix( m2 ) then
    return m1;
  fi;
  return Concatenation( m1, m2 );
end );
