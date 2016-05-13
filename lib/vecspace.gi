DeclareRepresentation( "IsQPAVectorSpaceRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [ ] );
DeclareRepresentation( "IsVectorSpaceBasisRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [ ] );
DeclareRepresentation( "IsQPAVectorRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [ "type", "field", "entries" ] );
DeclareRepresentation( "IsLinearTransformationRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [ ] );

BindGlobal( "FamilyOfQPAVectors",
            NewFamily( "vectors" ) );
BindGlobal( "FamilyOfQPAVectorSpaces",
            CollectionsFamily( FamilyOfQPAVectors ) );
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
    type := NewType( FamilyOfQPAVectors,
                     IsQPARowVector and IsQPAVectorRep );
    space := RowVectorSpace( F, Length( entries ) );
  elif type_str = "col" then
    type := NewType( FamilyOfQPAVectors,
                     IsQPAColVector and IsQPAVectorRep );
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

InstallMethod( Vector, [ IsQPAVectorSpace, IsList ],
function( V, coeffs )
  return LinearCombination( coeffs, CanonicalBasis( V ) );
end );

InstallMethod( Vector, [ IsQPAVectorSpace, IsQPARowVector ],
function( V, row_vec )
  return Vector( V, AsList( row_vec ) );
end );

InstallMethod( Vector, [ IsQPAVectorSpace, IsQPAColVector ],
function( V, col_vec )
  return Vector( V, AsList( col_vec ) );
end );

InstallMethod( AsRowVector, [ IsQPAVector ],
function( v )
  return RowVector( LeftActingDomain( SpaceContainingVector( v ) ),
                    AsList( v ) );
end );

InstallMethod( AsColVector, [ IsQPAVector ],
function( v )
  return ColVector( LeftActingDomain( SpaceContainingVector( v ) ),
                    AsList( v ) );
end );

InstallMethod( AsList, [ IsQPAVector ],
function( v )
  return Coefficients( CanonicalBasis( SpaceContainingVector( v ) ), v );
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

InstallMethod( AdditiveInverseMutable, [ IsQPAVector ],
function( v )
  return MakeQPAVector( v!.type, v!.field, - v!.entries );
end );

InstallMethod( ZeroMutable, [ IsQPAVector ],
function( v )
  return Zero( SpaceContainingVector( v ) );
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

InstallMethod( ZeroVectorSpace, [ IsField ],
function( F )
  local space, type, cat;
  space := rec();
  type := NewType( FamilyOfQPAVectorSpaces,
                   IsZeroVectorSpace and IsQPAVectorSpaceRep );
  cat := CategoryOfVectorSpaces( F );
  ObjectifyWithAttributes( space, type,
                           LeftActingDomain, F,
                           Dimension, 0 );
  Add( cat, space );
  return space;
end );

InstallMethod( MakeQPAVectorSpace, [ IsString, IsField, IsInt ],
function( type_str, F, dim )
  local type, space, cat;
  if dim = 0 then
    return ZeroVectorSpace( F );
  fi;
  space := rec( type := type_str );
  if type_str = "row" then
    type := NewType( FamilyOfQPAVectorSpaces,
                     IsRowVectorSpace and IsQPAVectorSpaceRep );
    cat := CategoryOfVectorSpaces( F );
  elif type_str = "col" then
    type := NewType( FamilyOfQPAVectorSpaces,
                     IsColVectorSpace and IsQPAVectorSpaceRep );
    cat := CategoryOfVectorSpaces( F );
  else
    Error( "type must be \"row\" or \"col\"" );
  fi;
  ObjectifyWithAttributes( space, type,
                           LeftActingDomain, F,
                           Dimension, dim );
  Add( cat, space );
  return space;
end );

InstallMethod( MakeQPAVectorSpace, [ IsQPAVectorSpace, IsInt ],
function( prototype_space, dim )
  if IsRowVectorSpace( prototype_space ) then
    return MakeQPAVectorSpace( "row", LeftActingDomain( prototype_space ), dim );
  elif IsColVectorSpace( prototype_space ) then
    return MakeQPAVectorSpace( "col", LeftActingDomain( prototype_space ), dim );
  else
    Error( "not implemented" ); # TODO fix
  fi;
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

InstallMethod( MakeLinearTransformation, "for vector spaces and matrices",
               [ IsQPAVectorSpace, IsQPAVectorSpace, IsQPAMatrix, IsQPAMatrix ],
function( source, range, left_matrix, right_matrix )
  local T, type, cat, dim_source, dim_range, i, j;
  cat := CapCategory( source );
  if cat <> CapCategory( range ) then
    Error( "vector spaces in different categories" );
  fi;
  dim_source := Dimension( source );
  dim_range := Dimension( range );
  if DimensionsMat( left_matrix ) <> [ dim_range, dim_source ] then
    Error( "wrong dimensions of left matrix" );
  elif DimensionsMat( right_matrix ) <> [ dim_source, dim_range ] then
    Error( "wrong dimensions of right matrix" );
  fi;
  for i in [ 1 .. dim_source ] do
    for j in [ 1 .. dim_range ] do
      if MatElm( right_matrix, i, j ) <> MatElm( left_matrix, j, i ) then
        Error( "left and right matrix not equivalent" );
      fi;
    od;
  od;
  T := rec();
  type := NewType( FamilyOfLinearTransformations,
                   IsLinearTransformation and IsLinearTransformationRep );
  ObjectifyWithAttributes
    ( T, type,
      Source, source,
      Range, range,
      LeftMatrixOfLinearTransformation, left_matrix,
      RightMatrixOfLinearTransformation, right_matrix );
  Add( cat, T );
  return T;
end );

InstallMethod( LinearTransformationByLeftMatrix, "for vector spaces and matrix",
               [ IsQPAVectorSpace, IsQPAVectorSpace, IsQPAMatrix ],
function( source, range, left_matrix )
  return MakeLinearTransformation( source, range, left_matrix, TransposedMat( left_matrix ) );
end );

InstallMethod( LinearTransformationByRightMatrix, "for vector spaces and matrix",
               [ IsQPAVectorSpace, IsQPAVectorSpace, IsQPAMatrix ],
function( source, range, right_matrix )
  return MakeLinearTransformation( source, range, TransposedMat( right_matrix ), right_matrix );
end );

InstallMethod( LinearTransformationOfRowSpaces, "for matrix",
               [ IsQPAMatrix ],
function( matrix )
  local d, F;
  d := DimensionsMat( matrix );
  F := BaseDomain( matrix );
  return LinearTransformationByRightMatrix
         ( RowVectorSpace( F, d[ 1 ] ),
           RowVectorSpace( F, d[ 2 ] ),
           matrix );
end );

InstallMethod( LinearTransformationOfColSpaces, "for matrix",
               [ IsQPAMatrix ],
function( matrix )
  local d, F;
  d := DimensionsMat( matrix );
  F := BaseDomain( matrix );
  return LinearTransformationByLeftMatrix
         ( ColVectorSpace( F, d[ 2 ] ),
           ColVectorSpace( F, d[ 1 ] ),
           matrix );
end );

InstallMethod( MatrixOfLinearTransformation, "for linear transformation",
               [ IsLinearTransformation ],
function( T )
  local   is_rowspaceorzero,  is_colspaceorzero;

  is_rowspaceorzero := V -> IsRowVectorSpace( V ) or IsZero( V );
  is_colspaceorzero := V -> IsColVectorSpace( V ) or IsZero( V );
  if is_rowspaceorzero( Source( T ) ) and is_rowspaceorzero( Range( T ) ) then
    return RightMatrixOfLinearTransformation( T );
  elif is_colspaceorzero( Source( T ) ) and is_colspaceorzero( Range( T ) ) then
    return LeftMatrixOfLinearTransformation( T );
  else
    Error( "MatrixOfLinearTransformation undefined for ", T );
  fi;
end );

InstallMethod( String, [ IsLinearTransformation ],
function( m )
  return Concatenation( "linear transformation ",
                        String( Dimension( Source( m ) ) ), "->",
                        String( Dimension( Range( m ) ) ) );
end );

InstallMethod( ViewObj, [ IsLinearTransformation ],
function( m )
  Print( String( m ) );
end );

InstallMethod( \+, [ IsLinearTransformation, IsLinearTransformation ],
function( m1, m2 )
  local mat1, mat2, lm1, lm2;
  if Source( m1 ) <> Source( m2 ) then
    Error( "cannot add linear transformations with different source spaces" );
  elif Range( m1 ) <> Range( m2 ) then
    Error( "cannot add linear transformations with different range spaces" );
  fi;
  lm1 := LeftMatrixOfLinearTransformation( m1 );
  lm2 := LeftMatrixOfLinearTransformation( m2 );
  return LinearTransformationByLeftMatrix( Source( m1 ), Range( m1 ),
                                           lm1 + lm2 );
end );

InstallMethod( ImageElm, [ IsLinearTransformation, IsQPAVector ],
function( T, v )
  if not v in Source( T ) then
    Error( "vector not in source vector space" );
  fi;
  return Vector( Range( T ),
                 AsRowVector( v ) *
                 RightMatrixOfLinearTransformation( T ) );
  # return LinearCombination( CanonicalBasis( Range( T ) ),
  #                           Coefficients( CanonicalBasis( Source( T ) ), v )
end );

InstallMethod( PreImagesRepresentative, [ IsLinearTransformation, IsQPAVector ],
function( T, v )
  local x;
  x := SolutionMat( RowsOfMatrix( RightMatrixOfLinearTransformation( T ) ),
                    AsList( v ) );
  if x = fail then return fail; fi;
  return Vector( Source( T ), x );
end );

InstallMethod( CategoryOfVectorSpaces, [ IsField ],
function( F )
  local cat, equal_objects, equal_morphisms, zero_object,
        zero_morphism, is_zero_morphism, identity_morphism,
        pre_compose, addition, additive_inverse, kernel_emb,
        coker_proj, mono_lift, epi_colift, direct_sum,
        injection_direct_sum,  projection_direct_sum;

  cat := CreateCapCategory( Concatenation( "vector spaces over ", String( F ) ) );
  SetFilterObj( cat, IsVectorSpaceCategory );

  SetIsAbelianCategory( cat, true );

  equal_objects := function( V1, V2 )
    if IsRowVectorSpace( V1 ) or IsRowVectorSpace( V2 ) or
       IsColVectorSpace( V1 ) or IsColVectorSpace( V2 ) then
      return ( ( IsRowVectorSpace( V1 ) and IsRowVectorSpace( V2 ) )
               or
               ( IsColVectorSpace( V1 ) and IsColVectorSpace( V2 ) ) )
             and
             ( Dimension( V1 ) = Dimension( V2 ) );
    else
      return false; # TODO fix
    fi;
  end;
  AddIsEqualForObjects( cat, equal_objects );
                        
  equal_morphisms := function( m1, m2 )
    return RightMatrixOfLinearTransformation( m1 )
           = RightMatrixOfLinearTransformation( m2 );
  end;
  AddIsEqualForMorphisms( cat, equal_morphisms );

  zero_object := function()
    return ZeroVectorSpace( F );
  end;
  AddZeroObject( cat, zero_object );

  zero_morphism := function( V1, V2 )
    return LinearTransformationByRightMatrix
           ( V1, V2, MakeZeroMatrix( F, Dimension( V1 ), Dimension( V2 ) ) );
  end;
  AddZeroMorphism( cat, zero_morphism );

  is_zero_morphism := function( m )
    return IsZero( RightMatrixOfLinearTransformation( m ) );
  end;
  AddIsZeroForMorphisms( cat, is_zero_morphism );

  identity_morphism := function( V )
    return LinearTransformationByRightMatrix
           ( V, V, IdentityMatrix( F, Dimension( V ) ) );
  end;
  AddIdentityMorphism( cat, identity_morphism );

  pre_compose := function( m1, m2 )
    return LinearTransformationByRightMatrix
           ( Source( m1 ), Range( m2 ),
             RightMatrixOfLinearTransformation( m1 ) *
             RightMatrixOfLinearTransformation( m2 ) );
  end;
  AddPreCompose( cat, pre_compose );

  addition := function( m1, m2 )
    return LinearTransformationByRightMatrix
           ( Source( m1 ), Range( m1 ),
             RightMatrixOfLinearTransformation( m1 ) +
             RightMatrixOfLinearTransformation( m2 ) );
  end;
  AddAdditionForMorphisms( cat, addition );

  additive_inverse := function( m )
    return LinearTransformationByRightMatrix
           ( Source( m ), Range( m ), - RightMatrixOfLinearTransformation( m ) );
  end;
  AddAdditiveInverseForMorphisms( cat, additive_inverse );

  kernel_emb := function( m )
    local kernel_mat, dim;
    kernel_mat := TriangulizedNullspaceMat( RowsOfMatrix( RightMatrixOfLinearTransformation( m ) ) );
    if kernel_mat = [] then
      return zero_morphism( zero_object(), Source( m ) );
    else
      dim := DimensionsMat( kernel_mat )[ 1 ];
      return LinearTransformationByRightMatrix
             ( MakeQPAVectorSpace( Source( m ), dim ), Source( m ),
               MatrixByRows( F, kernel_mat ) );
    fi;
  end;
  AddKernelEmbedding( cat, kernel_emb );

  coker_proj := function( m )
    local mat, coker_mat, dim;
    mat := RowsOfMatrix( RightMatrixOfLinearTransformation( m ) );
    coker_mat := TransposedMat( TriangulizedNullspaceMat( TransposedMat( m ) ) );
    if coker_mat = [] then
      return zero_morphism( Range( m ), zero_object() );
    else
      dim := DimensionsMat( coker_mat )[ 2 ];
      return LinearTransformationByRightMatrix
             ( Range( m ), MakeQPAVectorSpace( Range( m ), dim ),
               MatrixByRows( F, coker_mat ) );
    fi;
  end;
  AddCokernelProjection( cat, coker_proj );

  mono_lift := function( i, test )
    local matrix;
    if IsZero( Source( i ) ) or IsZero( Source( test ) ) then
      return zero_morphism( Source( test ), Source( i ) );
    fi;
    matrix := List( Basis( Source( test ) ),
                    v -> AsList( PreImagesRepresentative( i, ImageElm( test, v ) ) ) );
    return LinearTransformationByRightMatrix
           ( Source( test ), Source( i ),
             MatrixByRows( F, matrix ) );
  end;
  AddLiftAlongMonomorphism( cat, mono_lift );

  epi_colift := function( e, test )
    local matrix;
    if IsZero( Range( e ) ) or IsZero( Range( test ) ) then
      return zero_morphism( Range( e ), Range( test ) );
    fi;
    matrix := List( Basis( Range( e ) ),
                    v -> AsList( ImageElm( test,
                                           PreImagesRepresentative( e, v ) ) ) );
    return LinearTransformationByRightMatrix
           ( Range( e ), Range( test ),
             MatrixByRows( F, matrix ) );
  end;
  AddColiftAlongEpimorphism( cat, epi_colift );

  direct_sum := function( summands )
    return MakeQPAVectorSpace( summands[ 1 ], # TODO look at all summands?
                               Sum( List( summands, Dimension ) ) );
  end;
  AddDirectSum( cat, direct_sum );

  injection_direct_sum := function( summands, i, sum )
    local n, summands_before, summands_after, dim_before, dim_after, dim_i,
          m1, m2, m3, matrix;
    n := Length( summands );
    summands_before := summands{ [ 1 .. ( i - 1 ) ] };
    summands_after := summands{ [ ( i + 1 ) .. n ] };
    dim_before := Sum( List( summands_before, Dimension ) );
    dim_after := Sum( List( summands_after, Dimension ) );
    dim_i := Dimension( summands[ i ] );
    m1 := MakeZeroMatrix( F, dim_i, dim_before );
    m2 := IdentityMatrix( F, dim_i );
    m3 := MakeZeroMatrix( F, dim_i, dim_after );
    matrix := StackMatricesHorizontally( [ m1, m2, m3 ] );
    return LinearTransformationByRightMatrix
           ( summands[ i ], sum, matrix );
  end;
  AddInjectionOfCofactorOfDirectSumWithGivenDirectSum( cat, injection_direct_sum );

  projection_direct_sum := function( summands, i, sum )
    local n, summands_before, summands_after, dim_before, dim_after, dim_i,
          m1, m2, m3, matrix;
    n := Length( summands );
    summands_before := summands{ [ 1 .. ( i - 1 ) ] };
    summands_after := summands{ [ ( i + 1 ) .. n ] };
    dim_before := Sum( List( summands_before, Dimension ) );
    dim_after := Sum( List( summands_after, Dimension ) );
    dim_i := Dimension( summands[ i ] );
    m1 := MakeZeroMatrix( F, dim_before, dim_i );
    m2 := IdentityMatrix( F, dim_i );
    m3 := MakeZeroMatrix( F, dim_after, dim_i );
    matrix := StackMatricesVertically( [ m1, m2, m3 ] );
    return LinearTransformationByRightMatrix
           ( sum, summands[ i ], matrix );
  end;
  AddProjectionInFactorOfDirectSumWithGivenDirectSum( cat, projection_direct_sum );
  
  Finalize( cat );
  DeactivateCachingOfCategory( cat );

  return cat;
end );

InstallMethod( StackMatricesHorizontally, [ IsDenseList ],
function( matrices )
  return Iterated( matrices, StackMatricesHorizontally );
end );

InstallMethod( StackMatricesHorizontally, [ IsQPAMatrix, IsQPAMatrix ],
function( m1, m2 )
  local dim1, dim2, F, rows;
  dim1 := DimensionsMat( m1 );
  dim2 := DimensionsMat( m2 );
  if dim1[ 1 ] <> dim2[ 1 ] then
    Error( "matrices of different height: ", dim1[ 1 ], " and ", dim2[ 1 ] );
  fi;
  F := BaseDomain( m1 );
  if F <> BaseDomain( m2 ) then
    Error( "matrices over different rings" );
  fi;
  if dim1[ 2 ] = 0 then
    return m2;
  elif dim2[ 2 ] = 0 then
    return m1;
  fi;
  rows := ListN( RowsOfMatrix( m1 ), RowsOfMatrix( m2 ), Concatenation );
  return MatrixByRows( F, rows );
end );

InstallMethod( StackMatricesVertically, [ IsDenseList ],
function( matrices )
  return Iterated( matrices, StackMatricesVertically );
end );

InstallMethod( StackMatricesVertically, [ IsQPAMatrix, IsQPAMatrix ],
function( m1, m2 )
  local dim1, dim2, F, rows;
  dim1 := DimensionsMat( m1 );
  dim2 := DimensionsMat( m2 );
  if dim1[ 2 ] <> dim2[ 2 ] then
    Error( "matrices of different width: ", dim1[ 2 ], " and ", dim2[ 2 ] );
  fi;
  F := BaseDomain( m1 );
  if F <> BaseDomain( m2 ) then
    Error( "matrices over different rings" );
  fi;
  if dim1[ 1 ] = 0 then
    return m2;
  elif dim2[ 1 ] = 0 then
    return m1;
  fi;
  rows := Concatenation( RowsOfMatrix( m1 ), RowsOfMatrix( m2 ) );
  return MatrixByRows( F, rows );
end );

