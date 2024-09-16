DeclareRepresentation( "IsQPAVectorSpaceRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [ ] );
DeclareRepresentation( "IsVectorSpaceBasisRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [ ] );
DeclareRepresentation( "IsQPAVectorRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [ ] );
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

InstallMethod( StandardVector, [ IsField, IsDenseList ],
function( F, entries )
  local space, type, v;
  if Length( entries ) = 0 then
    return EmptyVector( F );
  fi;
  space := StandardVectorSpace( F, Length( entries ) );
  type := NewType( FamilyOfQPAVectors, IsStandardVector and IsQPAVectorRep );
  v := rec();
  ObjectifyWithAttributes( v, type,
                           UnderlyingField, F,
                           SpaceContainingVector, space,
                           AsList, entries );
  return v;
end );

InstallMethod( Vector, [ IsStandardVectorSpace, IsDenseList ],
function( V, coeffs )
  return StandardVector( UnderlyingField( V ), coeffs );
end );

InstallMethod( Vector, [ IsQPAVectorSpace, IsDenseList ],
function( V, coeffs )
  return LinearCombination( CanonicalBasis( V ), coeffs );
end );

InstallMethod( Vector, [ IsQPAVectorSpace, IsQPAVector ],
function( V, vec )
  return Vector( V, AsList( vec ) );
end );

InstallMethod( Vector, [ IsZeroVectorSpace, IsList ],
function( V, coeffs )
  return Zero( V );
end );

InstallMethod( Vector, [ IsZeroVectorSpace, IsQPAVector ],
function( V, row_vec )
  return Zero( V );
end );

InstallMethod( AsList, [ IsQPAVector ],
function( v )
  return Coefficients( CanonicalBasis( SpaceContainingVector( v ) ), v );
end );

InstallMethod( AsStandardVector, [ IsQPAVector ],
function( v )
  return StandardVector( UnderlyingField( v ), AsList( v ) );
end );

InstallMethod( String, [ IsQPAVector ],
function( v )
  return Concatenation( "<vector ", String( AsList( v ) ), ">" );
end );

InstallMethod( String, [ IsEmptyVector ],
function( v )
  return Concatenation( "<empty vector over ",
                        String( UnderlyingField( v ) ),
                        ">" );
end );

InstallMethod( ViewObj, [ IsQPAVector ],
function( v )
  Print( String( v ) );
end );

InstallMethod( \[\], [ IsQPAVector, IsPosInt ],
function( v, i )
  return AsList( v )[ i ];
end );

InstallMethod( Length, [ IsQPAVector ],
function( v )
  return Length( AsList( v ) );
end );

InstallMethod( \+, [ IsQPAVector, IsQPAVector ],
function( v1, v2 )
  if SpaceContainingVector( v1 ) <> SpaceContainingVector( v2 ) then
    Error( "vectors from different vector spaces" );
  fi;
  return Vector( SpaceContainingVector( v1 ), AsList( v1 ) + AsList( v2 ) );
end );

InstallMethod( AdditiveInverseMutable, [ IsQPAVector ],
function( v )
  return Vector( SpaceContainingVector( v ), - AsList( v ) );
end );

InstallMethod( ZeroMutable, [ IsQPAVector ],
function( v )
  return Zero( SpaceContainingVector( v ) );
end );

InstallMethod( \*, [ IsMultiplicativeElement, IsQPAVector ],
function( a, v )
  if not a in UnderlyingField( v ) then
    Error( "scalar not from appropriate field" );
  fi;
  return Vector( SpaceContainingVector( v ), a * AsList( v ) );
end );

InstallMethod( \in, [ IsQPAVector, IsQPAVectorSpace ],
function( v, V )
  return SpaceContainingVector( v ) = V;
end );

InstallMethod( \=, [ IsQPAVector, IsQPAVector ],
function( v1, v2 )
  return SpaceContainingVector( v1 ) = SpaceContainingVector( v2 ) and
         AsList( v1 ) = AsList( v2 );
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

InstallMethod( EmptyVector, [ IsField ],
function( F )
  local v, type, V;
  V := ZeroVectorSpace( F );
  v := rec( );
  type := NewType( FamilyOfQPAVectors,
                   IsEmptyVector and IsQPAVectorRep );
  ObjectifyWithAttributes( v, type,
                           UnderlyingField, F,
                           SpaceContainingVector, V,
                           AsList, [],
                           IsZero, true );
  return v;
end );

InstallMethod( Zero, [ IsZeroVectorSpace ],
function( V )
  return EmptyVector( LeftActingDomain( V ) );
end );

InstallMethod( StandardVectorSpace, [ IsField, IsInt ],
function( F, dim )
  local type, space, cat;
  if dim = 0 then
    return ZeroVectorSpace( F );
  fi;
  type := NewType( FamilyOfQPAVectorSpaces,
                   IsStandardVectorSpace and IsQPAVectorSpaceRep );
  space := rec( );
  cat := CategoryOfVectorSpaces( F );
  ObjectifyWithAttributes( space, type,
                           LeftActingDomain, F,
                           Dimension, dim );
  Add( cat, space );
  return space;
end );

InstallMethod( \=, [ IsStandardVectorSpace, IsStandardVectorSpace ],
function( V1, V2 )
  return UnderlyingField( V1 ) = UnderlyingField( V2 ) and
         Dimension( V1 ) = Dimension( V2 );
end );

InstallMethod( \=,  [ IsQPAVectorSpace, IsQPAVectorSpace ],
function( V1, V2 )
  return false;
end );

InstallMethod( Zero, [ IsQPAVectorSpace ],
function( V )
  local F, dim;
  F := UnderlyingField( V );
  dim := Dimension( V );
  return Vector( V, List( [ 1 .. dim ], i -> Zero( F ) ) );
end );

InstallMethod( LeftActingDomain, [ IsQPAVectorSpace ],
               UnderlyingField );

InstallMethod( CanonicalBasis, [ IsStandardVectorSpace ],
function( V )
  local basis, basis_vectors;
  if Dimension( V ) = 0 then
    basis_vectors := [];
  else
    basis_vectors := List( IdentityMat( Dimension( V ), UnderlyingField( V ) ),
                           v -> StandardVector( UnderlyingField( V ), v ) );
  fi;
  basis := rec();
  ObjectifyWithAttributes( basis,
                           NewType( FamilyOfVectorSpaceBases,
                                    IsStandardVectorSpaceBasis and IsVectorSpaceBasisRep ),
                           BasisVectors, basis_vectors,
                           UnderlyingLeftModule, V,
                           IsCanonicalBasis, true );
  return basis;
end );

InstallMethod( Basis, [ IsQPAVectorSpace ], CanonicalBasis );

InstallMethod( Dimension, "for qpa vector space",
               [ IsQPAVectorSpace ],
               V -> Length( CanonicalBasis( V ) ) );

InstallMethod( Coefficients,
               [ IsCanonicalBasis,
                 IsQPAVector ],
function( B, v )
  local V;
  V := UnderlyingLeftModule( B );
  if not ( v in V ) then
    Error( "basis for wrong vector space" );
  fi;
  return AsList( v );
end );

DeclareDirectionOperations( LinearTransformation,
                            LinearTransformationByLeftMatrix,
                            LinearTransformationByRightMatrix );

DeclareDirectionOperations( MatrixOfLinearTransformation,
                            LeftMatrixOfLinearTransformation,
                            RightMatrixOfLinearTransformation );

InstallMethodWithDirections( LinearTransformation,
                             [ IsQPAVectorSpace, IsQPAVectorSpace, IsQPAMatrix ],
dir -> function( V1, V2, mat )
  local cat, mats, dim1, dim2, T, type;
  cat := CapCategory( V1 );
  if cat <> CapCategory( V2 ) then
    Error( "vector spaces in different categories" );
  fi;
  mats := [];
  mats[ Int( dir ) ] := mat;
  mats[ Int( Opposite( dir ) ) ] := TransposedMat( mat );
  dim1 := Dimension( V1 );
  dim2 := Dimension( V2 );
  if DimensionsMat( mats[ Int( RIGHT ) ] ) <> [ dim1, dim2 ] then
    Error( "wrong dimensions of matrix" );
  fi;
  T := rec();
  type := NewType( FamilyOfLinearTransformations,
                   IsLinearTransformation and IsLinearTransformationRep );
  ObjectifyWithAttributes
    ( T, type,
      Source, V1,
      Range, V2,
      LeftMatrixOfLinearTransformation, mats[ Int( LEFT ) ],
      RightMatrixOfLinearTransformation, mats[ Int( RIGHT ) ]
      );
  Add( cat, T );
  return T;
end );

InstallMethod( LinearTransformation, "for vector spaces and dense list",
               [ IsQPAVectorSpace, IsQPAVectorSpace, IsDenseList ],
function( V1, V2, list )
  local F, dim1, dim2, mat;
  F := UnderlyingField( V1 );
  dim1 := Dimension( V1 );
  dim2 := Dimension( V2 );
  mat := MatrixByRows( F, [ dim1, dim2 ], list );
  return LinearTransformation( RIGHT, V1, V2, mat );
end );

InstallMethodWithDirections( LinearTransformation,
                             [ IsQPAVectorSpace, IsQPAVectorSpace, IsDenseList ],
dir -> function( V1, V2, list )
  return LinearTransformation( V1, V2, list );
end );

InstallMethod( LinearTransformationByFunction, "for vector spaces and function",
               [ IsQPAVectorSpace, IsQPAVectorSpace, IsFunction ],
function( V1, V2, f )
  local mat, b;
  mat := [];
  for b in CanonicalBasis( V1 ) do
    Add( mat, AsList( f( b ) ) );
  od;
  return LinearTransformationByRightMatrix( V1, V2, mat );
end );

InstallMethod( SpaceContainingVector, "for linear transformation",
               [ IsLinearTransformation ],
               f -> Hom( Source( f ), Range( f ) ) );

InstallMethod( AsList,
               [ IsLinearTransformation ],
function( T )
  return Flat( RowsOfMatrix( RightMatrixOfLinearTransformation( T ) ) );
end );

InstallMethod( MatrixOfLinearTransformation, "for linear transformation",
               [ IsDirection, IsLinearTransformation ],
function( dir, T )
  local f;
  f := MatrixOfLinearTransformation ^ dir;
  return f( T );
end );

InstallMethod( String, [ IsLinearTransformation ],
function( T )
  return Concatenation( "linear transformation ",
                        String( Dimension( Source( T ) ) ), "->",
                        String( Dimension( Range( T ) ) ) );
end );

InstallMethod( ViewObj, [ IsLinearTransformation ],
function( T )
  Print( String( T ) );
end );

InstallMethod( \=, "for linear transformations",
               [ IsLinearTransformation, IsLinearTransformation ],
function( T1, T2 )
  return Source( T1 ) = Source( T2 ) and
         Range( T1 ) = Range( T2 ) and
         RightMatrixOfLinearTransformation( T1 ) = RightMatrixOfLinearTransformation( T2 );
end );

InstallOtherMethod( \+, [ IsLinearTransformation, IsLinearTransformation ],
function( T1, T2 )
  local mat1, mat2;
  if Source( T1 ) <> Source( T2 ) then
    Error( "cannot add linear transformations with different source spaces" );
  elif Range( T1 ) <> Range( T2 ) then
    Error( "cannot add linear transformations with different range spaces" );
  fi;
  mat1 := RightMatrixOfLinearTransformation( T1 );
  mat2 := RightMatrixOfLinearTransformation( T2 );
  return LinearTransformationByRightMatrix( Source( T1 ), Range( T1 ),
                                            mat1 + mat2 );
end );

InstallMethod( \*, [ IsMultiplicativeElement, IsLinearTransformation ],
function( a, T )
  if not a in UnderlyingField( T ) then
    TryNextMethod();
  fi;
  return LinearTransformationByRightMatrix
         ( Source( T ),
           Range( T ),
           a * RightMatrixOfLinearTransformation( T ) );
end );

InstallOtherMethod( IsZero, [ IsLinearTransformation ],
               T -> IsZero( RightMatrixOfLinearTransformation( T ) ) );

InstallMethod( ImageElm, [ IsLinearTransformation, IsQPAVector ],
function( T, v )
  if not v in Source( T ) then
    Error( "vector not in source vector space" );
  fi;
  if IsZeroVectorSpace( Range( T ) ) then
    return Zero( Range( T ) );
  fi;
  return Vector( Range( T ),
                 AsStandardVector( v ) *
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

InstallMethod( \=, [ IsVectorSpaceCategory, IsVectorSpaceCategory ],
function( cat1, cat2 )
  return UnderlyingField( cat1 ) = UnderlyingField( cat2 );
end );

InstallMethod( CategoryOfVectorSpaces, [ IsField ],
function( F )
  local cat, equal_objects, equal_morphisms, zero_object,
        zero_morphism, is_zero_morphism, identity_morphism,
        pre_compose, addition, additive_inverse, kernel_emb,
        coker_proj, mono_lift, epi_colift, direct_sum,
        injection_direct_sum,  projection_direct_sum;

  cat := CreateCapCategory( Concatenation( "vector spaces over ", String( F ) ) );
  cat!.category_as_first_argument := true;
  SetFilterObj( cat, IsVectorSpaceCategory );
  SetUnderlyingField( cat, F );

  SetIsAbelianCategory( cat, true );

  AddIsEqualForObjects( cat, { category, m1, m2 } -> m1 = m2 );
  AddIsEqualForMorphisms( cat, { category, m1, m2 } -> m1 = m2 );

  zero_object := function( category )
    return ZeroVectorSpace( F );
  end;
  AddZeroObject( cat, zero_object );

  zero_morphism := function( category, V1, V2 )
    return LinearTransformationByRightMatrix
           ( V1, V2, MakeZeroMatrix( F, Dimension( V1 ), Dimension( V2 ) ) );
  end;
  AddZeroMorphism( cat, zero_morphism );

  is_zero_morphism := function( category, m )
    return IsZero( RightMatrixOfLinearTransformation( m ) );
  end;
  AddIsZeroForMorphisms( cat, is_zero_morphism );

  identity_morphism := function( category, V )
    return LinearTransformationByRightMatrix
           ( V, V, IdentityMatrix( F, Dimension( V ) ) );
  end;
  AddIdentityMorphism( cat, identity_morphism );

  pre_compose := function( category, m1, m2 )
    return LinearTransformationByRightMatrix
           ( Source( m1 ), Range( m2 ),
             RightMatrixOfLinearTransformation( m1 ) *
             RightMatrixOfLinearTransformation( m2 ) );
  end;
  AddPreCompose( cat, pre_compose );

  addition := function( category, m1, m2 )
    return LinearTransformationByRightMatrix
           ( Source( m1 ), Range( m1 ),
             RightMatrixOfLinearTransformation( m1 ) +
             RightMatrixOfLinearTransformation( m2 ) );
  end;
  AddAdditionForMorphisms( cat, addition );

  additive_inverse := function( category, m )
    return LinearTransformationByRightMatrix
           ( Source( m ), Range( m ), - RightMatrixOfLinearTransformation( m ) );
  end;
  AddAdditiveInverseForMorphisms( cat, additive_inverse );

  kernel_emb := function( category, m )
    local kernel_mat, dim;
    kernel_mat := NullspaceMat( RightMatrixOfLinearTransformation( m ) );
    dim := DimensionsMat( kernel_mat )[ 1 ];
    return LinearTransformationByRightMatrix
           ( StandardVectorSpace( F, dim ), Source( m ),
             kernel_mat );
  end;
  AddKernelEmbedding( cat, kernel_emb );

  coker_proj := function( category, m )
    local mat, coker_mat, dim;
    mat := RightMatrixOfLinearTransformation( m );
    coker_mat := TransposedMat( NullspaceMat( TransposedMat( mat ) ) );
    dim := DimensionsMat( coker_mat )[ 2 ];
    return LinearTransformationByRightMatrix
           ( Range( m ), StandardVectorSpace( F, dim ), coker_mat );
  end;
  AddCokernelProjection( cat, coker_proj );

  mono_lift := function( category, i, test )
    local matrix;
    if IsZero( Source( i ) ) or IsZero( Source( test ) ) then
      return zero_morphism( CapCategory( test ), Source( test ), Source( i ) );
    fi;
    matrix := List( Basis( Source( test ) ),
                    v -> AsList( PreImagesRepresentative( i, ImageElm( test, v ) ) ) );
    return LinearTransformationByRightMatrix
           ( Source( test ), Source( i ),
             MatrixByRows( F, matrix ) );
  end;
  AddLiftAlongMonomorphism( cat, mono_lift );

  epi_colift := function( category, e, test )
    local matrix;
    if IsZero( Range( e ) ) or IsZero( Range( test ) ) then
      return zero_morphism( CapCategory( e ), Range( e ), Range( test ) );
    fi;
    matrix := List( Basis( Range( e ) ),
                    v -> AsList( ImageElm( test,
                                           PreImagesRepresentative( e, v ) ) ) );
    return LinearTransformationByRightMatrix
           ( Range( e ), Range( test ),
             MatrixByRows( F, matrix ) );
  end;
  AddColiftAlongEpimorphism( cat, epi_colift );

  direct_sum := function( category, summands )
    return StandardVectorSpace( F, Sum( List( summands, Dimension ) ) );
  end;
  AddDirectSum( cat, direct_sum );

  injection_direct_sum := function( category, summands, i, sum )
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

  projection_direct_sum := function( category, summands, i, sum )
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
  CapCategorySwitchLogicOff( cat ); # TODO test this

  return Intern( cat );
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
  
  if dim1[ 1 ] = 0 then 
    return MakeZeroMatrix( F, 0, dim1[ 2 ] + dim2[ 2 ] );
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
  if dim1[ 2 ] = 0 then 
    return MakeZeroMatrix( F, dim1[ 1 ] + dim2[ 1 ], 0 );
  fi;
  rows := Concatenation( RowsOfMatrix( m1 ), RowsOfMatrix( m2 ) );
  return MatrixByRows( F, rows );
end );

InstallMethod( SubspaceInclusion, [ IsQPAVectorSpace, IsHomogeneousList ],
function( V, gens )
  local   K,  W,  B,  matrix,  WQPA, f;
  
  if not ForAll( gens, g -> g in V ) then
    Error("not all generators are in the entered vector space,\n");
  fi;
  K := UnderlyingField( V );
  if ForAll( gens, IsZero ) then
    f := ZeroMorphism( ZeroVectorSpace( K ), V );
  else
    W := VectorSpace( K, List( gens, AsList ) );
    B := BasisVectors( CanonicalBasis( W ) );
    matrix := MatrixByRows( K, B );
    WQPA := StandardVectorSpace( K, Length( B ) );
    f := LinearTransformationByRightMatrix( WQPA, V, matrix );
  fi;
  SetIsInjective( f, true );
  return f;
end );

InstallMethod( LeftInverse, [ IsLinearTransformation ],
function( f )
  local   dim_src,  f_mat,  mat,  ort,  basis_change,  proj_mat;

  if not IsInjective( f ) then
    Error( "non-injective linear transformation do not have left inverses" );
  fi;

  dim_src := Dimension( Source( f ) );

  if dim_src = 0 then
    return ZeroMorphism( Range( f ), Source( f ) );
  fi;

  f_mat := RightMatrixOfLinearTransformation( f );
  mat := RowsOfMatrix( f_mat );
  ort := BaseOrthogonalSpaceMat( mat );
  basis_change := Inverse( Concatenation( mat, ort ) );
  proj_mat := List( basis_change, row -> row{ [ 1 .. dim_src ] } );
  return LinearTransformationByRightMatrix( Range( f ), Source( f ),
                                            MatrixByRows( UnderlyingField( f ), proj_mat ) );
end );

InstallMethod( RightInverse, [ IsLinearTransformation ],
function( f )
  local dim_range, f_mat, mat, ort, basis_change, proj_mat;

  if not IsEpimorphism( f ) then
    Error( "Non-surjective linear transformations do not have right inverses.\n" );
  fi;
  dim_range := Dimension( Range( f ) );
  if dim_range = 0 then
    return ZeroMorphism( Range( f ), Source( f ) );
  fi;

  f_mat := RightMatrixOfLinearTransformation( f );
  mat := TransposedMat( RowsOfMatrix( f_mat ) );
  ort := BaseOrthogonalSpaceMat( mat );
  basis_change := Inverse( Concatenation( mat, ort ) );
  proj_mat := TransposedMat( List( basis_change, row -> row{ [ 1 .. dim_range ] } ) );
  
  return LinearTransformationByRightMatrix( Range( f ), Source( f ),
                                            MatrixByRows( UnderlyingField( f ), proj_mat ) );
end );

InstallMethod( PreImagesRepresentative, "for a linear transformation and a vector",
               [ IsLinearTransformation, IsQPAVector ],
function( f, v )
  local   temp;

  if not v in Range( f ) then
    Error("the entered element is not in the range for the entered linear transformation,\n");
  fi;
  temp := SolutionMat( RightMatrixOfLinearTransformation( f ), AsStandardVector( v ) );
  if temp <> fail then 
    return Vector( Source( f ), temp );
  else
    return fail;
  fi;
end
);

##
InstallOtherMethod( InverseOp,
    [ IsLinearTransformation ],
function( m )
  return LiftAlongMonomorphism( m, IdentityMorphism( Range( m ) ) );
end );

##
InstallMethod( Display, [ IsLinearTransformation ],
  function( m )
  local dim, mat;
  mat := RightMatrixOfLinearTransformation( m );
  dim := Concatenation( String( DimensionsMat( mat )[ 1 ] ), "x", String( DimensionsMat( mat )[ 2 ] ) );
  Print( "Linear transformation given (in row-convention) by the ", dim, " matrix \n" );
  Display( RowsOfMatrix( mat ) );
end );


InstallHandlingByNiceBasis( "IsSubspaceOfQPAVectorSpace",
  rec( detect :=
       function( F, gens, V, z )
         if z <> false then
           return IsQPAVector( z );
         else
           return IsQPAVector( gens[ 1 ] );
         fi;
       end,
       NiceFreeLeftModuleInfo := function( V )
         local gens, whole_space, inc;
         gens := GeneratorsOfLeftModule( V );
         whole_space := SpaceContainingVector( Zero( V ) );
         inc := SubspaceInclusion( whole_space, gens );
         return rec( whole_space := whole_space,
                     inc := inc );
       end,
       NiceVector := function( V, v )
         local info, inc, pre;
         info := NiceFreeLeftModuleInfo( V );
         inc := info.inc;
         pre := PreImagesRepresentative( inc, v );
         # if pre = fail then
         #   return fail;
         # fi;
         return pre;
       end,
       UglyVector := function( V, r )
         local info, inc;
         info := NiceFreeLeftModuleInfo( V );
         inc := info.inc;
         return ImageElm( inc, r );
       end ) );


InstallMethod( NiceFreeLeftModule, [ IsSubspaceOfQPAVectorSpace ],
function( V )
  local info;
  info := NiceFreeLeftModuleInfo( V );
  return Source( info.inc );
end );

