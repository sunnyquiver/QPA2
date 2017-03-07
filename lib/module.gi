DeclareRepresentation( "IsQuiverModuleElementRep",
                       IsComponentObjectRep and IsAttributeStoringRep, [] );
DeclareRepresentation( "IsQuiverModuleRep",
                       IsComponentObjectRep and IsAttributeStoringRep, [] );

BindGlobal( "FamilyOfQuiverModuleElements",
            NewFamily( "quiver module elements" ) );
BindGlobal( "FamilyOfQuiverModules",
            CollectionsFamily( FamilyOfQuiverModuleElements ) );

InstallMethod( AlgebraForLeftModules,
               [ IsQuiverAlgebra ],
function( A )
  if IsLeftQuiver( QuiverOfAlgebra( A ) ) then
    return A;
  else
    return OppositeAlgebra( A );
  fi;
end );

InstallMethod( AlgebraForRightModules,
               [ IsQuiverAlgebra ],
function( A )
  if IsRightQuiver( QuiverOfAlgebra( A ) ) then
    return A;
  else
    return OppositeAlgebra( A );
  fi;
end );

InstallMethod( AlgebraForBimodules,
               [ IsQuiverAlgebra, IsQuiverAlgebra ],
function( A, B )
  local A_, B_;
  if IsLeftQuiver( QuiverOfAlgebra( A ) ) then
    A_ := A;
  else
    A_ := OppositeAlgebra( A );
  fi;
  if IsRightQuiver( QuiverOfAlgebra( B ) ) then
    B_ := B;
  else
    B_ := OppositeAlgebra( B );
  fi;
  return TensorProductOfAlgebras( A_, B_ );
end );

InstallMethod( AsModule,
               [ IsQuiverRepresentation, IsQuiverAlgebra ],
function( R, A )
  local M, k, Q, rep_algebra, cat, module_type, acting_algebra_attr;
  k := FieldOfRepresentation( R );
  Q := QuiverOfRepresentation( R );
  rep_algebra := AlgebraOfRepresentation( R );
  if rep_algebra = A then
    cat := AsDirectCategoryOfModules( CapCategory( R ) );
    if IsLeftQuiver( Q ) then
      module_type := IsLeftQuiverModule;
    else
      module_type := IsRightQuiverModule;
    fi;
  elif rep_algebra = OppositeAlgebra( A ) then
    cat := AsOppositeCategoryOfModules( CapCategory( R ) );
    if IsLeftQuiver( Q ) then
      module_type := IsRightQuiverModule;
    else
      module_type := IsLeftQuiverModule;
    fi;
  else
    Error( "Representation is not over the given algebra or its opposite" );
  fi;
  if module_type = IsLeftQuiverModule then
    acting_algebra_attr := LeftActingAlgebra;
  else
    acting_algebra_attr := RightActingAlgebra;
  fi;
  M := rec();
  ObjectifyWithAttributes( M, NewType( FamilyOfQuiverModules,
                                       module_type and IsQuiverModuleRep ),
                           UnderlyingRepresentation, R,
                           LeftActingDomain, k,
                           acting_algebra_attr, A );
  Add( cat, M );
  return M;
end );

InstallMethod( AsLeftModule,
               [ IsQuiverRepresentation ],
function( R )
  return AsModule( R, AlgebraForLeftModules( AlgebraOfRepresentation( R ) ) );
end );

InstallMethod( AsRightModule,
               [ IsQuiverRepresentation ],
function( R )
  return AsModule( R, AlgebraForRightModules( AlgebraOfRepresentation( R ) ) );
end );

InstallMethod( AsBimodule,
               [ IsQuiverRepresentation, IsQuiverAlgebra, IsQuiverAlgebra ],
function( R, A, B )
  local M;
  if not IsTensorProductOfAlgebras( AlgebraOfRepresentation( R ),
                                    A, OppositeAlgebra( B ) ) then
    Error( "Representation is not over the appropriate tensor algebra" );
  fi;
  M := rec();
  ObjectifyWithAttributes( M, NewType( FamilyOfQuiverModules,
                                       IsQuiverBimodule and IsQuiverModuleRep ),
                           UnderlyingRepresentation, R,
                           LeftActingAlgebra, A,
                           RightActingAlgebra, B );
  return M;
end );

InstallMethod( QuiverBimodule,
               [ IsQuiverAlgebra, IsQuiverAlgebra, IsDenseList, IsList ],
function( A, B, dimensions, matrices )
  return AsBimodule( QuiverRepresentation( AlgebraForBimodules( A, B ), dimensions, matrices ),
                     A, B );
end );

InstallMethod( QuiverBimodule,
               [ IsQuiverAlgebra, IsQuiverAlgebra, IsDenseList, IsDenseList, IsList ],
function( A, B, dimensions, arrows, matrices_for_arrows )
  return AsBimodule( QuiverRepresentation( AlgebraForBimodules( A, B ), dimensions, arrows, matrices_for_arrows ),
                     A, B );
end );

InstallMethod( LeftQuiverModule,
               [ IsPathAlgebra, IsDenseList, IsDenseList ],
function( A, dimensions, matrices )
  return AsLeftModule
         ( QuiverRepresentation( AlgebraForLeftModules( A ),
                                 dimensions, matrices ) );
end );

InstallMethod( LeftQuiverModuleByArrows,
               [ IsPathAlgebra, IsDenseList, IsDenseList, IsDenseList ],
function( A, dimensions, arrows, matrices )
  return AsLeftModule
         ( QuiverRepresentationByArrows( AlgebraForLeftModules( A ),
                                         dimensions, arrows, matrices ) );
end );

InstallMethod( LeftZeroModule,
               [ IsPathAlgebra ],
function( A )
  return AsLeftModule
         ( ZeroRepresentation( AlgebraForLeftModules( A ) ) );
end );

InstallMethod( RightQuiverModule,
               [ IsPathAlgebra, IsDenseList, IsDenseList ],
function( A, dimensions, matrices )
  return AsRightModule
         ( QuiverRepresentation( AlgebraForRightModules( A ),
                                 dimensions, matrices ) );
end );

InstallMethod( RightQuiverModuleByArrows,
               [ IsPathAlgebra, IsDenseList, IsDenseList, IsDenseList ],
function( A, dimensions, arrows, matrices )
  return AsRightModule
         ( QuiverRepresentationByArrows( AlgebraForRightModules( A ),
                                         dimensions, arrows, matrices ) );
end );

InstallMethod( RightZeroModule,
               [ IsPathAlgebra ],
function( A )
  return AsRightModule
         ( ZeroRepresentation( AlgebraForRightModules( A ) ) );
end );

InstallMethod( \=, [ IsLeftQuiverModule, IsLeftQuiverModule ],
function( M1, M2 )
  return UnderlyingRepresentation( M1 ) = UnderlyingRepresentation( M2 );
end );

InstallMethod( \=, [ IsRightQuiverModule, IsRightQuiverModule ],
function( M1, M2 )
  return UnderlyingRepresentation( M1 ) = UnderlyingRepresentation( M2 );
end );

InstallMethod( \=, [ IsQuiverBimodule, IsQuiverBimodule ],
function( M1, M2 )
  return UnderlyingRepresentation( M1 ) = UnderlyingRepresentation( M2 );
end );

InstallMethod( String,
               [ IsQuiverModule ],
function( M )
  return String( UnderlyingRepresentation( M ) );
end );

InstallMethod( ViewObj,
               [ IsQuiverModule ],
function( R )
  Print( "<", String( R ), ">" );
end );

InstallMethod( QuiverOfModule,
               [ IsQuiverModule ],
function( M )
  return QuiverOfRepresentation( UnderlyingRepresentation( M ) );
end );

InstallMethod( FieldOfModule,
               [ IsQuiverModule ],
function( M )
  return FieldOfRepresentation( UnderlyingRepresentation( M ) );
end );

InstallMethod( VertexDimensions,
               [ IsQuiverModule ],
function( M )
  return VertexDimensions( UnderlyingRepresentation( M ) );
end );

InstallMethod( VertexDimension,
               [ IsQuiverModule, IsPosInt ],
function( M, i )
  return VertexDimension( UnderlyingRepresentation( M ), i );
end );

InstallMethod( VertexDimension,
               [ IsQuiverModule, IsVertex ],
function( M, v )
  return VertexDimension( UnderlyingRepresentation( M ), v );
end );

InstallMethod( AsModuleElement,
               [ IsQuiverRepresentationElement, IsQuiverModule ],
function( e, M )
  local me, elem_cat;
  if RepresentationOfElement( e ) <> UnderlyingRepresentation( M ) then
    Error( "Element is not from the underlying representation of the module" );
  fi;
  if IsLeftQuiverModule( M ) then
    elem_cat := IsLeftQuiverModuleElement;
  else
    elem_cat := IsRightQuiverModuleElement;
  fi;
  me := rec();
  ObjectifyWithAttributes( me, NewType( FamilyOfQuiverModuleElements,
                                        elem_cat and IsQuiverModuleRep ),
                           UnderlyingRepresentationElement, e,
                           ModuleOfElement, M );
  return me;
end );

InstallMethod( QuiverModuleElement,
               [ IsQuiverModule, IsDenseList ],
function( M, vectors )
  local R, e;
  R := UnderlyingRepresentation( M );
  e := QuiverRepresentationElement( R, vectors );
  return AsModuleElement( e, M );
end );

InstallMethod( QuiverModuleElementByVertices,
               [ IsQuiverModule, IsDenseList, IsDenseList ],
function( M, vertices, vectors )
  local R, e;
  R := UnderlyingRepresentation( M );
  e := QuiverRepresentationElement( R, vertices, vectors );
  return AsModuleElement( e, M );
end );

InstallMethod( Zero,
               [ IsQuiverModule ],
function( M )
  local R;
  R := UnderlyingRepresentation( M );
  return AsModuleElement( Zero( R ), M );
end );

InstallMethod( ElementVectors,
               [ IsQuiverModuleElement ],
function( e )
  return ElementVectors( UnderlyingRepresentationElement( e ) );
end );

InstallMethod( ElementVector,
               [ IsQuiverModuleElement, IsPosInt ],
function( e, i )
  return ElementVector( UnderlyingRepresentationElement( e ), i );
end );

InstallMethod( \[\], [ IsQuiverModuleElement, IsPosInt ], ElementVector );

InstallMethod( ElementVector,
               [ IsQuiverModuleElement, IsVertex ],
function( e, v )
  return ElementVector( UnderlyingRepresentationElement( e ), v );
end );

InstallMethod( String,
               [ IsLeftQuiverModuleElement ],
function( e )
  return Concatenation( "left module element ", String( ElementVectors( e ) ) );
end );

InstallMethod( String,
               [ IsRightQuiverModuleElement ],
function( e )
  return Concatenation( "right module element ", String( ElementVectors( e ) ) );
end );

InstallMethod( ViewObj,
               [ IsQuiverModuleElement ],
function( e )
  Print( "<", String( e ), ">" );
end );

InstallMethod( \in, "for quiver module element and quiver module",
               [ IsQuiverModuleElement, IsQuiverModule ],
function( e, M )
  return ModuleOfElement( e ) = M;
end );

InstallMethod( \^,
               [ IsQuiverAlgebraElement, IsLeftQuiverModuleElement ],
function( ae, me )
  local A, Q, re, re_;
  A := AlgebraOfElement( ae );
  Q := QuiverOfAlgebra( A );
  re := UnderlyingRepresentationElement( me );
  if IsLeftQuiver( Q ) then
    re_ := QuiverAlgebraAction( re, ae );
  else
    re_ := QuiverAlgebraAction( re, OppositeAlgebraElement( ae ) );
  fi;
  return AsModuleElement( re_, ModuleOfElement( me ) );
end );

InstallMethod( \^,
               [ IsRightQuiverModuleElement, IsQuiverAlgebraElement ],
function( me, ae )
  local A, Q, re, re_;
  A := AlgebraOfElement( ae );
  Q := QuiverOfAlgebra( A );
  re := UnderlyingRepresentationElement( me );
  if IsRightQuiver( Q ) then
    re_ := QuiverAlgebraAction( re, ae );
  else
    re_ := QuiverAlgebraAction( re, OppositeAlgebraElement( ae ) );
  fi;
  return AsModuleElement( re_, ModuleOfElement( me ) );
end );

InstallMethod( \=, [ IsQuiverModuleElement, IsQuiverModuleElement ],
function( e1, e2 )
  local re1, re2;
  if ModuleOfElement( e1 ) <> ModuleOfElement( e2 ) then
    return false;
  fi;
  re1 := UnderlyingRepresentationElement( e1 );
  re2 := UnderlyingRepresentationElement( e2 );
  return re1 = re2;
end );

InstallMethod( \+, [ IsQuiverModuleElement, IsQuiverModuleElement ],
function( e1, e2 )
  local re1, re2;
  if ModuleOfElement( e1 ) <> ModuleOfElement( e2 ) then
    Error( "cannot add elements of different modules" );
  fi;
  re1 := UnderlyingRepresentationElement( e1 );
  re2 := UnderlyingRepresentationElement( e2 );
  return AsModuleElement( re1 + re2, ModuleOfElement( e1 ) );
end );

InstallMethod( \*, "for multiplicative element and element of quiver module",
               [ IsMultiplicativeElement, IsQuiverModuleElement ],
function( c, e )
  return AsModuleElement( c * UnderlyingRepresentationElement( e ),
                          ModuleOfElement( e ) );
end );

InstallMethod( \*, "for element of quiver module and multiplicative element",
               [ IsQuiverModuleElement, IsMultiplicativeElement ],
function( e, c )
  return AsModuleElement( UnderlyingRepresentationElement( e ) * c,
                          ModuleOfElement( e ) );
end );

# basis of modules

BindGlobal( "FamilyOfQuiverModuleBases",
            NewFamily( "quiver module bases" ) );

DeclareRepresentation( "IsQuiverModuleBasisRep", IsComponentObjectRep,
                       [ "module", "underlyingRepresentationBasis" ] );

InstallMethod( CanonicalBasis, "for quiver module",
               [ IsQuiverModule ],
function( M )
  local R, rep_basis;
  R := UnderlyingRepresentation( M );
  rep_basis := CanonicalBasis( R );
  return Objectify( NewType( FamilyOfQuiverModuleBases,
                             IsBasis and IsQuiverModuleBasisRep ),
                    rec( module := M,
                         underlyingRepresentationBasis := rep_basis ) );
end );

InstallMethod( Basis, "for quiver module",
               [ IsQuiverModule ],
               CanonicalBasis );

InstallMethod( BasisVectors, "for quiver module basis",
               [ IsBasis and IsQuiverModuleBasisRep ],
function( B )
  return List( BasisVectors( B!.underlyingRepresentationBasis ),
               v -> AsModuleElement( v, B!.module ) );
end );

# TODO: right modules?
InstallMethod( UnderlyingLeftModule, "for quiver module basis",
               [ IsBasis and IsQuiverModuleBasisRep ],
function( B )
  return B!.module;
end );


InstallMethod( AsCategoryOfModules, [ IsQuiverRepresentationCategory, IsBool ],
function( rep_cat, is_opposite )
  local rA, Q, A, side, _R, _r, _M, _m, cat;

  # TODO: add objects to category when they are created

  rA := AlgebraOfCategory( rep_cat );
  Q := QuiverOfAlgebra( rA );

  if is_opposite then
    A := OppositeAlgebra( rA );
  else
    A := rA;
  fi;

  if IsLeftQuiver( Q ) and not is_opposite then
    side := "left";
  else
    side := "right";
  fi;

  _R := UnderlyingRepresentation;
  _r := UnderlyingRepresentationHomomorphism;
  _M := R -> AsModule( R, A );
  _m := m -> AsModuleHomomorphism( m, A );

  cat := CreateCapCategory( Concatenation( side, " modules over ", String( A ) ) );
  SetFilterObj( cat, IsQuiverModuleCategory );
  SetAlgebraOfCategory( cat, A );
  SetUnderlyingRepresentationCategory( cat, rep_cat );
  SetIsAbelianCategory( cat, true );

  AddIsEqualForObjects( cat,
  function( M1, M2 )
    return IsEqualForObjects( _R( M1 ), _R( M2 ) );
  end );

  AddIsEqualForMorphisms( cat,
  function( m1, m2 )
    return IsEqualForMorphisms( _r( m1 ), _r( m2 ) );
  end );

  AddZeroObject( cat, function()
    return _M( ZeroObject( rep_cat ) );
  end );
  AddZeroMorphism( cat, function( M1, M2 )
    return _m( ZeroMorphism( _R( M1 ), _R( M2 ) ) );
  end );
  AddIdentityMorphism( cat, M -> _m( IdentityMorphism( _R( M ) ) ) );
  AddPreCompose( cat, function( m1, m2 )
    return _m( PreCompose( _r( m1 ), _r( m2 ) ) );
  end );
  AddAdditionForMorphisms( cat, function( m1, m2 )
    return _m( AdditionForMorphisms( _r( m1 ), _r( m2 ) ) );
  end );
  AddAdditiveInverseForMorphisms( cat, m -> _m( AdditiveInverseForMorphisms( _r( m ) ) ) );
  AddKernelEmbedding( cat, m -> _m( KernelEmbedding( _r( m ) ) ) );
  AddCokernelProjection( cat, m -> _m( CokernelProjection( _r( m ) ) ) );
  AddLiftAlongMonomorphism( cat, function( i, test )
    return _m( LiftAlongMonomorphism( _r( i ), _r( test ) ) );
  end );
  AddColiftAlongEpimorphism( cat, function( e, test )
    return _m( ColiftAlongEpimorphism( _r( e ), _r( test ) ) );
  end );
  AddDirectSum( cat, function( summands )
    return _M( DirectSum( List( summands, _R ) ) );
  end );
  AddInjectionOfCofactorOfDirectSumWithGivenDirectSum( cat, function( summands, i, sum )
    return _m( InjectionOfCofactorOfDirectSumWithGivenDirectSum
               ( List( summands, _R ), i, _R( sum ) ) );
  end );
  AddProjectionInFactorOfDirectSumWithGivenDirectSum( cat, function( summands, i, sum )
    return _m( ProjectionInFactorOfDirectSumWithGivenDirectSum
               ( List( summands, _R ), i, _R( sum ) ) );
  end );

  Finalize( cat );

  return cat;
end );

InstallMethod( AsDirectCategoryOfModules, [ IsQuiverRepresentationCategory ],
               rep_cat -> AsCategoryOfModules( rep_cat, false ) );

InstallMethod( AsOppositeCategoryOfModules, [ IsQuiverRepresentationCategory ],
               rep_cat -> AsCategoryOfModules( rep_cat, true ) );

InstallMethod( AsCategoryOfLeftModules, [ IsQuiverRepresentationCategory ],
function( rep_cat )
  if IsLeftQuiverAlgebra( AlgebraOfCategory( rep_cat ) ) then
    return AsDirectCategoryOfModules( rep_cat );
  else
    return AsOppositeCategoryOfModules( rep_cat );
  fi;
end );

InstallMethod( AsCategoryOfRightModules, [ IsQuiverRepresentationCategory ],
function( rep_cat )
  if IsRightQuiverAlgebra( AlgebraOfCategory( rep_cat ) ) then
    return AsDirectCategoryOfModules( rep_cat );
  else
    return AsOppositeCategoryOfModules( rep_cat );
  fi;
end );

InstallMethod( CategoryOfLeftModules, [ IsLeftQuiverAlgebra ],
               A -> AsDirectCategoryOfModules( CategoryOfQuiverRepresentations( A ) ) );
InstallMethod( CategoryOfLeftModules, [ IsRightQuiverAlgebra ],
               A -> AsOppositeCategoryOfModules( CategoryOfQuiverRepresentations( OppositeAlgebra( A ) ) ) );
InstallMethod( CategoryOfRightModules, [ IsRightQuiverAlgebra ],
               A -> AsDirectCategoryOfModules( CategoryOfQuiverRepresentations( A ) ) );
InstallMethod( CategoryOfRightModules, [ IsLeftQuiverAlgebra ],
               A -> AsOppositeCategoryOfModules( CategoryOfQuiverRepresentations( OppositeAlgebra( A ) ) ) );
