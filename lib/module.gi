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
               [ IsQuiverRepresentation, IsString ],
function( R, side )
  local attr, k, Q, rep_algebra, cat, module_type, left_algebra,  
        right_algebra, algebras, M;

  if side = LEFT then attr := AsLeftModule;
  elif side = RIGHT then attr := AsRightModule;
  elif side = LEFT_RIGHT then attr := AsBimodule;
  else Error( "invalid value for side: ", side ); fi;
  if Tester( attr )( R ) then
    return attr( R );
  fi;

  k := FieldOfRepresentation( R );
  Q := QuiverOfRepresentation( R );
  rep_algebra := AlgebraOfRepresentation( R );
  cat := AsModuleCategory( CapCategory( R ), side );
  if side = LEFT then
    module_type := IsLeftQuiverModule;
    left_algebra := AlgebraForLeftModules( rep_algebra );
    right_algebra := fail;
  elif side = RIGHT then
    module_type := IsRightQuiverModule;
    right_algebra := AlgebraForRightModules( rep_algebra );
    left_algebra := fail;
  elif side = LEFT_RIGHT then
    module_type := IsQuiverBimodule;
    # TODO: check that rep_algebra is tensor product of two algebras
    algebras := TensorProductFactorsLeftRight( rep_algebra );
    left_algebra := algebras[ 1 ];
    right_algebra := algebras[ 2 ];
  fi;
  M := rec();
  ObjectifyWithAttributes( M, NewType( FamilyOfQuiverModules,
                                       module_type and IsQuiverModuleRep ),
                           UnderlyingRepresentation, R,
                           LeftActingDomain, k,
                           Side, side );
  if left_algebra <> fail then
    SetLeftActingAlgebra( M, left_algebra );
  fi;
  if right_algebra <> fail then
    SetRightActingAlgebra( M, right_algebra );
  fi;
  Add( cat, M );
  Setter( attr )( R, M );
  return M;
end );

InstallMethod( AsLeftModule,
               [ IsQuiverRepresentation ],
               R -> AsModule( R, LEFT ) );

InstallMethod( AsRightModule,
               [ IsQuiverRepresentation ],
               R -> AsModule( R, RIGHT ) );

InstallMethod( AsBimodule,
               [ IsQuiverRepresentation ],
               R -> AsModule( R, LEFT_RIGHT ) );

InstallMethod( QuiverModule,
               [ IsString, IsQuiverAlgebra, IsDenseList, IsList ],
function( side, A, dimensions, matrices )
  return AsModule( QuiverRepresentation( A^side, dimensions, matrices ),
                   side );
end );

InstallMethod( QuiverModule,
               [ IsString, IsQuiverAlgebra, IsDenseList, IsDenseList, IsDenseList ],
function( side, A, dimensions, arrows, matrices_for_arrows )
  return AsModule( QuiverRepresentation( A^side,
                                         dimensions, arrows, matrices_for_arrows ),
                   side );
end );

InstallMethod( LeftQuiverModule,
               [ IsPathAlgebra, IsDenseList, IsList ],
function( A, dimensions, matrices )
  return QuiverModule( LEFT, A, dimensions, matrices );
end );

InstallMethod( LeftQuiverModule,
               [ IsPathAlgebra, IsDenseList, IsDenseList, IsDenseList ],
function( A, dimensions, arrows, matrices_for_arrows )
  return QuiverModule( LEFT, A, dimensions, arrows, matrices_for_arrows );
end );

InstallMethod( RightQuiverModule,
               [ IsPathAlgebra, IsDenseList, IsList ],
function( A, dimensions, matrices )
  return QuiverModule( RIGHT, A, dimensions, matrices );
end );

InstallMethod( RightQuiverModule,
               [ IsPathAlgebra, IsDenseList, IsDenseList, IsDenseList ],
function( A, dimensions, arrows, matrices_for_arrows )
  return QuiverModule( RIGHT, A, dimensions, arrows, matrices_for_arrows );
end );

InstallMethod( QuiverBimodule,
               [ IsQuiverAlgebra, IsQuiverAlgebra, IsDenseList, IsList ],
function( A, B, dimensions, matrices )
  return AsBimodule( QuiverRepresentation( AlgebraForBimodules( A, B ), dimensions, matrices ) );
end );

InstallMethod( QuiverBimodule,
               [ IsQuiverAlgebra, IsQuiverAlgebra, IsDenseList, IsDenseList, IsList ],
function( A, B, dimensions, arrows, matrices_for_arrows )
  return AsBimodule( QuiverRepresentation( AlgebraForBimodules( A, B ), dimensions, arrows, matrices_for_arrows ) );
end );

InstallMethod( LeftZeroModule,
               [ IsPathAlgebra ],
function( A )
  return AsLeftModule
         ( ZeroRepresentation( AlgebraForLeftModules( A ) ) );
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


InstallMethod( AsModuleCategory, [ IsQuiverRepresentationCategory, IsString ],
function( rep_cat, side )
  local attr, rep_algebra, Q, _R, _r, _M, _m, algebras, A, B, cat;

  if side = LEFT then attr := AsLeftModuleCategory;
  elif side = RIGHT then attr := AsRightModuleCategory;
  elif side = LEFT_RIGHT then attr := AsBimoduleCategory;
  else Error( "invalid value for side: ", side ); fi;
  if Tester( attr )( rep_cat ) then
    return attr( rep_cat );
  fi;

  rep_algebra := AlgebraOfCategory( rep_cat );
  Q := QuiverOfAlgebra( rep_algebra );

  _R := UnderlyingRepresentation;
  _r := UnderlyingRepresentationHomomorphism;
  _M := R -> AsModule( R, side );
  _m := f -> AsModuleHomomorphism( f, side );

  if side = LEFT_RIGHT then
    algebras := TensorProductFactorsLeftRight( rep_algebra );
    A := algebras[ 1 ];
    B := algebras[ 2 ];
    cat := CreateCapCategory( Concatenation( "bimodules over ", String( A ), " and ", String( B ) ) );
    SetFilterObj( cat, IsQuiverBimoduleCategory );
    SetAlgebrasOfCategory( cat, algebras );
  else
    A := rep_algebra^side;
    cat := CreateCapCategory( Concatenation( side, " modules over ", String( A ) ) );
    if side = LEFT then
      SetFilterObj( cat, IsLeftQuiverModuleCategory );
    else
      SetFilterObj( cat, IsRightQuiverModuleCategory );
    fi;
    SetAlgebraOfCategory( cat, A );
  fi;
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

  Setter( attr )( rep_cat, cat );
  return cat;
end );

InstallMethod( AsLeftModuleCategory, [ IsQuiverRepresentationCategory ],
               rep_cat -> AsModuleCategory( rep_cat, LEFT ) );

InstallMethod( AsRightModuleCategory, [ IsQuiverRepresentationCategory ],
               rep_cat -> AsModuleCategory( rep_cat, RIGHT ) );

InstallMethod( AsBimoduleCategory, [ IsQuiverRepresentationCategory ],
               rep_cat -> AsModuleCategory( rep_cat, LEFT_RIGHT ) );

InstallMethod( ModuleCategory,
               [ IsQuiverAlgebra, IsString ],
function( A, side )
  return AsModuleCategory( CategoryOfQuiverRepresentations( A^side ),
                           side );
end );

InstallMethod( LeftModuleCategory, [ IsQuiverAlgebra ],
               A -> ModuleCategory( A, LEFT ) );

InstallMethod( RightModuleCategory, [ IsQuiverAlgebra ],
               A -> ModuleCategory( A, RIGHT ) );

InstallMethod( BimoduleCategory,
               [ IsQuiverAlgebra, IsQuiverAlgebra ],
function( A, B )
  local T;
  T := AlgebraForBimodules( A, B );
  return AsModuleCategory( CategoryOfQuiverRepresentations( T ), LEFT_RIGHT );
end );
