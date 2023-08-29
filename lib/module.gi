DeclareRepresentation( "IsQuiverModuleElementRep",
                       IsComponentObjectRep and IsAttributeStoringRep, [] );
DeclareRepresentation( "IsQuiverModuleRep",
                       IsComponentObjectRep and IsAttributeStoringRep, [] );

BindGlobal( "FamilyOfQuiverModuleElements",
            NewFamily( "quiver module elements" ) );
BindGlobal( "FamilyOfQuiverModules",
            CollectionsFamily( FamilyOfQuiverModuleElements ) );

DeclareSideOperations( IsQuiverModule, IsLeftQuiverModule, IsRightQuiverModule, IsQuiverBimodule );
DeclareSideOperations( AsModule, AsLeftQuiverModule, AsRightQuiverModule, AsBimodule );
DeclareSideOperations( QuiverModule, LeftQuiverModule, RightQuiverModule, QuiverBimodule );
DeclareSideOperations( ZeroModule, LeftZeroModule, RightZeroModule, ZeroBimodule );
DeclareSideOperations( IsQuiverModuleElement,
                       IsLeftQuiverModuleElement, IsRightQuiverModuleElement, IsQuiverBimoduleElement );
DeclareSideOperations( AsModuleCategory,
                       AsLeftModuleCategory, AsRightModuleCategory, AsBimoduleCategory );
DeclareSideOperations( ModuleCategory,
                       LeftModuleCategory, RightModuleCategory, BimoduleCategory );


InstallMethodWithSides( AsModule,
                        [ IsQuiverRepresentation ],
side -> function( R )
  local k, Q, rep_algebra, cat, module_type, left_algebra,  
        right_algebra, algebras, M;

  rep_algebra := AlgebraOfRepresentation( R );
  if side = LEFT_RIGHT then
    algebras := TensorProductFactorsLeftRight( rep_algebra );
  else
    algebras := [ fail, fail ];
    algebras[ Int( side ) ] := rep_algebra^side;
  fi;

  M := rec();
  ObjectifyWithAttributes( M, NewType( FamilyOfQuiverModules,
                                       IsQuiverModule^side and IsQuiverModuleRep ),
                           UnderlyingRepresentation, R,
                           LeftActingDomain, FieldOfRepresentation( R ),
                           ActingAlgebras, algebras,
                           Side, side );

  cat := AsModuleCategory( side, CapCategory( R ) );
  Add( cat, M );

  return M;
end );

InstallMethodWithSides( QuiverModule,
                        [ "algebra", IsDenseList, IsList ],
side -> function( A, dimensions, matrices )
  return AsModule( side, QuiverRepresentation( A^side, dimensions, matrices ) );
end );

InstallMethodWithSides( QuiverModule,
                        [ "algebra", IsDenseList, IsDenseList, IsDenseList ],
side -> function( A, dimensions, arrows, matrices_for_arrows )
  return AsModule( side,
                   QuiverRepresentation( A^side,
                                         dimensions, arrows, matrices_for_arrows ) );
end );

InstallMethod( QuiverBimodule,
               [ IsQuiverAlgebra, IsQuiverAlgebra, IsDenseList, IsList ],
function( A, B, dimensions, matrices )
  return QuiverBimodule( [ A, B ], dimensions, matrices );
end );

InstallMethod( QuiverBimodule,
               [ IsQuiverAlgebra, IsQuiverAlgebra, IsDenseList, IsDenseList, IsList ],
function( A, B, dimensions, arrows, matrices_for_arrows )
  return QuiverBimodule( [ A, B ], dimensions, arrows, matrices_for_arrows );
end );

InstallMethodWithSides( ZeroModule, [ "algebra" ],
side -> function( A )
  return AsModule( side, ZeroRepresentation( A^side ) );
end );

InstallMethod( \=, [ IsQuiverModule, IsQuiverModule ],
function( M1, M2 )
  return Side( M1 ) = Side( M2 )
         and UnderlyingRepresentation( M1 ) = UnderlyingRepresentation( M2 );
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

InstallMethod( Side, [ IsQuiverModule ],
               M -> Side( CapCategory( M ) ) );

DeclareSideOperations( ActingAlgebra,
                       LeftActingAlgebra, RightActingAlgebra, ActingAlgebras );

InstallMethodWithSides( ActingAlgebra, [ IsQuiverModule ],
side -> function( M )
  return ActingAlgebra( side, CapCategory( M ) );
end );

InstallMethod( ActingAlgebra, "for side and quiver module category",
               [ IsSide, IsQuiverModuleCategory ],
function( side, cat )
  return ( ActingAlgebra^side )( cat );
end );

InstallMethod( LeftActingAlgebra, "for quiver module category", [ IsQuiverModuleCategory ],
               cat -> ActingAlgebras( cat )[ 1 ] );
InstallMethod( RightActingAlgebra, "for quiver module category", [ IsQuiverModuleCategory ],
               cat -> ActingAlgebras( cat )[ 2 ] );

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

InstallMethod( DimensionVector,
               [ IsQuiverModule ],
function( M )
  return DimensionVector( UnderlyingRepresentation( M ) );
end );

InstallMethod( VertexDimension,
               [ IsQuiverModule, IsPosInt ],
function( M, i )
  return VertexDimension( UnderlyingRepresentation( M ), i );
end );

InstallMethod( VertexDimension,
               [ IsQuiverModule, IsQuiverVertex ],
function( M, v )
  return VertexDimension( UnderlyingRepresentation( M ), v );
end );

InstallMethod( AsQPAVectorSpace, "for quiver module", [ IsQuiverModule ],
               M -> AsQPAVectorSpace( UnderlyingRepresentation( M ) ) );

InstallMethod( AsModuleElement,
               [ IsQuiverRepresentationElement, IsQuiverModule ],
function( e, M )
  local me, elem_cat;
  if RepresentationOfElement( e ) <> UnderlyingRepresentation( M ) then
    Error( "Element is not from the underlying representation of the module" );
  fi;
  elem_cat := IsQuiverModuleElement^Side( M );
  me := rec();
  ObjectifyWithAttributes( me, NewType( FamilyOfQuiverModuleElements,
                                        elem_cat and IsQuiverModuleRep ),
                           UnderlyingRepresentationElement, e,
                           ModuleOfElement, M,
                           Side, Side( M ) );
  return me;
end );

DeclareSideOperations( AsModuleElement, AsLeftModuleElement, AsRightModuleElement,
                       AsBimoduleElement );

InstallMethodWithSides( AsModuleElement,
                        [ IsQuiverRepresentationElement ],
side -> function( r )
  return AsModuleElement( r, AsModule( side, RepresentationOfElement( r ) ) );
end );


InstallMethod( QuiverModuleElement,
               [ IsQuiverModule, IsList ],
function( M, vectors )
  local R, e;
  R := UnderlyingRepresentation( M );
  e := QuiverRepresentationElement( R, vectors );
  return AsModuleElement( e, M );
end );

InstallMethod( QuiverModuleElement,
               [ IsQuiverModule, IsDenseList, IsList ],
function( M, vertices, vectors )
  local R, e;
  R := UnderlyingRepresentation( M );
  e := QuiverRepresentationElement( R, vertices, vectors );
  return AsModuleElement( e, M );
end );

InstallMethod( AsVector,
               [ IsQuiverModuleElement ],
               e -> AsVector( UnderlyingRepresentationElement( e ) ) );

InstallMethod( AsList,
               [ IsQuiverModuleElement ],
               e -> AsList( UnderlyingRepresentationElement( e ) ) );

InstallMethod( Zero,
               [ IsQuiverModule ],
function( M )
  local R;
  R := UnderlyingRepresentation( M );
  return AsModuleElement( Zero( R ), M );
end );

InstallMethod( Zero,
               [ IsQuiverModuleElement ],
function( elem )
  return Zero( ModuleOfElement( elem ) );
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
               [ IsQuiverModuleElement, IsQuiverVertex ],
function( e, v )
  return ElementVector( UnderlyingRepresentationElement( e ), v );
end );

InstallMethod( String,
               [ IsQuiverModuleElement ],
function( e )
  return Concatenation( String( Side( e ) ), " module element ", String( ElementVectors( e ) ) );
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

InstallMethod( \^,
               [ IsQuiverAlgebraElement, IsQuiverBimoduleElement ],
function( a, m )
  local M, r, ax1;
  M := ModuleOfElement( m );
  r := UnderlyingRepresentationElement( m );
  ax1 := [ a, One( RightActingAlgebra( M ) ) ]^LEFT_RIGHT;
  return AsModuleElement( QuiverAlgebraAction( r, ax1 ), M );
end );

InstallMethod( \^,
               [ IsQuiverBimoduleElement, IsQuiverAlgebraElement ],
function( m, a )
  local M, r, 1xa;
  M := ModuleOfElement( m );
  r := UnderlyingRepresentationElement( m );
  1xa := [ One( LeftActingAlgebra( M ) ), a ]^LEFT_RIGHT;
  return AsModuleElement( QuiverAlgebraAction( r, 1xa ), M );
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

InstallMethod( AdditiveInverse, "for element of quiver module",
               [ IsQuiverModuleElement ],
function( e )
  return AsModuleElement( AdditiveInverse( UnderlyingRepresentationElement( e ) ),
                          ModuleOfElement( e ) );
end );

# basis of modules

BindGlobal( "FamilyOfQuiverModuleBases",
            NewFamily( "quiver module bases" ) );

DeclareRepresentation( "IsQuiverModuleBasisRep", IsComponentObjectRep and IsAttributeStoringRep,
                       [ ] );

InstallMethod( CanonicalBasis, "for quiver module",
               [ IsQuiverModule ],
function( M )
  local R, rep_basis, B;
  R := UnderlyingRepresentation( M );
  rep_basis := CanonicalBasis( R );
  B := rec( module := M,
            underlyingRepresentationBasis := rep_basis );
  ObjectifyWithAttributes( B,
                           NewType( FamilyOfQuiverModuleBases,
                                    IsBasis and IsQuiverModuleBasisRep ),
                           IsCanonicalBasis, true );
  return B;
end );

InstallMethod( Basis, "for quiver module",
               [ IsQuiverModule ],
               CanonicalBasis );

InstallMethod( Coefficients, "for canonical quiver module basis and quiver module element",
               [ IsCanonicalBasis and IsQuiverModuleBasisRep,
                 IsQuiverModuleElement ],
function( B, e )
  return AsList( e );
end );

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

InstallMethodWithSides( AsModuleCategory, [ IsQuiverRepresentationCategory ],
side -> function( rep_cat )
  local rep_algebra, Q, _R, _r, _M, _m, algebras, A, B, cat;

  rep_algebra := AlgebraOfCategory( rep_cat );
  Q := QuiverOfAlgebra( rep_algebra );

  _R := UnderlyingRepresentation;
  _r := UnderlyingRepresentationHomomorphism;
  _M := AsModule^side;
  _m := f -> AsModuleHomomorphism( side, f );

  if side = LEFT_RIGHT then
    algebras := TensorProductFactorsLeftRight( rep_algebra );
    A := algebras[ 1 ];
    B := algebras[ 2 ];
    cat := CreateCapCategory( Concatenation( "bimodules over ", String( A ), " and ", String( B ) ) );
    cat!.category_as_first_argument := true;
    SetFilterObj( cat, IsQuiverBimoduleCategory );
  else
    A := rep_algebra^side;
    algebras := [ fail, fail ];
    algebras[ Int( side ) ] := A;
    cat := CreateCapCategory( Concatenation( String( side ), " modules over ", String( A ) ) );
    cat!.category_as_first_argument := true;
    if side = LEFT then
      SetFilterObj( cat, IsLeftQuiverModuleCategory );
    else
      SetFilterObj( cat, IsRightQuiverModuleCategory );
    fi;
  fi;
  SetActingAlgebras( cat, algebras );
  SetSide( cat, side );
  SetUnderlyingRepresentationCategory( cat, rep_cat );
  SetIsAbelianCategory( cat, true );
  SetIsAbelianCategoryWithEnoughProjectives( cat, true );
  SetIsAbelianCategoryWithEnoughInjectives( cat, true );

  AddIsEqualForObjects( cat,
  function( category, M1, M2 )
    return IsEqualForObjects( _R( M1 ), _R( M2 ) );
  end );

  AddIsEqualForMorphisms( cat,
  function( category, m1, m2 )
    return IsEqualForMorphisms( _r( m1 ), _r( m2 ) );
  end );

  AddZeroObject( cat, function( category )
    return _M( ZeroObject( rep_cat ) );
  end );
  AddZeroMorphism( cat, function( category, M1, M2 )
    return _m( ZeroMorphism( _R( M1 ), _R( M2 ) ) );
  end );
  AddIdentityMorphism( cat, { category, M } -> _m( IdentityMorphism( _R( M ) ) ) );
  AddPreCompose( cat, function( category, m1, m2 )
    return _m( PreCompose( _r( m1 ), _r( m2 ) ) );
  end );
  AddAdditionForMorphisms( cat, function( category, m1, m2 )
    return _m( AdditionForMorphisms( _r( m1 ), _r( m2 ) ) );
  end );
  AddAdditiveInverseForMorphisms( cat, { category, m } -> _m( AdditiveInverseForMorphisms( _r( m ) ) ) );
  AddKernelEmbedding( cat, { category, m } -> _m( KernelEmbedding( _r( m ) ) ) );
  AddCokernelProjection( cat, { category, m } -> _m( CokernelProjection( _r( m ) ) ) );
  AddLiftAlongMonomorphism( cat, function( category, i, test )
    return _m( LiftAlongMonomorphism( _r( i ), _r( test ) ) );
  end );
  AddColiftAlongEpimorphism( cat, function( category, e, test )
    return _m( ColiftAlongEpimorphism( _r( e ), _r( test ) ) );
  end );
  AddProjectiveLift( cat, function( category, pi, epsilon )
    return _m( ProjectiveLift( _r( pi ), _r( epsilon ) ) );
  end );
  AddDirectSum( cat, function( category, summands )
    return _M( DirectSum( List( summands, _R ) ) );
  end );
  AddInjectionOfCofactorOfDirectSumWithGivenDirectSum( cat, function( category, summands, i, sum )
    return _m( InjectionOfCofactorOfDirectSumWithGivenDirectSum
               ( List( summands, _R ), i, _R( sum ) ) );
  end );
  AddProjectionInFactorOfDirectSumWithGivenDirectSum( cat, function( category, summands, i, sum )
    return _m( ProjectionInFactorOfDirectSumWithGivenDirectSum
               ( List( summands, _R ), i, _R( sum ) ) );
  end );

  AddEpimorphismFromSomeProjectiveObject( cat, { category, m } -> ProjectiveCover( m ) );
  AddMonomorphismIntoSomeInjectiveObject( cat, { category, m } -> InjectiveEnvelope( m ) );

  Finalize( cat );

  return cat;
end );

InstallMethodWithSides( ModuleCategory, [ "algebra" ],
side -> function( A )
  return AsModuleCategory( side, CategoryOfQuiverRepresentations( A^side ) );
end );

InstallMethod( BimoduleCategory, [ IsQuiverAlgebra, IsQuiverAlgebra ],
function( A, B )
  return BimoduleCategory( [ A, B ] );
end );

InstallMethod( ModuleCategory, "for dense list", [ IsDenseList ],
function( algebras )
  local side, A;
  if Length( algebras ) <> 2 then
    Error( "list of algebras must have length two" );
  fi;
  if IsQuiverAlgebra( algebras[ 1 ] ) and algebras[ 2 ] = fail then
    side := LEFT;
    A := algebras[ 1 ];
  elif algebras[ 1 ] = fail and IsQuiverAlgebra( algebras[ 2 ] ) then
    side := RIGHT;
    A := algebras[ 2 ];
  elif IsQuiverAlgebra( algebras[ 1 ] ) and IsQuiverAlgebra( algebras[ 2 ] ) then
    side := LEFT_RIGHT;
    A := algebras;
  else
    Error( "list of algebras must consist of quiver algebras or `fail`" );
  fi;
  return ( ModuleCategory ^ side )( A );
end );

InstallMethod( UnderlyingField, "for module category",
               [ IsQuiverModuleCategory ],
               cat -> UnderlyingField( UnderlyingRepresentationCategory( cat ) ) );

InstallMethod( UnderlyingRepresentationFunctor, "for a module category",
        [ IsQuiverModuleCategory ], 
        function( C )
    
    local   D,  underlying;
    
    D := UnderlyingRepresentationCategory( C );
    underlying := CapFunctor( "UnderlyingRepresentation", C, D );
    
    AddObjectFunction( underlying, UnderlyingRepresentation ); 
    AddMorphismFunction( underlying, 
            function( M, f, N ) return UnderlyingRepresentationHomomorphism( f ); end );
    
    return underlying;
end );

DeclareSideOperations( AsModuleFunctor, AsLeftModuleFunctor, AsRightModuleFunctor, AsBimoduleFunctor );

InstallMethodWithSides( AsModuleFunctor, 
        [ IsQuiverRepresentationCategory ],
side -> function( C )
    local   F;
    
    F := CapFunctor( "RepresentationModuleEquivalence", C, AsModuleCategory( side, C ) );
    
    AddObjectFunction( F, AsModule^side ); 
    AddMorphismFunction( F, 
            function( M, f, N ) return AsModuleHomomorphism( side, f ); end );
    
    return F;
end );

DeclareDirectionOperations( AsRepresentationOfModules,
                            AsRepresentationOfLeftModules, AsRepresentationOfRightModules );

InstallMethodWithDirections( AsRepresentationOfModules,
                             [ IsQuiverBimodule ],
dir -> function( M )
  local layered, cat;
  layered := AsLayeredRepresentation( Int( dir ), UnderlyingRepresentation( M ) );
  cat := ChangeBaseCategory( CapCategory( layered ),
                             ModuleCategory( dir, ActingAlgebra( dir, M ) ) );
  return MapRepresentation( AsModule^dir, AsModuleHomomorphism^dir, layered, cat );
end );

DeclareDirectionOperations( AsRepresentationOfModulesElement,
                            AsRepresentationOfLeftModulesElement, AsRepresentationOfRightModulesElement );

InstallMethodWithDirections( AsRepresentationOfModulesElement,
                             [ IsQuiverBimoduleElement ],
dir -> function( e )
  local M, RoM, e_, e_l;
  M := ModuleOfElement( e );
  RoM := AsRepresentationOfModules( dir, M );
  e_ := UnderlyingRepresentationElement( e );
  e_l := AsLayeredRepresentationElement( Int( dir ), e_ );
  return MapRepresentation( AsModuleElement^dir, e_l, RoM );
end );

InstallMethod( RepresentationOfModulesAsLayeredRepresentation,
               "for quiver representation",
               [ IsQuiverRepresentation ],
function( RoM )
  local cat, ucat_m, ucat_r, cat_l, layered;
  cat := CapCategory( RoM );
  ucat_m := VectorSpaceCategory( cat );
  ucat_r := UnderlyingRepresentationCategory( ucat_m );
  cat_l := CategoryOfQuiverRepresentationsOverVectorSpaceCategory
           ( AlgebraOfCategory( cat ), ucat_r );
  layered := MapRepresentation( UnderlyingRepresentation, UnderlyingRepresentationHomomorphism,
                                RoM, cat_l );
  return layered;
end );

InstallMethod( RepresentationOfModulesAsBimodule, "for quiver representation",
               [ IsQuiverRepresentation ],
function( RoM )
  local layered, s, flat;
  layered := RepresentationOfModulesAsLayeredRepresentation( RoM );
  s := Opposite( Side( VectorSpaceCategory( CapCategory( RoM ) ) ) );
  flat := AsFlatRepresentation( Int( s ), layered );
  return AsBimodule( flat );
end );

InstallMethod( RepresentationOfModulesElementAsBimoduleElement, "for quiver representation element",
               [ IsQuiverRepresentationElement ],
function( e_RoM )
  local RoM, layered, M, e_l, s, e_f;
  RoM := RepresentationOfElement( e_RoM );
  layered := RepresentationOfModulesAsLayeredRepresentation( RoM );
  M := RepresentationOfModulesAsBimodule( RoM );
  e_l := MapRepresentation( UnderlyingRepresentationElement, e_RoM, layered );
  s := Side( VectorSpaceCategory( CapCategory( RoM ) ) );
  e_f := AsFlatRepresentationElement( Int( s ), e_l );
  return AsBimoduleElement( e_f );
end );


InstallMethod ( AnnihilatorOfModule, 
"for a QuiverModule",
[ IsQuiverModule ],
function( M )
  local R; 
  
  R := UnderlyingRepresentation( M );
  
  return AnnihilatorOfRepresentation( R );
end
  );

#######################################################################
##
#O  IntersectionOfModules( <args> )
##
##                         f_i             
##  Given submodules  R_i -----> X  for i = 1,...,n of a module  X by 
##  n  monomorphism  f_i, this function computes the intersection of 
##  the images of all the  f_i.
##  
InstallMethod ( IntersectionOfModules, 
"for a list of IsQuiverModuleHomomorphisms",
[ IsDenseList ], 
function( list )

  local   side,  temp;
    
  if Length( list ) = 0 then
      return fail;
  fi;  
  side := Side( list[ 1 ] );
  temp := List( list, UnderlyingRepresentationHomomorphism );
  temp := IntersectionOfRepresentations( temp );
  
  return AsModuleHomomorphism( side, temp );
end
  );

InstallMethod( ProjectiveCover, "for a quiver module",
               [ IsQuiverModule ],
function( M )
  return AsModuleHomomorphism( Side( M ),
                               ProjectiveCover( UnderlyingRepresentation( M ) ) );
end );

InstallMethod( InjectiveEnvelope, "for a quiver module",
               [ IsQuiverModule ],
function( M )
  return AsModuleHomomorphism( Side( M ),
                               InjectiveEnvelope( UnderlyingRepresentation( M ) ) );
end );

#######################################################################
##
#O  RightAlgebraModuleToRightQuiverModule( <M> ) 
##  
##  This function constructs a right quiver module over a (quotient of a) 
##  path algebra  A  from a RightAlgebraModule over the same algebra  A.
##  The function checks if  A  actually is a quotient of a path algebra
##  if the module  M  is finite dimensional. In either cases it returns
##  and an error message. 
##
InstallMethod ( RightAlgebraModuleToRightQuiverModule, 
"for an RightAlgebraModule",
[ IsRightAlgebraModuleElementCollection ],
function( M )

  local A, vertices, num_vert, B, arrows, generators_in_vertices, i, 
        j, vertexwise_basis, dimensions, mat, arrow_list, vertices_Q, 
        a, partial_mat, source, target, source_index, target_index, 
        rows, cols, arrow, b, vector;
    
    if not IsFiniteDimensional( M ) then
        Error( "The entered module is not finite dimensional.\n" );
    fi;
    A := RightActingAlgebra( M ); 
    if not IsQuiverAlgebra( A ) then 
        Error( "The entered module is not a module over a quotient of a path algebra.\n" );
    fi;
    vertices := Vertices( QuiverOfAlgebra( A ) );
    vertices := List( vertices, x -> x * One( A ) );
    num_vert := Length( vertices ); 
    B := BasisVectors( Basis( M ) );
    arrows := Arrows( QuiverOfAlgebra( A ) );
    arrows := List( arrows, x -> x * One( A ) );    
    #
    #  Constructing a uniform basis of  M. 
    #
    generators_in_vertices := List( vertices, x -> [ ] ); 
    for i in [1..Length( B ) ] do
        for j in [ 1..num_vert ] do
            if ( B[ i ]^vertices[ j ] <> Zero( M ) ) then
                Add( generators_in_vertices[ j ], B[ i ]^vertices[ j ] );
            fi;
        od;
    od;    
    #
    #   Finding a K-basis of M in each vertex.
    #
    vertexwise_basis := List( generators_in_vertices, x -> Basis( Subspace( M, x ) ) );
    dimensions := List( vertexwise_basis, Length );
    # 
    #   Finding the matrices defining the representation
    # 
    mat := [];
    arrow_list := [ ]; 
    vertices_Q:= Vertices( QuiverOfAlgebra( A ) ); 
    for a in arrows do
        partial_mat := [];
        source := Source( LeadingPath( a!.representative ) );
        target := Target( LeadingPath( a!.representative ) );
        source_index := Position( vertices_Q, source );
        target_index := Position( vertices_Q, target );
        rows := Length( BasisVectors( vertexwise_basis[ source_index ] ) );
        cols := Length( BasisVectors( vertexwise_basis[ target_index ] ) );
        arrow := LeadingPath( a!.representative );
        if ( rows <> 0 and cols <> 0 ) then
            for b in BasisVectors( vertexwise_basis[ source_index ] ) do
                vector := Coefficients( vertexwise_basis[ target_index ], b^a ); 
                Add( partial_mat, vector );
            od;
            Add( arrow_list, arrow );
            Add( mat, partial_mat );
        fi;
    od;
    
    return RightQuiverModule( A, dimensions, arrow_list, mat );
end
  );

#######################################################################
##
#O  LeftAlgebraModuleToLeftQuiverModule( <M> ) 
##  
##  This function constructs a left quiver module over a (quotient of a) 
##  path algebra  A  from a LeftAlgebraModule over the same algebra  A.
##  The function checks if  A  actually is a quotient of a path algebra
##  if the module  M  is finite dimensional. In either cases it returns
##  and an error message. 
##
InstallMethod ( LeftAlgebraModuleToLeftQuiverModule, 
"for an LeftAlgebraModule",
[ IsLeftAlgebraModuleElementCollection ],
function( M )

  local A, vertices, num_vert, B, arrows, generators_in_vertices, i, 
        j, vertexwise_basis, dimensions, mat, arrow_list, vertices_Q, 
        a, partial_mat, source, target, source_index, target_index, 
        rows, cols, arrow, b, vector;
    
    if not IsFiniteDimensional( M ) then
        Error( "The entered module is not finite dimensional.\n" );
    fi;
    A := LeftActingAlgebra( M ); 
    if not IsQuiverAlgebra( A ) then 
        Error( "The entered module is not a module over a quotient of a path algebra.\n" );
    fi;
    vertices := Vertices( QuiverOfAlgebra( A ) );
    vertices := List( vertices, x -> x * One( A ) );
    num_vert := Length( vertices ); 
    B := BasisVectors( Basis( M ) );
    arrows := Arrows( QuiverOfAlgebra( A ) );
    arrows := List( arrows, x -> x * One( A ) );    
    #
    #  Constructing a uniform basis of  M. 
    #
    generators_in_vertices := List( vertices, x -> [ ] ); 
    for i in [1..Length( B ) ] do
        for j in [ 1..num_vert ] do
            if ( vertices[ j ]^B[ i ] <> Zero( M ) ) then
                Add( generators_in_vertices[ j ], vertices[ j ]^B[ i ] );
            fi;
        od;
    od;    
    #
    #   Finding a K-basis of M in each vertex.
    #
    vertexwise_basis := List( generators_in_vertices, x -> Basis( Subspace( M, x ) ) );
    dimensions := List( vertexwise_basis, Length );
    # 
    #   Finding the matrices defining the representation
    # 
    mat := [];
    arrow_list := [ ]; 
    vertices_Q:= Vertices( QuiverOfAlgebra( A ) ); 
    for a in arrows do
        partial_mat := [];
        source := Source( LeadingPath( a!.representative ) );
        target := Target( LeadingPath( a!.representative ) );
        source_index := Position( vertices_Q, source );
        target_index := Position( vertices_Q, target );
        rows := Length( BasisVectors( vertexwise_basis[ source_index ] ) );
        cols := Length( BasisVectors( vertexwise_basis[ target_index ] ) );
        arrow := LeadingPath( a!.representative );
        if ( rows <> 0 and cols <> 0 ) then
            for b in BasisVectors( vertexwise_basis[ source_index ] ) do
                vector := Coefficients( vertexwise_basis[ target_index ], a^b ); 
                Add( partial_mat, vector );
            od;
            Add( arrow_list, arrow );
            Add( mat, partial_mat );
        fi;
    od;
    mat := List( mat, TransposedMat ); 
    
    return LeftQuiverModule( A, dimensions, arrow_list, mat );
end
  );

InstallMethod( FromEndMToHomMM, 
"for endomorphism algebra of a module to the space of homomorphisms", 
[ IsQuiverModule, IsMatrix ],
function( M, x )
    local   f;
    
    f := FromEndRToHomRR( UnderlyingRepresentation( M ), x );
    
    return AsModuleHomomorphism( Side( M ), f );
end
  );

InstallOtherMethod( EndomorphismAlgebra,
"for a IsQuiverModule",
[ IsQuiverModule ],
function( M )
    
    return EndomorphismAlgebra( UnderlyingRepresentation( M ) );
end
  );
