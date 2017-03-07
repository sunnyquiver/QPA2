BindGlobal( "FamilyOfQuiverModuleHomomorphisms",
            NewFamily( "quiver module homomorphisms" ) );

# We need this because the builtin global function Image uses
# the FamilySource attribute to check whether a given map can
# be applied to a given element.
SetFamilySource( FamilyOfQuiverModuleHomomorphisms,
                 FamilyOfQuiverModuleElements );

DeclareRepresentation( "IsQuiverModuleHomomorphismRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [] );

InstallMethod( AsModuleHomomorphism,
               [ IsQuiverRepresentationHomomorphism, IsQuiverAlgebra ],
function( rf, A )
  local R1, R2, rep_algebra, cat, hom_type, M1, M2, matrices, f;
  R1 := Source( rf );
  R2 := Range( rf );
  rep_algebra := AlgebraOfRepresentation( R1 );
  if A = AlgebraForLeftModules( rep_algebra ) then
    hom_type := IsLeftQuiverModuleHomomorphism;
    cat := AsCategoryOfLeftModules( CapCategory( R1 ) );
  elif A = AlgebraForRightModules( rep_algebra ) then
    hom_type := IsRightQuiverModuleHomomorphism;
    cat := AsCategoryOfRightModules( CapCategory( R1 ) );
  else
    Error( "Representation homomorphism is not over the given algebra or its opposite" );
  fi;
  M1 := AsModule( R1, A );
  M2 := AsModule( R2, A );
  matrices := MatricesOfRepresentationHomomorphism( rf );
  f := rec();
  ObjectifyWithAttributes( f, NewType( FamilyOfQuiverModuleHomomorphisms,
                                       hom_type and IsQuiverModuleHomomorphismRep ),
                           UnderlyingRepresentationHomomorphism, rf,
                           MatricesOfModuleHomomorphism, matrices,
                           Source, M1,
                           Range, M2 );
  Add( cat, f );
  return f;
end );

InstallMethod( AsLeftModuleHomomorphism,
               [ IsQuiverRepresentationHomomorphism ],
function( f )
  local A;
  A := AlgebraOfRepresentation( Source( f ) );
  return AsModuleHomomorphism( f, AlgebraForLeftModules( A ) );
end );

InstallMethod( AsRightModuleHomomorphism,
               [ IsQuiverRepresentationHomomorphism ],
function( f )
  local A;
  A := AlgebraOfRepresentation( Source( f ) );
  return AsModuleHomomorphism( f, AlgebraForRightModules( A ) );
end );

InstallMethod( QuiverModuleHomomorphism,
               [ IsLeftQuiverModule, IsLeftQuiverModule, IsList ],
function( M1, M2, matrices )
  local R1, R2, rf;
  R1 := UnderlyingRepresentation( M1 );
  R2 := UnderlyingRepresentation( M2 );
  rf := QuiverRepresentationHomomorphism( R1, R2, matrices );
  return AsLeftModuleHomomorphism( rf );
end );

InstallMethod( QuiverModuleHomomorphism,
               [ IsRightQuiverModule, IsRightQuiverModule, IsList ],
function( M1, M2, matrices )
  local R1, R2, rf;
  R1 := UnderlyingRepresentation( M1 );
  R2 := UnderlyingRepresentation( M2 );
  rf := QuiverRepresentationHomomorphism( R1, R2, matrices );
  return AsRightModuleHomomorphism( rf );
end );

InstallMethod( ImageElm,
               [ IsQuiverModuleHomomorphism, IsQuiverModuleElement ],
function( f, m )
  local M, r, F;
  M := ModuleOfElement( m );
  r := UnderlyingRepresentationElement( m );
  F := UnderlyingRepresentationHomomorphism( f );
  return AsModuleElement( ImageElm( F, r ), M );
end );
