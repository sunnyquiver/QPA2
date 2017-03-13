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
               [ IsQuiverRepresentationHomomorphism, IsString ],
function( rf, side )
  local R1, R2, rep_algebra, cat, hom_type, M1, M2, matrices, f;
  R1 := Source( rf );
  R2 := Range( rf );
  M1 := AsModule( R1, side );
  M2 := AsModule( R2, side );
  rep_algebra := AlgebraOfRepresentation( R1 );
  cat := AsModuleCategory( CapCategory( R1 ), side );
  if side = LEFT_RIGHT then
    hom_type := IsQuiverBimoduleHomomorphism;
  elif side = LEFT then
    hom_type := IsLeftQuiverModuleHomomorphism;
  else
    hom_type := IsRightQuiverModuleHomomorphism;
  fi;
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
               f -> AsModuleHomomorphism( f, LEFT ) );

InstallMethod( AsRightModuleHomomorphism,
               [ IsQuiverRepresentationHomomorphism ],
               f -> AsModuleHomomorphism( f, RIGHT ) );

InstallMethod( AsBimoduleHomomorphism,
               [ IsQuiverRepresentationHomomorphism ],
               f -> AsModuleHomomorphism( f, LEFT_RIGHT ) );

InstallMethod( QuiverModuleHomomorphism,
               [ IsQuiverModule, IsQuiverModule, IsList ],
function( M1, M2, matrices )
  local R1, R2, rf;
  # TODO check that M1 and M2 are in same category
  R1 := UnderlyingRepresentation( M1 );
  R2 := UnderlyingRepresentation( M2 );
  rf := QuiverRepresentationHomomorphism( R1, R2, matrices );
  return AsModuleHomomorphism( rf, Side( M1 ) );
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
