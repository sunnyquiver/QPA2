BindGlobal( "FamilyOfEnrichedQuiverModuleHomomorphisms",
            NewFamily( "enriched quiver module homomorphisms" ) );
BindGlobal( "FamilyOfHomModules",
            CollectionsFamily( FamilyOfEnrichedQuiverModuleHomomorphisms ) );

# We need this because the builtin global function Image uses
# the FamilySource attribute to check whether a given map can
# be applied to a given element.
SetFamilySource( FamilyOfEnrichedQuiverModuleHomomorphisms,
                 FamilyOfQuiverModuleElements );

InstallMethod( HomFunctor, "for side and quiver module categories",
               [ IsSide, IsQuiverModuleCategory, IsQuiverModuleCategory ],
function( side, cat1, cat2 )
  local K, acting_algebras, range, hom_functor, hom_alg, hom_cat;
  if UnderlyingField( cat1 ) <> UnderlyingField( cat2 ) then
    Error( "categories over different fields" );
  fi;
  K := UnderlyingField( cat1 );
  if ActingAlgebra( side, cat1 ) <> ActingAlgebra( side, cat2 ) then
    Error( "incompatible categories for ", side, " hom functor" );
  fi;
  hom_alg := ActingAlgebra( side, cat1 );
  if side = Side( cat1 ) and side = Side( cat2 ) then
    return HomFunctor( cat1 );
  fi;
  if hom_alg <> fail then
    hom_cat := ModuleCategory( side, hom_alg );
  else
    hom_cat := CategoryOfVectorSpaces( K );
  fi;
  if side = LEFT then
    acting_algebras := [ RightActingAlgebra( cat1 ), RightActingAlgebra( cat2 ) ];
  else # side = RIGHT
    acting_algebras := [ LeftActingAlgebra( cat2 ), LeftActingAlgebra( cat1 ) ];
  fi;
  range := ModuleCategory( acting_algebras );
  hom_functor := CapFunctor( "Hom", [ [ cat1, true ], [ cat2, false ] ], range );
  AddObjectFunction( hom_functor, function( M, N )
    local hom, type;
    hom := rec();
    type := NewType( FamilyOfHomModules,
                     IsHomModule and IsComponentObjectRep and IsAttributeStoringRep );
    ObjectifyWithAttributes( hom, type,
                             Source, M,
                             Range, N,
                             HomCategory, hom_cat,
                             UnderlyingHomFunctor, HomFunctor( hom_cat ),
                             HomSide, side );
    Add( range, hom );
    return Intern( hom );
  end );
  AddMorphismFunction( hom_functor, function( hom1, f, g, hom2 )
    local X1, X2, Y1, Y2;
    #     f
    # X1 --> X2
    #       /
    #      /
    #     / h
    #    /
    #   .
    # Y1 --> Y2
    #     g
    #
    # Hom( f, g ) : Hom( X2, Y1 ) -> Hom( X1, Y2 )
    #                       h    |-> PreCompose( [ f, h, g ] )
    X1 := Source( f ); X2 := Range( f );
    Y1 := Source( g ); Y2 := Range( g );
    return QuiverModuleHomomorphism( hom1, hom2,
                                     h -> PreCompose( f, h, g ) );
  end );
  return hom_functor;
end );

InstallMethod( UnderlyingRepresentation,
               [ IsHomModule ],
function( hom )
  local side, uhom, M, N, rep_cat, M_, N_, rep;
  side := HomSide( hom );
  uhom := UnderlyingHomFunctor( hom );
  M := Source( hom );
  N := Range( hom );
  rep_cat := UnderlyingRepresentationCategory( CapCategory( hom ) );
  if Side( M ) = LEFT_RIGHT and Side( N ) = LEFT_RIGHT then
    M_ := AsRepresentationOfModules( side, M );
    N_ := AsRepresentationOfModules( side, N );
    rep := MapRepresentation( uhom, [ M_, N_ ] );
    if side = RIGHT then
      rep := TensorFlipRestriction( rep );
    fi;
  elif Side( M ) = LEFT_RIGHT then
    M_ := AsRepresentationOfModules( side, M );
    rep := MapRepresentation( Mi -> Hom( Mi, N ), M_, rep_cat );
  elif Side( N ) = LEFT_RIGHT then
    N_ := AsRepresentationOfModules( side, N );
    rep := MapRepresentation( Ni -> Hom( M, Ni ), N_, rep_cat );
  else
    M_ := UnderlyingRepresentation( M );
    N_ := UnderlyingRepresentation( N );
    rep := MapRepresentation( uhom, [ M_, N_ ] );
    if side = RIGHT then
      rep := TensorFlipRestriction( rep );
    fi;
  fi;
  return rep;
end );

InstallMethod( EnrichedQuiverModuleHomomorphism,
               [ IsSide, IsQuiverModule, IsQuiverModule, IsList ],
function( side, M, N, morphisms )
  local hom;
  hom := Hom( side, M, N );
  return QuiverModuleElement( hom, morphisms );
end );

InstallMethod( AsModuleElement,
               [ IsQuiverRepresentationElement, IsHomModule ],
function( e, hom )
  local f, elem_cat;
  if RepresentationOfElement( e ) <> UnderlyingRepresentation( hom ) then
    Error( "Element is not from the underlying representation of the module" );
  fi;
  elem_cat := IsQuiverModuleElement ^ Side( hom );
  f := rec();
  ObjectifyWithAttributes( f, NewType( FamilyOfQuiverModuleElements,
                                        elem_cat and IsEnrichedQuiverModuleHomomorphism and IsQuiverModuleRep ),
                           UnderlyingRepresentationElement, e,
                           ModuleOfElement, hom,
                           Side, Side( hom ) );
  return f;
end );

InstallMethod( HomSide, "for enriched quiver module homomorphism",
               [ IsEnrichedQuiverModuleHomomorphism ],
               f -> HomSide( ModuleOfElement( f ) ) );

InstallMethod( Source, "for enriched quiver module homomorphism",
               [ IsEnrichedQuiverModuleHomomorphism ],
               f -> Source( ModuleOfElement( f ) ) );

InstallMethod( Range, "for enriched quiver module homomorphism",
               [ IsEnrichedQuiverModuleHomomorphism ],
               f -> Range( ModuleOfElement( f ) ) );

InstallMethod( String, "for enriched quiver module homomorphism",
               [ IsEnrichedQuiverModuleHomomorphism ],
function( f )
  return Concatenation( "enriched ", String( HomSide( f ) ), " homomorphism ",
                        "(", String( Source( f ) ), ")",
                        "->",
                        "(", String( Range( f ) ), ")" );
end );

InstallMethod( ImageElm, "for enriched quiver module homomorphism and quiver module element",
               [ IsEnrichedQuiverModuleHomomorphism, IsQuiverModuleElement ],
function( f, e )
  #TODO
end );

# TODO precompose, isomorphism{to,from}{left,right}module

InstallMethod( HomFunctor, "for quiver module category",
               [ IsQuiverModuleCategory ],
function( cat )
  local rep, rep_cat, hom_rep;
  rep := UnderlyingRepresentationFunctor( cat );
  rep_cat := UnderlyingRepresentationCategory( cat );
  hom_rep := HomFunctor( rep_cat );
  return PreComposeFunctors( [ rep, rep ], hom_rep );
end );

InstallMethod( HomFunctor, "for side, quiver module and quiver module category",
               [ IsSide, IsQuiverModule, IsQuiverModuleCategory ],
function( side, M, cat )
  return FixFunctorArguments( HomFunctor( side, CapCategory( M ), cat ),
                              [ M, fail ] );
end );

InstallMethod( HomFunctor, "for side, quiver module and quiver module category",
               [ IsSide, IsQuiverModuleCategory, IsQuiverModule ],
function( side, cat, M )
  return FixFunctorArguments( HomFunctor( side, cat, CapCategory( M ) ),
                              [ fail, M ] );
end );

InstallMethod( Hom, "for side and quiver modules",
               [ IsSide, IsQuiverModule, IsQuiverModule ],
function( side, M, N )
  return ApplyFunctor( HomFunctor( side, CapCategory( M ), CapCategory( N ) ),
                       M, N );
end );

InstallMethod( Hom, "for side, quiver module and quiver module homomorphism",
               [ IsSide, IsQuiverModule, IsQuiverModuleHomomorphism ],
function( side, M, g )
  return ApplyFunctor( HomFunctor( side, CapCategory( M ), CapCategory( g ) ),
                       IdentityMorphism( M ), g );
end );

InstallMethod( Hom, "for side, quiver module homomorphism and quiver module",
               [ IsSide, IsQuiverModuleHomomorphism, IsQuiverModule ],
function( side, f, N )
  return ApplyFunctor( HomFunctor( side, CapCategory( f ), CapCategory( N ) ),
                       f, IdentityMorphism( N ) );
end );

InstallMethod( Hom, "for side and quiver module homomorphisms",
               [ IsSide, IsQuiverModuleHomomorphism, IsQuiverModuleHomomorphism ],
function( side, f, g )
  return ApplyFunctor( HomFunctor( side, CapCategory( f ), CapCategory( g ) ),
                       f, g );
end );

