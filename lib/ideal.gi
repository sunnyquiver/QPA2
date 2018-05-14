# BindGlobal( "FamilyOfQuiverAlgebraIdeals", NewFamily( "quiver algebra ideals" ) );
# TODO fix algebra families

DeclareSideOperations( IsQuiverAlgebraIdeal,
                       IsQuiverAlgebraLeftIdeal, IsQuiverAlgebraRightIdeal,
                       IsQuiverAlgebraTwoSidedIdeal );

DeclareSideOperations( QuiverAlgebraIdeal,
                       LeftIdealByGenerators, RightIdealByGenerators, TwoSidedIdealByGenerators );

InstallMethodWithSides( QuiverAlgebraIdeal, [ IsQuiverAlgebra, IsDenseList ],
side -> function( A, gens )
  local inc, I, type;
  if not IsFiniteDimensional( A ) then
    Error( "algebra is not finite-dimensional" ); # TODO
  fi;
  inc := SubmoduleInclusion( AlgebraAsModule( side, A ),
                             List( gens, AsModuleElement ^ side ) );
  I := rec();
  type := NewType( FamilyObj( A ),
                   IsQuiverAlgebraIdeal ^ side
                   and IsComponentObjectRep
                   and IsAttributeStoringRep );
  ObjectifyWithAttributes( I, type,
                           Generators, gens,
                           IdealAsModule, Source( inc ),
                           IdealAsSubmoduleOfAlgebra, inc );
  return I;
end );

InstallMethod( GeneratorsOfLeftIdeal, [ IsQuiverAlgebraLeftIdeal ], Generators );
InstallMethod( GeneratorsOfRightIdeal, [ IsQuiverAlgebraRightIdeal ], Generators );
InstallMethod( GeneratorsOfTwoSidedIdeal, [ IsQuiverAlgebraTwoSidedIdeal ], Generators );

InstallMethod( \in, [ IsQuiverAlgebraElement, IsQuiverAlgebraIdeal ],
function( a, I )
  return IdealElementAsModuleElement( a, I ) <> fail;
end );

InstallMethod( IdealElementAsModuleElement, [ IsQuiverAlgebraElement, IsQuiverAlgebraIdeal ],
function( a, I )
  local inc, M;
  inc := IdealAsSubmoduleOfAlgebra( I );
  M := IdealAsModule( I );
  return PreImagesRepresentative( inc, AsModuleElement( Side( M ), a ) );
end );

InstallMethod( ModuleElementAsIdealElement, [ IsQuiverModuleElement, IsQuiverAlgebraIdeal ],
function( m, I )
  local inc;
  inc := IdealAsSubmoduleOfAlgebra( I );
  return AsAlgebraElement( Image( inc, m ) );
end );

InstallMethod( IsAdmissibleIdeal, [ IsPathIdeal ],
function( I )
  local gens, kQ, Q, A, is_reducible, path_length, paths, next_paths, p, a;

  gens := GeneratorsOfIdeal( I );
  
  # check I \subseteq J^2:
  if not ForAll( gens, g -> ForAll( Paths( g ), IsCompositePath ) ) then
    return false;
  fi;

  # check J^t \subseteq I:
  kQ := LeftActingRingOfIdeal( I );
  Q := QuiverOfAlgebra( kQ );
  A := kQ / I;

  if not IsFiniteDimensional( A ) then
    return false;
  fi;

  is_reducible :=
    p ->
    ( Representative( PathAsAlgebraElement( A, p ) )
      <> PathAsAlgebraElement( PathAlgebra( A ), p ) );

  path_length := 1;
  paths := Arrows( Q );
  next_paths := [];

  while not ForAll( paths, is_reducible ) do
    for p in paths do
      for a in OutgoingArrows( Target( p ) ) do
        Add( next_paths, ComposePaths( p, a ) );
      od;
    od;
    path_length := path_length + 1;
    paths := next_paths;
    next_paths := [];
  od;

  return ForAll( paths, p -> IsZero( PathAsAlgebraElement( A, p ) ) );
end );
