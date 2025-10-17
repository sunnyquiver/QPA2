BindGlobal( "FamilyOfSides", NewFamily( "sides" ) );

DeclareRepresentation( "IsSideRep", IsComponentObjectRep and IsAttributeStoringRep,
                       [ "name", "number" ] );

BindGlobal( "LEFT",
              Objectify( NewType( FamilyOfSides, IsDirection and IsSide and IsSideRep ),
                         rec( name := "LEFT", str := "left", number := 1 ) ) );

BindGlobal( "RIGHT",
              Objectify( NewType( FamilyOfSides, IsDirection and IsSide and IsSideRep ),
                         rec( name := "RIGHT", str := "right", number := 2 ) ) );

BindGlobal( "LEFT_RIGHT",
              Objectify( NewType( FamilyOfSides, IsSide and IsSideRep ),
                         rec( name := "LEFT_RIGHT", str := "left-right", number := 3 ) ) );

InstallMethod( \=, "for sides", [ IsSide, IsSide ], IsIdenticalObj );

InstallMethod( Opposite, "for direction", [ IsDirection ],
function( d )
  if d = LEFT then
    return RIGHT;
  else
    return LEFT;
  fi;
end );

BindGlobal( "OPERATIONS_WITH_SIDE_VERSIONS", rec() );

InstallMethod( String, "for side", [ IsSide ],
               side -> side!.str );

InstallMethod( Int, "for side", [ IsSide ],
               side -> side!.number );

InstallMethod( DeclareDirectionOperations, [ IsOperation, IsOperation, IsOperation ],
function( op, left_op, right_op )
  OPERATIONS_WITH_SIDE_VERSIONS.( NameFunction( op ) ) := [ left_op, right_op, fail ];
end );

InstallMethod( DeclareSideOperations, [ IsOperation, IsOperation, IsOperation, IsOperation ],
function( op, left_op, right_op, left_right_op )
  OPERATIONS_WITH_SIDE_VERSIONS.( NameFunction( op ) ) := [ left_op, right_op, left_right_op ];
end );

InstallMethod( \^, [ IsOperation, IsSide ],
function( op, side )
  local ops, name;
  ops := OPERATIONS_WITH_SIDE_VERSIONS;
  name := NameFunction( op );
  if IsBound( ops.( name ) ) then
    return ops.( name )[ Int( side ) ];
  else
    return fail;
  fi;
end );

InstallMethod( InstallMethodWithDirections, [ IsOperation, IsDenseList, IsFunction ],
function( op, filters, f )
  local dir;
  InstallMethod( op, Concatenation( [ IsDirection ], filters ),
  function( arg )
    local dir;
    dir := arg[ 1 ];
    return CallFuncList( op^dir, arg{ [ 2 .. Length( arg ) ] } );
  end );
  for dir in [ LEFT, RIGHT ] do
    InstallMethod( op^dir, filters, f( dir ) );
  od;
end );

InstallMethod( InstallMethodWithSides, [ IsOperation, IsDenseList, IsFunction ],
function( op, filters, f )
  local side;
  InstallMethod( op, Concatenation( [ IsSide ], ReplaceObj( filters, "algebra", IsQuiverAlgebra ) ),
  function( arg )
    local side;
    side := arg[ 1 ];
    return CallFuncList( op^side, arg{ [ 2 .. Length( arg ) ] } );
  end );
  for side in [ LEFT, RIGHT ] do
    InstallMethod( op^side, ReplaceObj( filters, "algebra", IsQuiverAlgebra ), f( side ) );
  od;
  InstallMethod( op^LEFT_RIGHT, ReplaceObj( filters, "algebra", IsDenseList ),
                 f( LEFT_RIGHT ) );
  # InstallMethod( op^LEFT_RIGHT, Flat( ReplaceObj( filters, IsQuiverAlgebra, [ IsQuiverAlgebra, IsQuiverAlgebra ] ) ),
  #                f( LEFT_RIGHT ) );
end );
