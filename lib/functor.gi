InstallMethod( PreComposeFunctors,
               [ IsCapFunctor, IsCapFunctor ],
function( F, G )
  local sig_F, sig_G, range_F, range_G, sig, i, n, name, result, 
        object_fun, morphism_fun;
  sig_F := InputSignature( F );
  sig_G := InputSignature( G );
  range_F := AsCapCategory( Range( F ) );
  range_G := AsCapCategory( Range( G ) );
  if Length( sig_G ) > 1 then
    Error( "functor to be composed must be unary" );
  fi;
  if not IsIdenticalObj( sig_G[ 1 ][ 1 ], range_F ) then
    Error( "non-composable functors" );
  fi;
  sig := List( sig_F, ShallowCopy );
  if sig_G[ 1 ][ 2 ] then # G is contravariant
    for i in [ 1 .. Length( sig ) ] do
      sig[ i ][ 2 ] := not sig[ i ][ 2 ];
    od;
  fi;
  n := Length( sig );

  name := Concatenation( "Composition of ", Name( F ), " and ", Name( G ) );

  result := CapFunctor( name, sig, range_G );

  object_fun := function( arg )
    local result_of_F;
    result_of_F := CallFuncList( ApplyFunctor, Concatenation( [ F ], arg ) );
    return ApplyFunctor( G, result_of_F );
  end;
  AddObjectFunction( result, object_fun );

  morphism_fun := function( arg )
    local result_of_F;
    result_of_F := CallFuncList( ApplyFunctor, Concatenation( [ F ], arg{ [ 2 .. n + 1 ] } ) );
    return ApplyFunctor( G, result_of_F );
  end;
  AddMorphismFunction( result, morphism_fun );

  return result;
end );


InstallMethod( PreComposeFunctors,
               [ IsDenseList, IsCapFunctor ],
function( Fs, G )
  local sig_G, sig_Fs, range_G, n_G, i, argument_indices, acc, sig, n, 
        j, name, result, object_fun, morphism_fun;
  sig_G := InputSignature( G );
  sig_Fs := List( Fs, InputSignature );
  range_G := AsCapCategory( Range( G ) );

  if Length( sig_G ) <> Length( Fs ) then
    Error( "length of list does not match arity of functor" );
  fi;
  n_G := Length( sig_G );
  for i in [ 1 .. n_G ] do
    if not IsIdenticalObj( AsCapCategory( Range( Fs[ i ] ) ),
                           sig_G[ i ][ 1 ] ) then
      Error( "non-composable functors" );
    fi;
  od;

  argument_indices := [];
  acc := 0;
  for i in [ 1 .. n_G ] do
    argument_indices[ i ] := [ acc + 1, acc + Length( sig_Fs[ i ] ) ];
    acc := acc + Length( sig_Fs[ i ] );
  od;
  
  sig := Concatenation( sig_Fs );
  n := Length( sig );
  for i in [ 1 .. n_G ] do
    sig[ i ] := ShallowCopy( sig[ i ] );
    if sig_G[ i ][ 2 ] then # G is contravariant in its i-th argument
      for j in [ argument_indices[ i ][ 1 ] .. argument_indices[ i ][ 2 ] ] do
        sig[ j ][ 2 ] := not sig[ j ][ 2 ];
      od;
    fi;
  od;

  name := Concatenation( "Composition of ",
                         JoinStringsWithSeparator( List( Fs, Name ), "," ),
                         " and ", Name( G ) );

  result := CapFunctor( name, sig, range_G );

  object_fun := function( arg )
    local result_of_Fs, i, args_i;
    result_of_Fs := [];
    for i in [ 1 .. n_G ] do
      args_i := arg{ [ argument_indices[ i ][ 1 ] .. argument_indices[ i ][ 2 ] ] };
      result_of_Fs[ i ] := CallFuncList( ApplyFunctor, Concatenation( [ Fs[ i ] ], args_i ) );
    od;
    return CallFuncList( ApplyFunctor, Concatenation( [ G ], result_of_Fs ) );
  end;
  AddObjectFunction( result, object_fun );
  
  morphism_fun := function( arg )
    local args, result_of_Fs, i, args_i;
    args := arg{ [ 2 .. n + 1 ] };
    result_of_Fs := [];
    for i in [ 1 .. n_G ] do
      args_i := args{ [ argument_indices[ i ][ 1 ] .. argument_indices[ i ][ 2 ] ] };
      result_of_Fs[ i ] := CallFuncList( ApplyFunctor, Concatenation( [ Fs[ i ] ], args_i ) );
    od;
    return CallFuncList( ApplyFunctor, Concatenation( [ G ], result_of_Fs ) );
  end;
  AddMorphismFunction( result, morphism_fun );

  return result;
end );

InstallMethod( FixFunctorArguments,
               [ IsCapFunctor, IsDenseList ],
function( F, args )
  local sig_F, n, sig, blank_positions, i, name, object_fun, 
        args_id_morphisms, morphism_fun, fixed_F;
  sig_F := InputSignature( F );
  if Length( sig_F ) <> Length( args ) then
    Error( "length of argument list does not match arity of functor" );
  fi;
  n := Length( sig_F );
  sig := [];
  blank_positions := [];
  for i in [ 1 .. n ] do
    if args[ i ] = fail then
      Add( blank_positions, i );
      Add( sig, sig_F[ i ] );
    elif not IsIdenticalObj( CapCategory( args[ i ] ), sig_F[ i ][ 1 ] ) then
      Error( "fixed argument at position ", i, " is in wrong category" );
    fi;
  od;

  name := Concatenation( Name( F ), "(" );
  for i in [ 1 .. n ] do
    if args[ i ] = fail then
      name := Concatenation( name, "-" );
    elif HasName( args[ i ] ) then
      name := Concatenation( name, Name( args[ i ] ) );
    else
      name := Concatenation( name, String( args[ i ] ) );
    fi;
    if i < n then
      name := Concatenation( name, "," );
    fi;
  od;
  name := Concatenation( name, ")" );
  
  object_fun := function( arg )
    local args_F;
    args_F := ShallowCopy( args );
    args_F{ blank_positions } := arg;
    return CallFuncList( ApplyFunctor, Concatenation( [ F ], args_F ) );
  end;
  args_id_morphisms := [];
  for i in [ 1 .. n ] do
    if args[ i ] = fail then
      args_id_morphisms[ i ] := fail;
    else
      args_id_morphisms[ i ] := IdentityMorphism( args[ i ] );
    fi;
  od;
  morphism_fun := function( arg )
    local args_F;
    args_F := ShallowCopy( args_id_morphisms );
    args_F{ blank_positions } := arg{ [ 2 .. Length( arg ) - 1 ] };
    return CallFuncList( ApplyFunctor, Concatenation( [ F ], args_F ) );
  end;

  fixed_F := CapFunctor( name, sig, AsCapCategory( Range( F ) ) );
  AddObjectFunction( fixed_F, object_fun );
  AddMorphismFunction( fixed_F, morphism_fun );
  return fixed_F;
end );
