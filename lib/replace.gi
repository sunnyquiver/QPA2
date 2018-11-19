InstallMethod( Replace, "for dense list, positive integer and object",
               [ IsDenseList, IsPosInt, IsObject ],
function( list, i, obj )
  local res;
  if i > Length( list ) then
    Error( "index out of bounds" );
  fi;
  res := ShallowCopy( list );
  res[ i ] := obj;
  return res;
end );

InstallMethod( Replace, "for dense lists",
               [ IsDenseList, IsDenseList, IsDenseList ],
function( list, is, objs )
  local res;
  if Length( is ) <> Length( objs ) then
    Error( "list of indices and list of objects have different lengths" );
  fi;
  if not ForAll( is, IsPosInt ) then
    Error( "list of indices contains elements that are not positive integers" );
  fi;
  if not ForAll( is, i -> i <= Length( list ) ) then
    Error( "index out of bounds" );
  fi;
  res := ShallowCopy( list );
  res{ is } := objs;
  return res;
end );

InstallMethod( ReplaceObj, [ IsList, IsObject, IsObject ],
function( list, value, replacement )
  return List( list,
               function( x ) if x = value then return replacement; else return x; fi; end );
end );
