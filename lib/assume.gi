InstallMethod( Assume, "for object, function and string",
               [ IsObject, IsFunction, IsString ],
function( obj, f, msg )
  if not f( obj ) then
    Error( msg, ": ", obj );
  fi;
end );

InstallMethod( AssumeAll, "for list, function and string",
               [ IsList, IsFunction, IsString ],
function( list, f, msg )
  local obj;
  for obj in list do
    if not f( obj ) then
      Error( msg, ": ", obj );
    fi;
  od;
end );

