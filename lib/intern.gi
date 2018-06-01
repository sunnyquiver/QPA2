BindGlobal( "INTERN_HASH_TABLE_SIZE", 256 );
BindGlobal( "INTERN_HASH_TABLE",
            List( [ 1 .. INTERN_HASH_TABLE_SIZE ], i -> WeakPointerObj( [] ) ) );

InstallMethod( Intern, "for object", [ IsObject ],
function( obj )
  local H, h, wp, i, interned, new_wp_list;
  H := Hash( obj );
  h := ( H mod INTERN_HASH_TABLE_SIZE ) + 1;
  wp := INTERN_HASH_TABLE[ h ];
  for i in [ 1 .. LengthWPObj( wp ) ] do
    interned := ElmWPObj( wp, i );
    if interned <> fail and interned = obj then
      return interned;
    fi;
  od;
  SetFilterObj( obj, IsInterned );
  new_wp_list := [ obj ];
  for i in [ 1 .. LengthWPObj( wp ) ] do
    interned := ElmWPObj( wp, i );
    if interned <> fail then
      Add( new_wp_list, interned );
    fi;
  od;
  INTERN_HASH_TABLE[ h ] := WeakPointerObj( new_wp_list );
  return obj;
end );

InstallMethod( Intern, "for interned object", [ IsInterned ], IdFunc );

InstallMethod( \=, "for interned objects",
               [ IsInterned, IsInterned ],
               SUM_FLAGS, # should be ranked above all other methods
               IsIdenticalObj );
