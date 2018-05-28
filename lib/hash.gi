# hack to install value which is a small integer
# (for which InstallValue does not work):
MakeReadWriteGlobal( "HashMod" );
HashMod := 2 ^ 32;
MakeReadOnlyGlobal( "HashMod" );

InstallMethod( Hash, "for string", [ IsString ],
function( s )
  local h, i, N, n, c;
  N := HashMod;
  n := [ 1, 256, 256 ^ 2, 256 ^ 3 ];
  h := 0;
  i := 1;
  for c in s do
    h := ( h + IntChar( c ) * n[ i ] ) mod N;
    i := ( i mod 4 ) + 1;
  od;
  return h;
end );

InstallMethod( Hash, "for integer", [ IsInt ],
function( i )
  return i mod HashMod;
end );

InstallMethod( Hash, "for object", [ IsObject ],
function( obj )
  return Hash( String( obj ) );
end );
