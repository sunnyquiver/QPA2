#! @Chapter Quivers

BindGlobal( "FamilyOfPaths", NewFamily( "paths" ) );
BindGlobal( "FamilyOfQuivers", CollectionsFamily( FamilyOfPaths ) );

DeclareRepresentation( "IsVertexRep", IsComponentObjectRep and IsAttributeStoringRep,
                       [ "quiver", "number" ] );
DeclareRepresentation( "IsArrowRep", IsComponentObjectRep and IsAttributeStoringRep,
                       [ "quiver", "label", "number", "source", "target" ] );
DeclareRepresentation( "IsCompositePathRep", IsComponentObjectRep,
                       [ "arrows" ] );
DeclareRepresentation( "IsQuiverRep", IsComponentObjectRep and IsAttributeStoringRep,
		       [ "label", "vertices", "arrows", "primitive_paths",
		         "vertices_desc", "arrows_desc"] );

InstallMethod( DecomposeQuiverDescriptionString,
               [ IsString ],
function( string )
  local label, vertices, arrows, i, part, c, errmsg;
  errmsg := function( msg )
    return Concatenation( msg, " at position ", String( i ),
                          " in quiver description string \"",
                          string, "\"" );
  end;
  part := 0; # 000000111111111233333334
             # label(vertices)[arrows]
  label := "";
  vertices := "";
  arrows := "";
  for i in [ 1 .. Length( string ) ] do
    c := string[ i ];
    if c = '(' then
      if part = 0 then
        part := 1;
      else
        Error( errmsg( "Unexpected '('" ) );
      fi;
    elif c = ')' then
      if part = 1 then
        part := 2;
      else
        Error( errmsg( "Unexpected ')'" ) );
      fi;
    elif c = '[' then
      if part = 0 or part = 2 then
        part := 3;
      else
        Error( errmsg( "Unexpected '['" ) );
      fi;
    elif c = ']' then
      if part = 3 then
        part := 4;
      else
        Error( errmsg( "Unexpected ']'" ) );
      fi;
    elif part = 0 then
      Add( label, c );
    elif part = 1 then
      Add( vertices, c );
    elif part = 2 then
      Error( errmsg( "Expected '['" ) );
    elif part = 3 then
      Add( arrows, c );
    elif part = 4 then
      Error( errmsg( "Expected end of string" ) );
    else
      Error( errmsg( "Internal error" ) );
    fi;
  od;
  return [ label, vertices, arrows ];
end );

InstallMethod( ParseStringAsLabel,
               [ IsString ],
function( string )
  local num;
  if Length( string ) = 0 then
    Error( "Empty label" );
  fi;
  num := Int( string );
  if num <> fail then
    return num;
  elif Length( string ) = 1 and IsAlphaChar( string[ 1 ] ) then
    return string[ 1 ];
  else
    return string;
  fi;
end );

InstallMethod( ParseLabelPatternString,
               [ IsString ],
function( string )
  local c, num, i, prefix, num_start;
  if Length( string ) = 0 then
    Error( "Empty label pattern" );
  fi;
  num := Int( string );
  if num <> fail then
    return [ IsInt, num - 1 ];
  fi;
  if Length( string ) = 1 then
    c := string[ 1 ];
    if IsAlphaChar( c ) then
      return [ IsChar, IntChar( c ) - 1 ];
    else
      Error( "One-char label pattern must be letter or digit, not ", c );
    fi;
  fi;
  num_start := fail;
  for i in [ 1 .. Length( string ) ] do
    c := string[ i ];
    if IsDigitChar( c ) or c = '-' then
      num_start := i;
      break;
    fi;
  od;
  num := fail;
  if num_start <> fail then
    prefix := string{ [ 1 .. ( num_start - 1 ) ] };
    num := Int( string{ [ num_start .. Length( string ) ] } );
  fi;
  if num = fail then
    Error( "Bad string label pattern \"", string, "\": does not end with an integer" );
  fi;
  return [ IsString, num - 1, prefix ];
end );  

InstallMethod( ApplyLabelPattern,
               [ IsDenseList, IsPosInt ],
function( pattern, i )
  local type, init, num;
  type := pattern[ 1 ];
  init := pattern[ 2 ];
  num := init + i;
  if type = IsInt then
    return num;
  elif type = IsChar then
    if num > 255 or not IsAlphaChar( CharInt( num ) ) then
      Error( "Too high value (", i, ") for character label pattern ", pattern );
    fi;
    return CharInt( num );
  elif type = IsString then
    return Concatenation( pattern[ 3 ], String( num ) );
  else
    Error( "Bad label pattern object ", pattern );
  fi;
end );

InstallMethod( MakeLabelsFromPatternObj,
               [ IsDenseList, IsPosInt, IsList ],
function( pattern, n, given_labels )
  local make_label;
  make_label := function( i )
    if IsBound( given_labels[ i ] ) then
      return given_labels[ i ];
    else
      return ApplyLabelPattern( pattern, i );
    fi;
  end;
  return List( [ 1 .. n ], make_label );
end );

InstallMethod( MakeLabelsFromPattern,
               [ IsString, IsPosInt, IsList ],
function( pattern, n, given_labels )
  return MakeLabelsFromPatternObj( ParseLabelPatternString( pattern ),
                                   n, given_labels );
end );

InstallMethod( MakeLabelsFromPattern,
               [ IsString, IsPosInt ],
function( pattern, n )
  return MakeLabelsFromPattern( pattern, n, [] );
end );

InstallMethod( ParseQuiverLabelString,
               [ IsString ],
function( string )
  local quiver_label, vertices_str, arrows_str,
        vertices_pattern, arrows_pattern, tmp;
  tmp := DecomposeQuiverDescriptionString( string );
  quiver_label := tmp[ 1 ];
  vertices_str := tmp[ 2 ];
  arrows_str := tmp[ 3 ];
  if vertices_str <> "" then
    vertices_pattern := ParseLabelPatternString( vertices_str );
  else
    vertices_pattern := fail;
  fi;
  if arrows_str <> "" then
    arrows_pattern := ParseLabelPatternString( arrows_str );
  else
    arrows_pattern := fail;
  fi;
  return [ quiver_label, vertices_pattern, arrows_pattern ];
end );

InstallMethod( ParseArrowDescriptionString,
               [ IsString ],
function( string )
  local split, label, source, target, source_int, target_int;
  split := SplitString( string, ":" );
  if Length( split ) = 2 then
    label := ParseStringAsLabel( split[ 1 ] );
    split := SplitStringSubstring( split[ 2 ], "->" );
    if Length( split ) = 2 then
      source := ParseStringAsLabel( split[ 1 ] );
      target := ParseStringAsLabel( split[ 2 ] );
      return [ label, source, target ];
    fi;
  fi;
  Error( "Bad arrow description string \"", string, "\"" );
end );

InstallMethod( ParseVerticesDescriptionString,
               [ IsString ],
function( string )
  local split, patterns, num;
  num := Int( string );
  if num <> fail then
    return num;
  fi;
  split := SplitStringSubstring( string, ".." );
  if Length( split ) = 2 then
    patterns := List( split, ParseLabelPatternString );
    if patterns[ 1 ][ 1 ] = patterns[ 2 ][ 1 ] and
       patterns[ 1 ][ 2 ] <= patterns[ 2 ][ 2 ] and
       ( patterns[ 1 ][ 1 ] <> IsString or ( patterns[ 1 ][ 3 ] = patterns[ 2 ][ 3 ] ) ) then
      num := patterns[ 2 ][ 2 ] - patterns[ 1 ][ 2 ] + 1;
      return List( [ 1 .. num ], i -> ApplyLabelPattern( patterns[ 1 ], i ) );
    fi;
  elif Length( split ) = 1 then
    return List( SplitString( string, "," ), ParseStringAsLabel );
  fi;
  Error( "Bad vertices string \"", string, "\"" );
end );

InstallMethod( ParseQuiverDescriptionString,
               [ IsString ],
function( string )
  local quiver_label, vertices, arrows, tmp;
  tmp := DecomposeQuiverDescriptionString( string );
  quiver_label := tmp[ 1 ];
  vertices := ParseVerticesDescriptionString( tmp[ 2 ] );
  arrows := List( SplitString( tmp[ 3 ], "," ),
                  ParseArrowDescriptionString );
  return [ quiver_label, vertices, arrows ];
end );

InstallMethod( SplitStringSubstring,
               [ IsString, IsString ],
function( string, sep )
  local sep_len, str_len, i, split, start;
  sep_len := Length( sep );
  str_len := Length( string );
  split := [];
  start := 1;
  i := 1;
  while i <= str_len - sep_len + 1 do
    if string{ [ i .. ( i + sep_len - 1 ) ] } = sep then
      Add( split, string{ [ start .. ( i - 1 ) ] } );
      start := i + sep_len;
      i := start;
    else
      i := i + 1;
    fi;
  od;
  Add( split, string{ [ start .. str_len ] } );
  return split;
end );

InstallMethod( Quiver,
               [ IsFunction, IsString, IsPosInt, IsDenseList ],
function( quiver_cat, label_with_patterns, num_vertices, arrows )
  local tmp, label, vertex_label_pattern, arrow_label_pattern;
  tmp := DecomposeQuiverDescriptionString( label_with_patterns );
  label := tmp[ 1 ];
  vertex_label_pattern := tmp[ 2 ];
  arrow_label_pattern := tmp[ 3 ];
  return Quiver( quiver_cat,
                 [ label, vertex_label_pattern, arrow_label_pattern ],
                 num_vertices, [], arrows );
end );

InstallMethod( Quiver,
               [ IsFunction, IsString, IsDenseList, IsDenseList ],
function( quiver_cat, label_with_patterns, vertex_labels, arrows )
  local tmp, label, vertex_label_pattern, arrow_label_pattern;
  tmp := DecomposeQuiverDescriptionString( label_with_patterns );
  label := tmp[ 1 ];
  vertex_label_pattern := tmp[ 2 ];
  arrow_label_pattern := tmp[ 3 ];
  return Quiver( quiver_cat,
                 [ label, vertex_label_pattern, arrow_label_pattern ],
                 Length( vertex_labels ), vertex_labels, arrows );
end );

InstallMethod( Quiver, "for function and string",
               [ IsFunction, IsString ],
function( quiver_cat, description )
  return CallFuncList( Quiver,
                       Concatenation( [ quiver_cat ],
                                      ParseQuiverDescriptionString( description ) ) );
end );

InstallMethod( Quiver,
               [ IsFunction, IsDenseList,
                 IsPosInt, IsList, IsDenseList ],
function( quiver_cat, label_with_patterns_list,
          num_vertices, vertex_labels, arrows )
  local label, vertex_pattern, arrow_pattern, vertex_pattern_str, arrow_pattern_str,
        make_vertex_label, all_vertex_labels,  
        set_arrow_label, arrows_with_labels, arrow_labels, get_vertex_index,  
        source_indices, target_indices;
  if Length( label_with_patterns_list ) <> 3 or
     ( not ForAll( label_with_patterns_list, IsString ) ) then
    Error( "Argument 'label_with_patterns_list' must be a list of three strings" );
  fi;
  label := label_with_patterns_list[ 1 ];
  vertex_pattern_str := label_with_patterns_list[ 2 ];
  arrow_pattern_str := label_with_patterns_list[ 3 ];
  if vertex_pattern_str = "" then
    vertex_pattern := [ IsInt, 0 ]; # gives labels 1, 2, 3, ...
  else
    vertex_pattern := ParseLabelPatternString( vertex_pattern_str );
  fi;
  if arrow_pattern_str = "" then
    arrow_pattern := fail;
  else
    arrow_pattern := ParseLabelPatternString( arrow_pattern_str );
  fi;
  make_vertex_label := function( i )
    if IsBound( vertex_labels[ i ] ) then
      return vertex_labels[ i ];
    else
      return ApplyLabelPattern( vertex_pattern, i );
    fi;
  end;
  all_vertex_labels := List( [ 1 .. num_vertices ], make_vertex_label );
  set_arrow_label := function( i )
    if Length( arrows[ i ] ) = 3 then
      return arrows[ i ];
    elif Length( arrows[ i ] ) <> 2 then
      Error( "Bad arrow specification (should be list of length two or three): ",
             arrows[ i ] );
    elif arrow_pattern = fail then
      Error( "Arrow ", arrows[ i ],
             " without label and no arrow label pattern given" );
    else
      return [ ApplyLabelPattern( arrow_pattern, i ),
               arrows[ i ][ 1 ], arrows[ i ][ 2 ] ];
    fi;
  end;
  arrows_with_labels := List( [ 1 .. Length( arrows ) ], set_arrow_label );
  arrow_labels := List( arrows_with_labels, a -> a[ 1 ] );
  get_vertex_index := function( obj )
    local i;
    for i in [ 1 .. num_vertices ] do
      if all_vertex_labels[ i ] = obj then
        return i;
      fi;
    od;
    if IsPosInt( obj ) and obj <= num_vertices then
      return obj;
    fi;
    Error( "No vertex named ", obj );
  end;
  source_indices := List( arrows_with_labels, a -> get_vertex_index( a[ 2 ] ) );
  target_indices := List( arrows_with_labels, a -> get_vertex_index( a[ 3 ] ) );
  return Quiver( quiver_cat, label, all_vertex_labels, arrow_labels,
                 source_indices, target_indices );
end );

InstallMethod( Quiver,
               [ IsFunction, IsObject, IsDenseList, IsDenseList,
                 IsDenseList, IsDenseList ],
function( quiver_cat, label, vertex_labels, arrow_labels,
          source_indices, target_indices )
  local num_vertices, num_arrows, path_cat, quiver_type,
        Q, vertex_type, make_vertex, arrow_type, make_arrow,
        incoming_arrows, outgoing_arrows, i;
  num_vertices := Length( vertex_labels );
  num_arrows := Length( arrow_labels );
  if quiver_cat = IsLeftQuiver then
    path_cat := IsLeftPath;
  elif quiver_cat = IsRightQuiver then
    path_cat := IsRightPath;
  else
    Error( "First argument to Quiver must be either IsLeftQuiver or IsRightQuiver" );
  fi;
  if num_vertices = 0 then
    Error( "Quiver must have at least one vertex" );
  fi;
  if Length( source_indices ) <> num_arrows then
    Error( "Wrong number of source indices",
           " (is ", Length( source_indices ), ", should be ", num_arrows, ")" );
  fi;
  if Length( target_indices ) <> num_arrows then
    Error( "Wrong number of target indices",
           " (is ", Length( target_indices ), ", should be ", num_arrows, ")" );
  fi;
  if Length( label ) = 0 then
    Error( "Empty quiver label" );
  fi;

  quiver_type := NewType( FamilyOfQuivers, quiver_cat and IsQuiverRep );
  Q := Objectify( quiver_type,
                  rec( label := label ) );

  vertex_type := NewType( FamilyOfPaths, IsVertex and IsVertexRep and path_cat );
  make_vertex := function( num, label )
    return Objectify( vertex_type,
                      rec( quiver := Q,
                           number := num,
                           label := label ) );
  end;
  Q!.vertices := ListN( [ 1 .. num_vertices ], vertex_labels,
                        make_vertex );

  arrow_type := NewType( FamilyOfPaths, IsArrow and IsArrowRep and path_cat );
  make_arrow := function( num, label, source_index, target_index )
    if ( not IsPosInt( source_index ) ) or source_index > num_vertices then
      Error( "Source of arrow ", label, " is not int in correct range" );
    fi;
    if ( not IsPosInt( target_index ) ) or target_index > num_vertices then
      Error( "Target of arrow ", label, " is not int in correct range" );
    fi;
    return Objectify( arrow_type,
                      rec( quiver := Q,
                           number := num,
                           label := label,
                           source := Q!.vertices[ source_index ],
                           target := Q!.vertices[ target_index ] ) );
  end;
  Q!.arrows := ListN( [ 1 .. num_arrows ], arrow_labels, source_indices, target_indices,
                      make_arrow );

  Q!.primitive_paths := Concatenation( Q!.vertices, Q!.arrows );

  outgoing_arrows := [];
  incoming_arrows := [];
  for i in [ 1 .. num_vertices ] do
    outgoing_arrows[ i ] := [];
    incoming_arrows[ i ] := [];
  od;
  for i in [ 1 .. num_arrows ] do
    Add( outgoing_arrows[ source_indices[ i ] ], Q!.arrows[ i ] );
    Add( incoming_arrows[ target_indices[ i ] ], Q!.arrows[ i ] );
  od;
  for i in [ 1 .. num_vertices ] do
    SetOutgoingArrows( Q!.vertices[ i ], outgoing_arrows[ i ] );
    SetIncomingArrows( Q!.vertices[ i ], incoming_arrows[ i ] );
  od;

  return Q;
end );

CallFuncList(
function()
  local left_quiver_func, right_quiver_func, filter_lists, fl;
  left_quiver_func := function( arg )
    return CallFuncList( Quiver, Concatenation( [ IsLeftQuiver ], arg ) );
  end;
  right_quiver_func := function( arg )
    return CallFuncList( Quiver, Concatenation( [ IsRightQuiver ], arg ) );
  end;
  filter_lists := [ [ IsString, IsPosInt, IsDenseList ],
                    [ IsString, IsDenseList, IsDenseList ],
                    [ IsString ],
                    [ IsDenseList, IsPosInt, IsList, IsDenseList ],
                    [ IsObject, IsDenseList, IsDenseList, IsDenseList, IsDenseList ] ];
  for fl in filter_lists do
    InstallMethod( LeftQuiver, fl, left_quiver_func );
    InstallMethod( RightQuiver, fl, right_quiver_func );
  od;
end, [] );

# InstallMethod( LeftQuiver, "for string, positive integer and list",
#                [ IsString, IsPosInt, IsDenseList ],
# function( label, num_vertices, arrows )
#   return Quiver( IsLeftQuiver, label, num_vertices, arrows );
# end );

# InstallMethod( LeftQuiver, "for string and lists",
#                [ IsString, IsDenseList, IsDenseList ],
# function( label, vertices, arrows )
#   return Quiver( IsLeftQuiver, label, vertices, arrows );
# end );

# InstallMethod( LeftQuiver, "for string",
#                [ IsString ],
# function( label )
#   return Quiver( IsLeftQuiver, label );
# end );

# InstallMethod( RightQuiver, "for string, positive integer and list",
#                [ IsString, IsPosInt, IsDenseList ],
# function( label, num_vertices, arrows )
#   return Quiver( IsRightQuiver, label, num_vertices, arrows );
# end );

# InstallMethod( RightQuiver, "for string and lists",
#                [ IsString, IsDenseList, IsDenseList ],
# function( label, vertices, arrows )
#   return Quiver( IsRightQuiver, label, vertices, arrows );
# end );

# InstallMethod( RightQuiver, "for string",
#                [ IsString ],
# function( label )
#   return Quiver( IsRightQuiver, label );
# end );

InstallMethod( QuiverOfPath,
               "for vertex",
	       [ IsVertex and IsVertexRep ],
function( a ) return a!.quiver; end );
InstallMethod( QuiverOfPath,
               "for arrow",
	       [ IsArrow and IsArrowRep ],
function( a ) return a!.quiver; end );
InstallMethod( QuiverOfPath,
               "for composite path",
	       [ IsCompositePath and IsCompositePathRep ],
function( a ) return QuiverOfPath( a!.arrows[1] ); end );

InstallMethod( Source,
               "for vertex",
	       [ IsVertex and IsVertexRep ],
function( v ) return v; end );
InstallMethod( Source,
               "for arrow",
	       [ IsArrow and IsArrowRep ],
function( a ) return a!.source; end );
InstallMethod( Source,
               "for composite path",
	       [ IsCompositePath and IsCompositePathRep ],
function( p )
  return Source( p!.arrows[ 1 ] );
end );

InstallMethod( Target,
               "for vertex",
	       [ IsVertex and IsVertexRep ],
function( v ) return v; end );
InstallMethod( Target,
               "for arrow",
	       [ IsArrow and IsArrowRep ],
function( a ) return a!.target; end );
InstallMethod( Target,
               "for composite path",
	       [ IsCompositePath and IsCompositePathRep ],
function( p )
  return Target( p!.arrows[ Length( p!.arrows ) ] );
end );

InstallMethod( LeftEnd, "for left path", [ IsLeftPath ], Target );
InstallMethod( RightEnd, "for left path", [ IsLeftPath ], Source );
InstallMethod( LeftEnd, "for right path", [ IsRightPath ], Source );
InstallMethod( RightEnd, "for right path", [ IsRightPath ], Target );

InstallMethod( Length,
               "for vertex",
	       [ IsVertex and IsVertexRep ],
function( v ) return 0; end );
InstallMethod( Length,
               "for arrow",
	       [ IsArrow and IsArrowRep ],
function( a ) return 1; end );
InstallMethod( Length,
               "for composite path",
	       [ IsCompositePath and IsCompositePathRep ],
function( p )
  return Length( p!.arrows );
end );

InstallMethod( ArrowList,
               "for vertex",
	       [ IsVertex and IsVertexRep ],
function( v ) return []; end );
InstallMethod( ArrowList,
               "for arrow",
	       [ IsArrow and IsArrowRep ],
function( a ) return [ a ]; end );
InstallMethod( ArrowList,
               "for composite path",
	       [ IsCompositePath and IsCompositePathRep ],
function( p )
  return p!.arrows;
end );

InstallMethod( ArrowListLR, "for left path", [ IsLeftPath ],
function( p )
  return Reversed( ArrowList( p ) );
end );

InstallMethod( ArrowListLR, "for right path", [ IsRightPath ], ArrowList );

InstallMethod( AsList,
               "for vertex",
	       [ IsVertex and IsVertexRep ],
function( v ) return [ v ]; end );
InstallMethod( AsList,
               "for arrow",
	       [ IsArrow and IsArrowRep ],
function( a ) return [ a ]; end );
InstallMethod( AsList,
               "for composite path",
	       [ IsCompositePath and IsCompositePathRep ],
function( p )
  return p!.arrows;
end );

InstallMethod( AsListLR, "for left path", [ IsLeftPath ],
function( p )
  return Reversed( AsList( p ) );
end );

InstallMethod( AsListLR, "for right path", [ IsRightPath ], AsList );

InstallMethod( Label,
               "for vertex",
	       [ IsVertex and IsVertexRep ],
function( v )
  return v!.label;
end );

InstallMethod( TranslatePath,
               [ IsPrimitivePath, IsFunction ],
function( p, f )
  return f( p );
end );

InstallMethod( TranslatePath,
               [ IsCompositePath, IsFunction ],
function( p, f )
  return PathFromArrowList( List( ArrowList( p ), f ) );
end );

InstallMethod( Label,
               "for arrow",
	       [ IsArrow and IsArrowRep ],
function( a )
  return a!.label;
end );

InstallGlobalFunction( QPA_LABEL_TO_STRING,
function( label )
  if IsString( label ) then
    return label;
  elif IsChar( label ) then
    return [ label ];
  elif IsList( label ) then
    return JoinStringsWithSeparator( List( label, QPA_LABEL_TO_STRING ),
                                     "x" );
  else
    return String( label );
  fi;
end );

InstallMethod( LabelAsString, "for primitive path",
               [ IsPrimitivePath ],
function( p )
  return QPA_LABEL_TO_STRING( Label( p ) );
end );

InstallMethod( LabelAsString, "for quiver",
               [ IsQuiver ],
function( Q )
  return QPA_LABEL_TO_STRING( Label( Q ) );
end );

InstallMethod( VertexNumber,
               "for vertex",
	       [ IsVertex and IsVertexRep ],
function( v )
  return v!.number;
end );

InstallMethod( ArrowNumber,
               "for arrow",
	       [ IsArrow and IsArrowRep ],
function( a )
  return a!.number;
end );

InstallMethod( Composable,
               "for two paths",
	       [ IsPath, IsPath ],
function( p1, p2 )
  return Target( p1 ) = Source( p2 );
end );

InstallMethod( ComposableLR,
               "for two paths",
	       [ IsPath, IsPath ],
function( p1, p2 )
  return RightEnd( p1 ) = LeftEnd( p2 );
end );

InstallMethod( ComposePaths2,
               "for vertex and path",
	       [ IsVertex, IsPath ],
function( v, p )
  if Composable( v, p ) then
    return p;
  else
    return fail;
  fi;
end );

InstallMethod( ComposePaths2,
               "for path and vertex",
	       [ IsPath, IsVertex ],
function( p, v )
  if Composable( p, v ) then
    return p;
  else
    return fail;
  fi;
end );

InstallMethod( ComposePaths2,
               "for two arrows",
	       [ IsArrow, IsArrow ],
function( a1, a2 )
  if Composable( a1, a2 ) then
    return PathFromArrowList( [ a1, a2 ] );
  else
    return fail;
  fi;
end );

InstallMethod( ComposePaths2,
               "for two nontrivial paths",
	       [ IsNontrivialPath, IsNontrivialPath ],
function( p1, p2 )
  if Composable( p1, p2 ) then
    return PathFromArrowList( Concatenation( ArrowList( p1 ), ArrowList( p2 ) ) );
  else
    return fail;
  fi;
end );

InstallOtherMethod( ComposePaths2,
                    "for path and bool",
                    [ IsPath, IsBool ],
function( p, x )
  if x = fail then
    return fail;
  fi;
  TryNextMethod();
end );

InstallOtherMethod( ComposePaths2,
                    "for bool and path",
                    [ IsBool, IsPath ],
function( x, p )
  if x = fail then
    return fail;
  fi;
  TryNextMethod();
end );

InstallGlobalFunction( ComposePaths,
function( arg )
  local list;
  if Length( arg ) = 1 and IsList( arg[ 1 ] ) then
    list := arg[ 1 ];
  else
    list := arg;
  fi;
  return Iterated( list, ComposePaths2 );
end );

InstallGlobalFunction( ComposePathsLR,
function( arg )
  local list;
  if Length( arg ) = 1 and IsList( arg[ 1 ] ) then
    list := arg[ 1 ];
  else
    list := arg;
  fi;
  return Iterated( list, \* );
end );

#! @Section Composition of paths

#! @BeginChunk PathMultiplication
#! @Description
#!  Compose the paths <A>p1</A> and <A>p2</A> in the multiplication order
#!  of the quiver.
#!  In a left-oriented quiver, we have <C>p * q = ComposePaths( q, p )</C>
#!  for any paths <C>p</C> and <C>q</C>.
#!  In a right-oriented quiver, we have <C>p * q = ComposePaths( p, q )</C>
#!  for any paths <C>p</C> and <C>q</C>.
#! @Returns <C>IsPath</C> or <C>fail</C>
InstallMethod( \*, "for two left paths",
               [ IsLeftPath, IsLeftPath ],
function( p1, p2 )
  return ComposePaths2( p2, p1 );
end );
#! @EndChunk PathMultiplication

InstallMethod( \*, "for two right paths",
               [ IsRightPath, IsRightPath ],
               ComposePaths2 );

InstallOtherMethod( \*, "for path and bool",
                    [ IsPath, IsBool ],
function( p, x )
  if x = fail then
    return fail;
  fi;
  TryNextMethod();
end );

InstallOtherMethod( \*, "for bool and path",
                    [ IsBool, IsPath ],
function( x, p )
  if x = fail then
    return fail;
  fi;
  TryNextMethod();
end );

InstallMethod( PathFromArrowListNC,
               "for list",
               [ IsList ],
function( list )
  local pathType;
  if Length( list ) > 1 then
    if IsLeftPath( list[ 1 ] ) then
      pathType := IsLeftPath;
    else
      pathType := IsRightPath;
    fi;
    return Objectify( NewType( FamilyObj( list[ 1 ] ),
                               pathType and IsCompositePath and IsCompositePathRep ),
                      rec( arrows := list ) );
  elif Length( list ) = 1 then
    return list[ 1 ];
  else
    return fail;
  fi;
end );

InstallMethod( PathFromArrowList, "for list", [ IsList ],
function( list )
  local i;
  for i in [ 1 .. Length( list ) - 1 ] do
    if not Composable( list[ i ], list[ i + 1 ] ) then
      Error( "Arrows ", list[ i ], " and ", list[ i + 1 ], " are not composable" );
    fi;
  od;
  return PathFromArrowListNC( list );
end );

InstallMethod( PathFromArrowListLR, "for list", [ IsList ],
function( list )
  if IsLeftPath( list[ 1 ] ) then
    return PathFromArrowList( Reversed( list ) );
  else
    return PathFromArrowList( list );
  fi;
end );

InstallMethod( Subpath, "for vertex and integers",
               [ IsVertex, IsInt, IsInt ],
function( v, from, to )
  if from = 0 and to = 0 then
    return v;
  else
    Error( "Bad bounds (", from, ", ", to, ") for subpath of vertex" );
  fi;
end );

InstallMethod( Subpath, "for arrow and integers",
               [ IsArrow, IsInt, IsInt ],
function( a, from, to )
  if from = 0 and to = 0 then
    return Source( a );
  elif from = 0 and to = 1 then
    return a;
  elif from = 1 and to = 1 then
    return Target( a );
  else
    Error( "Bad bounds (", from, ", ", to, ") for subpath of arrow" );
  fi;
end );

InstallMethod( Subpath, "for composite path and integers",
               [ IsCompositePath, IsInt, IsInt ],
function( p, from, to )
  local list, len;
  list := ArrowList( p );
  len := Length( list );
  if from < 0 or to < from or to > len then
    Error( "Bad bounds (", from, ", ", to, ") ",
           "for subpath of path ", p );
  fi;    
  if from = to then
    if from = 0 then
      return Source( p );
    else
      return Target( list[ from ] );
    fi;
  else
    return PathFromArrowList( list{ [ ( from + 1 ) .. to ] } );
  fi;
end );

InstallMethod( SubpathLR, "for left path and integers",
               [ IsLeftPath, IsInt, IsInt ],
function( p, from, to )
  local len;
  len := Length( p );
  return Subpath( p, len - to, len - from );
end );

InstallMethod( SubpathLR, "for right path and integers",
               [ IsRightPath, IsInt, IsInt ],
               Subpath );

InstallMethod( \<, "for paths",
               IsIdenticalObj,
               [ IsPath, IsPath ],
function( p1, p2 )
  local a1, a2, i;
  if Length( p1 ) < Length( p2 ) then
    return true;
  elif Length( p1 ) > Length( p2 ) then
    return false;
  elif IsVertex( p1 ) then
    return VertexNumber( p1 ) < VertexNumber( p2 );
  elif IsArrow( p1 ) then
    return ArrowNumber( p1 ) < ArrowNumber( p2 );
  else
    a1 := ArrowList( p1 );
    a2 := ArrowList( p2 );
    for i in [ 1 .. Length( a1 ) ] do
      if a1[ i ] < a2[ i ] then
        return true;
      elif a1[ i ] > a2[ i ] then
        return false;
      fi;
    od;
    return false;
  fi;
end );

InstallMethod( SubpathIndex, "for two vertices",
               IsIdenticalObj,
               [ IsVertex, IsVertex ],
function( v1, v2 )
  if v1 = v2 then
    return 0;
  else
    return fail;
  fi;
end );

InstallMethod( SubpathIndex, "for vertex and nontrivial path",
               IsIdenticalObj,
               [ IsVertex, IsNontrivialPath ],
               ReturnFail );

InstallMethod( SubpathIndex, "for nontrivial path and vertex",
               IsIdenticalObj,
               [ IsNontrivialPath, IsVertex ],
function( p, v )
  local a, i, list;
  if Source( p ) = v then
    return 0;
  fi;
  list := ArrowList( p );
  for i in [ 1 .. Length( list ) ] do
    a := list[ i ];
    if Target( a ) = v then
      return i;
    fi;
  od;
  return fail;
end );

InstallMethod( SubpathIndex, "for two nontrivial paths",
               IsIdenticalObj,
               [ IsNontrivialPath, IsNontrivialPath ],
function( p, q )
  local i, j, list_p, list_q, len_p, len_q, match,
        arrows_left, arrows_right, left, right;
  list_p := ArrowList( p );
  list_q := ArrowList( q );
  len_p := Length( list_p );
  len_q := Length( list_q );
  if len_q > len_p then
    return fail;
  fi;
  for i in [ 1 .. ( len_p - len_q + 1 ) ] do
    match := true;
    for j in [ 1 .. len_q ] do
      if list_p[ i + j - 1 ] <> list_q[ j ] then
        match := false;
        break;
      fi;
    od;
    if match then
      return i - 1;
    fi;
  od;
  return fail;
end );

InstallMethod( SubpathIndexLR, "for left paths",
               [ IsLeftPath, IsLeftPath ],
function( p, q )
  local index;
  index := SubpathIndex( p, q );
  if index = fail then
    return fail;
  else
    return Length( p ) - ( index + Length( q ) );
  fi;
end );

InstallMethod( SubpathIndexLR, "for right paths",
               [ IsRightPath, IsRightPath ],
               SubpathIndex );

InstallMethod( ExtractSubpath, "for two paths",
               [ IsPath, IsPath ],
function( p, q )
  local index;
  index := SubpathIndex( p, q );
  if index = fail then
    return fail;
  else
    return [ Subpath( p, 0, index ),
             Subpath( p, index + Length( q ), Length( p ) ) ];
  fi;
end );

InstallMethod( \/, "for left paths",
               [ IsLeftPath, IsLeftPath ],
function( p, q )
  local quotients;
  quotients := ExtractSubpath( p, q );
  if quotients = fail then
    return fail;
  else
    return [ quotients[ 2 ], quotients[ 1 ] ];
  fi;
end );

InstallMethod( \/, "for right paths",
               [ IsRightPath, IsRightPath ],
               ExtractSubpath );

InstallMethod( PathOverlaps, "for vertex and path",
               [ IsVertex, IsPath ],
function( v, p )
  return [];
end );

InstallMethod( PathOverlaps, "for path and vertex",
               [ IsPath, IsVertex ],
function( p, v )
  return [];
end );

InstallMethod( PathOverlaps, "for two nontrivial paths",
               [ IsNontrivialPath, IsNontrivialPath ],
function( p, q )
  local list_p, list_q, len_p, len_q, overlaps, overlap_len,
        is_overlap, i, b, c;
  list_p := ArrowListLR( p );
  list_q := ArrowListLR( q );
  len_p := Length( list_p );
  len_q := Length( list_q );
  overlaps := [];
  for overlap_len in [ 1 .. Minimum( len_p, len_q ) ] do
    is_overlap := true;
    for i in [ 1 .. overlap_len ] do
      if list_q[ i ] <> list_p[ len_p - overlap_len + i ] then
        is_overlap := false;
        break;
      fi;
    od;
    if is_overlap then
      # find b and c such that p*c = b*q:
      b := SubpathLR( p, 0, len_p - overlap_len );
      c := SubpathLR( q, overlap_len, len_q );
      Add( overlaps, [ b, c ] );
    fi;
  od;
  return overlaps;
end );



InstallMethod( QuiverCategory, [ IsLeftQuiver ], Q -> IsLeftQuiver );
InstallMethod( QuiverCategory, [ IsRightQuiver ], Q -> IsRightQuiver );

InstallMethod( Orientation, [ IsLeftQuiver ], Q -> LEFT );
InstallMethod( Orientation, [ IsRightQuiver ], Q -> RIGHT );

InstallMethod( IsAcyclicQuiver, [ IsQuiver ],
function( Q )
  local vertices, arrow_removed, to_remove, i, v, incoming, a;
  vertices := ShallowCopy( Vertices( Q ) );
  arrow_removed := List( [ 1 .. NumberOfArrows( Q ) ], i -> false );
  repeat
    to_remove := [];
    for i in [ 1 .. Length( vertices ) ] do
      v := vertices[ i ];
      incoming := IncomingArrows( v );
      if ForAll( IncomingArrows( v ),
                 a -> arrow_removed[ ArrowNumber( a ) ] ) then
        Add( to_remove, i );
        for a in OutgoingArrows( v ) do
          arrow_removed[ ArrowNumber( a ) ] := true;
        od;
      fi;
    od;
    for i in to_remove do
      Remove( vertices, i );
    od;
  until Length( vertices ) = 0 or Length( to_remove ) = 0;
  return Length( vertices ) = 0;
end );

InstallMethod( Vertices,
               "for quiver",
	       [ IsQuiver and IsQuiverRep ],
function( Q )
  return Q!.vertices;
end );

InstallMethod( Arrows,
               "for quiver",
	       [ IsQuiver and IsQuiverRep ],
function( Q )
  return Q!.arrows;
end );

InstallMethod( VertexLabels, [ IsQuiver ], Q -> List( Vertices( Q ), Label ) );
InstallMethod( ArrowLabels, [ IsQuiver ], Q -> List( Arrows( Q ), Label ) );

InstallMethod( NumberOfVertices,
               "for quiver",
	       [ IsQuiver and IsQuiverRep ],
function( Q )
  return Length( Q!.vertices );
end );

InstallMethod( NumberOfArrows,
               "for quiver",
	       [ IsQuiver and IsQuiverRep ],
function( Q )
  return Length( Q!.arrows );
end );

InstallMethod( ArrowSourceIndices, "for quiver",
               [ IsQuiver ],
function( Q )
  return List( Arrows( Q ), a -> VertexNumber( Source( a ) ) );
end );

InstallMethod( ArrowTargetIndices, "for quiver",
               [ IsQuiver ],
function( Q )
  return List( Arrows( Q ), a -> VertexNumber( Target( a ) ) );
end );

InstallMethod( PrimitivePaths,
               "for quiver",
	       [ IsQuiver and IsQuiverRep ],
function( Q )
  return Q!.primitive_paths;
end );

InstallMethod( Vertex, "for quiver and positive integer",
	       [ IsQuiver and IsQuiverRep, IsPosInt ],
function( Q, i )
  return Q!.vertices[ i ];
end );

InstallMethod( Arrow, "for quiver and positive integer",
	       [ IsQuiver and IsQuiverRep, IsPosInt ],
function( Q, i )
  return Q!.arrows[ i ];
end );

InstallMethod( PrimitivePathByLabel, "for quiver and object",
               [ IsQuiver, IsObject ],
function( Q, label )
  local p;
  for p in PrimitivePaths( Q ) do
    if Label( p ) = label then
      return p;
    fi;
  od;
  return fail;
end );

InstallMethod( \[\], "for quiver and object",
	       [ IsQuiver, IsObject ],
               PrimitivePathByLabel );

InstallMethod( PathFromString, "for quiver and string",
	       [ IsQuiver, IsString ],
function( Q, string )
  local p, list;
  p := PrimitivePathByLabel( Q, string );
  if p <> fail then
    return p;
  fi;
  list := List( string, label -> PrimitivePathByLabel( Q, label ) );
  if ForAll( list, p -> p <> fail ) then
    return ComposePathsLR( list );
  fi;
  for p in PrimitivePaths( Q ) do
    if LabelAsString( p ) = string then
      return p;
    fi;
  od;
  return fail;
end );

InstallMethod( \., "for quiver and positive integer",
	       [ IsQuiver, IsPosInt ],
function( Q, string_as_int )
  return PathFromString( Q, NameRNam( string_as_int ) );
end );

InstallMethod( String, "for primitive path",
               [ IsPrimitivePath ],
               LabelAsString );

InstallMethod( String, "for composite path",
               [ IsCompositePath ],
function( p )
  return JoinStringsWithSeparator( List( ArrowListLR( p ), LabelAsString ), "*" );
end );

InstallMethod( ViewObj, "for path",
	       [ IsPath ],
function( p )
  Print( "(", String( p ), ")" );
end );

InstallMethod( PrintObj,
               "for vertex",
	       [ IsVertex ],
function( v )
  Print( "<vertex ", Label( v ), " in ", LabelAsString( QuiverOfPath( v ) ), ">" );
end );

InstallMethod( ArrowString, "for arrow",
               [ IsArrow ],
function( a )
  return Concatenation( LabelAsString( a ), ":",
                        String( Source( a ) ), "->",
                        String( Target( a ) ) );
end );

InstallMethod( PrintObj,
               "for arrow",
	       [ IsArrow ],
function( a )
  Print( "<arrow ", String( a ),
         " in ", LabelAsString( QuiverOfPath( a ) ), ">" );
end );

InstallMethod( PrintObj,
               "for composite path",
	       [ IsCompositePath ],
function( p )
  Print( "<composite path ", String( p ), " in ",
         LabelAsString( QuiverOfPath( p ) ), ">" );
end );

InstallMethod( String, "for quiver",
               [ IsQuiver ],
function( Q )
  local vertices, arrows;
  if ForAll( Vertices( Q ),
             v -> Label( v ) = VertexNumber( v ) ) then
    vertices := String( NumberOfVertices( Q ) );
  else
    vertices := JoinStringsWithSeparator( Vertices( Q ), "," );
    if NumberOfVertices( Q ) = 1 and IsPosInt( Label( Vertex( Q, 1 ) ) ) then
      # When we have only one vertex, and it has a positive integer as label,
      # we add a comma to make it clear that this is in fact a listing of labels
      # and not the number of vertices.
      vertices := Concatenation( vertices, "," );
      # For example, consider the following two quivers:
      #   LeftQuiver("Q",4,[['a',4,4]]);
      #   LeftQuiver("Q",[4],[['a',4,4]]);
      # (the first has four vertices, the second has one vertex labelled 4)
    fi;
  fi;
  arrows := JoinStringsWithSeparator( List( Arrows( Q ), ArrowString ), "," );
  return Concatenation( LabelAsString( Q ), "(", vertices, ")[", arrows, "]" );
end );

InstallMethod( PrintObj,
               "for quiver",
	       [ IsQuiver ],
function( Q )
  Print( String( Q ) );
end );

InstallMethod( Label,
	       "for quiver",
	       [ IsQuiver and IsQuiverRep ],
function( Q )
  return Q!.label;
end );

InstallMethod( \=, "for vertices",
 	       [ IsVertex and IsVertexRep, IsVertex and IsVertexRep ],
function( v1, v2 )
  return QuiverOfPath( v1 ) = QuiverOfPath( v2 )
         and VertexNumber( v1 ) = VertexNumber( v2 );
end );

InstallMethod( \=,
               "for arrows",
 	       [ IsArrow and IsArrowRep, IsArrow and IsArrowRep ],
function( a1, a2 )
  return QuiverOfPath( a1 ) = QuiverOfPath( a2 )
         and ArrowNumber( a1 ) = ArrowNumber( a2 );
end );

# InstallMethod( \=, "for vertices",
#                [ IsVertex and IsVertexRep, IsVertex and IsVertexRep ],
#                IsIdenticalObj );

# InstallMethod( \=, "for arrows",
#                [ IsArrow and IsArrowRep, IsArrow and IsArrowRep ],
#                IsIdenticalObj );

InstallMethod( \=,
               "for vertex and nontrivial path",
 	       [ IsVertex, IsNontrivialPath ],
	       ReturnFalse );
InstallMethod( \=,
               "for nontrivial path and vertex",
 	       [ IsNontrivialPath, IsVertex ],
	       ReturnFalse );
InstallMethod( \=,
               "for arrow and composite path",
 	       [ IsArrow, IsCompositePath ],
	       ReturnFalse );
InstallMethod( \=,
               "for composite path and arrow",
 	       [ IsCompositePath, IsArrow ],
	       ReturnFalse );
InstallMethod( \=,
	       "for composite paths",
	       [ IsCompositePath, IsCompositePath ],
function( p1, p2 )
  return ArrowList( p1 ) = ArrowList( p2 );
end );

InstallMethod( \=, "for quivers",
               [ IsQuiver, IsQuiver ],
function( Q1, Q2 )
  return QuiverCategory( Q1 ) = QuiverCategory( Q2 )
         and Label( Q1 ) = Label( Q2 )
         and List( Vertices( Q1 ), Label ) = List( Vertices( Q2 ), Label )
         and List( Arrows( Q1 ), Label ) = List( Arrows( Q2 ), Label )
         and ArrowSourceIndices( Q1 ) = ArrowSourceIndices( Q2 )
         and ArrowTargetIndices( Q1 ) = ArrowTargetIndices( Q2 );
end );

InstallMethod( \in, "for object and quiver",
               [ IsObject, IsQuiver ],
               ReturnFalse );

InstallMethod( \in, "for path and quiver",
               [ IsPath, IsQuiver ],
function( p, Q )
  return QuiverOfPath( p ) = Q;
end );


InstallMethod( OppositeQuiver,
               [ IsQuiver ],
function( Q )
  local Q_op;
  Q_op := Quiver( QuiverCategory( Q ),
                  ToggleSuffix( Label( Q ), "_op" ),
                  VertexLabels( Q ),
                  ArrowLabels( Q ),
                  ArrowTargetIndices( Q ),
                  ArrowSourceIndices( Q ) );
  SetOppositeQuiver( Q_op, Q );
  return Q_op;
end );

InstallMethod( ToggleSuffix, "for two strings", [ IsString, IsString ],
function( s, suffix )
  if EndsWith( s, suffix ) and Length( s ) > Length( suffix ) then
    return s{ [ 1 .. Length( s ) - Length( suffix ) ] };
  else
    return Concatenation( s, suffix );
  fi;
end );

InstallMethod( OppositePath,
               [ IsVertex ],
function( v )
  return Vertex( OppositeQuiver( QuiverOfPath( v ) ), VertexNumber( v ) );
end );

InstallMethod( OppositePath,
               [ IsArrow ],
function( a )
  return Arrow( OppositeQuiver( QuiverOfPath( a ) ), ArrowNumber( a ) );
end );

InstallMethod( OppositePath,
               [ IsCompositePath ],
function( p )
  return PathFromArrowListNC( Reversed( List( ArrowList( p ), OppositePath ) ) );
end );

InstallMethod( QuiverProduct,
               [ IsQuiver, IsQuiver ],
function( Q, R )
  local nQ, nR, make_index, QxR;
  if QuiverCategory( Q ) <> QuiverCategory( R ) then
    Error( "Quivers in quiver product must have same orientation" );
  fi;
  nQ := NumberOfVertices( Q );
  nR := NumberOfVertices( R );
  make_index := L -> nR * ( L[ 1 ] - 1 ) + L[ 2 ];
  QxR := Quiver( QuiverCategory( Q ),
                 [ Label( Q ), Label( R ) ],
                 Cartesian( VertexLabels( Q ), VertexLabels( R ) ),
                 Concatenation( Cartesian( VertexLabels( Q ), ArrowLabels( R ) ),
                                Cartesian( ArrowLabels( Q ), VertexLabels( R ) ) ),
                 List( Concatenation( Cartesian( [ 1 .. nQ ], ArrowSourceIndices( R ) ),
                                      Cartesian( ArrowSourceIndices( Q ), [ 1 .. nR ] ) ),
                       make_index ),
                 List( Concatenation( Cartesian( [ 1 .. nQ ], ArrowTargetIndices( R ) ),
                                      Cartesian( ArrowTargetIndices( Q ), [ 1 .. nR ] ) ),
                       make_index ) );
  SetIsProductQuiver( QxR, true );
  SetProductQuiverFactors( QxR, [ Q, R ] );
  return QxR;
end );

InstallMethod( QuiverProduct,
               [ IsHomogeneousList ],
function( L )
  if Length( L ) = 2 then
    return QuiverProduct( L[ 1 ], L[ 2 ] );
  fi;
  Error( "not implemented" );
end );

InstallMethod( IsProductQuiver,
               [ IsQuiver ],
function( Q )
  return ProductQuiverFactors( Q ) <> fail;
end );

InstallMethod( ProductQuiverFactors,
               [ IsQuiver ],
function( Q )
  local num_factors, vertex_labels, num_vertices, num_arrows,
        arrow_part_sizes, arrow_labels, sources, targets, p, s,
        vertex_index, source_index, target_index, n, na, i, Qs;

  if not IsList( Label( Q ) ) then
    return fail;
  fi;

  num_factors := Length( Label( Q ) );
  if not ForAll( PrimitivePaths( Q ),
                 p -> IsList( Label( p ) ) and
                      Length( Label( p ) ) = num_factors ) then
    return fail;
  fi;

  vertex_labels := Decartesian( List( Vertices( Q ), Label ) );
  num_vertices := List( vertex_labels, Length );
  num_arrows := [];
  arrow_part_sizes := [];
  arrow_labels := List( [ 1 .. num_factors ], i -> [] );
  sources := List( [ 1 .. num_factors ], i -> [] );
  targets := List( [ 1 .. num_factors ], i -> [] );
  p := 1;
  s := 0;
  vertex_index := function( label, n )
    local i;
    for i in [ 1 .. num_vertices[ n ] ] do
      if vertex_labels[ n ][ i ] = label then
        return i;
      fi;
    od;
    Print( "No vertex with label ", label, " (labels are ", vertex_labels[n], ")\n" );
    return fail; # or Error?
  end;
  source_index := function( i, n )
    return vertex_index( Label( Source( Arrow( Q, i ) ) )[ n ], n );
  end;
  target_index := function( i, n )
    return vertex_index( Label( Target( Arrow( Q, i ) ) )[ n ], n );
  end;
  for n in [ num_factors, num_factors - 1 .. 1 ] do
    na := 0;
    for i in [ s + p, s + 2 * p .. NumberOfArrows( Q ) ] do
      if i > s + p and Label( Arrow( Q, i ) )[ n ] = arrow_labels[ n ][ 1 ] then
        break;
      else
        na := na + 1;
        arrow_labels[ n ][ na ] := Label( Arrow( Q, i ) )[ n ];
        sources[ n ][ na ] := source_index( i, n );
        targets[ n ][ na ] := target_index( i, n );
      fi;
    od;
    num_arrows[ n ] := na;
    arrow_part_sizes[ n ] := NumberOfVertices( Q ) / num_vertices[ n ] * na;
    p := p * num_vertices[ n ];
    s := s + arrow_part_sizes[ n ];
  od;

  Qs := ListN( List( [ 1 .. num_factors ], i -> QuiverCategory( Q ) ),
               Label( Q ),
               vertex_labels,
               arrow_labels,
               sources,
               targets,
               Quiver );

  if QuiverProduct( Qs ) = Q then
    return Qs;
  else
    return fail;
  fi;
end );

InstallMethod( ProductQuiverFactor,
               [ IsProductQuiver, IsPosInt ],
function( PQ, n )
  return ProductQuiverFactors( PQ )[ n ];
end );

InstallMethod( Decartesian,
               [ IsList ],
function( L )
  local num_factors, lengths, length_products, p, total_length,
        n, e, len, i, lists;
  total_length := Length( L );
  if total_length = 0 then
    return fail;
  fi;
  num_factors := Length( L[ 1 ] );
  if not ForAll( L, l -> IsList( l ) and Length( l ) = num_factors ) then
    #Print( "Different types of elements\n" );
    return fail;
  fi;
  lengths := [];
  length_products := [];
  p := 1;
  for n in [ num_factors, ( num_factors - 1 ) .. 1 ] do
    e := L[ p ][ n ];
    len := 1;
    for i in [ 2 * p, 3 * p .. total_length ] do
      if L[ i ][ n ] = L[ p ][ n ] then
        break;
      else
        len := len + 1;
      fi;
    od;
    lengths[ n ] := len;
    #Print( "lengths[", n, "] = ", len, "\n" );
    length_products[ n ] := p;
    p := p * len;
    if total_length mod p <> 0 then
      #Print( "Bad length\n" );
      return fail;
    fi;
  od;
  if p <> total_length then
    #Print( "Product of lengths not equal to total length\n" );
    return fail;
  fi;
  lists := List( [ 1 .. num_factors ], i -> [] );
  for n in [ 1 .. num_factors ] do
    for i in [ 1 .. lengths[ n ] ] do
      #Print( "(", n, ",", i, ")\n" );
      lists[ n ][ i ] := L[ ( i - 1 ) * length_products[ n ] + 1 ][ n ];
    od;
  od;
  for i in [ 1 .. total_length ] do
    for n in [ 1 .. num_factors ] do
      if lists[ n ][ ( QuoInt( i - 1, length_products[ n ] ) mod lengths[ n ] ) + 1 ]
         <> L[ i ][ n ] then
        # Print( "Wrong value at position ", [i,n], ", is ",
        #        L[ i ][ n ],
        #        ", should be ",
        #        lists[ n ][ ( QuoInt( i - 1, length_products[ n ] ) mod lengths[ n ] ) + 1 ],
        #        "\n" );
        return fail;
      fi;
    od;
  od;
  return lists;
end );

InstallMethod( ProjectPathFromProductQuiver,
               [ IsPosInt, IsPrimitivePath ],
function( n, p )
  local PQ;
  PQ := QuiverOfPath( p );
  if not IsProductQuiver( PQ ) then
    Error( "The path ", p, " is not in a product quiver" );
  fi;
  return PrimitivePathByLabel( ProductQuiverFactor( PQ, n ),
                               Label( p )[ n ] );
end );

InstallMethod( ProjectPathFromProductQuiver,
               [ IsPosInt, IsCompositePath ],
function( n, p )
  return PathFromArrowList
         ( List( ArrowList( p ),
                 a -> ProjectPathFromProductQuiver( n, a ) ) );
end );

InstallMethod( IncludePathInProductQuiver,
               [ IsProductQuiver, IsPosInt, IsList, IsPrimitivePath ],
function( PQ, n, vertices, p )
  local label, i;
  if ProductQuiverFactor( PQ, n ) <> QuiverOfPath( p ) then
    Error( "The path ", p, " is not in factor ", n, " of the product quiver" );
  fi;
  label := [];
  label[ n ] := Label( p );
  for i in [ 1 .. Length( ProductQuiverFactors( PQ ) ) ] do
    if i = n then
      continue;
    elif not IsBound( vertices[ i ] ) then
      Error( "Vertex ", i, " missing" );
    elif not IsVertex( vertices[ i ] ) then
      Error( "Element ", i, " in vertex list is not a vertex" );
    elif ProductQuiverFactor( PQ, i ) <> QuiverOfPath( vertices[ i ] ) then
      Error( "Vertex ", i, " is not in factor ", i, " of the product quiver" );
    fi;
    label[ i ] := Label( vertices[ i ] );
  od;
  return PrimitivePathByLabel( PQ, label );
end );

InstallMethod( IncludePathInProductQuiver,
               [ IsProductQuiver, IsPosInt, IsList, IsCompositePath ],
function( PQ, n, vertices, p )
  return TranslatePath( p, a -> IncludePathInProductQuiver( PQ, n, vertices, a ) );
end );

InstallMethod( PathInProductQuiver,
               [ IsProductQuiver, IsHomogeneousList, IsPerm ],
function( PQ, paths, permutation )
  local n, i, num_factors, sources, targets, included_paths, vertices, pos;
  num_factors := Length( ProductQuiverFactors( PQ ) );
  if Length( paths ) <> num_factors then
    Error( "Product quiver has ", num_factors,
           " factors, but ", Length( paths ), " paths were given" );
  fi;
  for n in [ 1 .. num_factors ] do
    if ProductQuiverFactor( PQ, n ) <> QuiverOfPath( paths[ n ] ) then
      Error( "Path ", n, " is not in factor ", n, " of the product quiver" );
    fi;
  od;
  sources := List( paths, Source );
  targets := List( paths, Target );
  included_paths := [];
  for n in [ 1 .. num_factors ] do
    vertices := [];
    pos := n^permutation;
    for i in [ 1 .. ( pos - 1 ) ] do
      vertices[ i/permutation ] := targets[ i/permutation ];
    od;
    for i in [ ( pos + 1 ) .. num_factors ] do
      vertices[ i/permutation ] := sources[ i/permutation ];
    od;
    included_paths[ n ] := IncludePathInProductQuiver( PQ, n, vertices, paths[ n ] );
  od;
  return ComposePaths( Permuted( included_paths, permutation ) );
end );

InstallMethod( PathInProductQuiver,
               [ IsProductQuiver, IsHomogeneousList ],
function( PQ, paths )
  return PathInProductQuiver( PQ, paths, () );
end );


InstallMethod( PathIterator, [ IsQuiver ],
function( Q )
  return FilteredPathIterator( Q, ReturnTrue );
end );

InstallMethod( FilteredPathIterator, [ IsQuiver, IsFunction ],
function( Q, filter )
  local next, is_done, print, make_init_record, init_queue;
  next := function( iter )
    local p, a, new_p;
    if Length( iter!.path_queue ) = 0 then
      Error( "no more paths in path iterator" );
    fi;
    p := Remove( iter!.path_queue, 1 );
    if Length( p ) > 0 then
      for a in OutgoingArrows( Target( p ) ) do
        new_p := ComposePaths( p, a );
        if filter( new_p ) then
          Add( iter!.path_queue, new_p );
        fi;
      od;
    fi;
    return p;
  end;
  is_done := iter -> Length( iter!.path_queue ) = 0;
  print := function( iter )
    Print( "<path iterator over quiver ", Q, ">" );
  end;
  make_init_record := function( queue )
    return rec( path_queue := queue,
                NextIterator := next,
                IsDoneIterator := is_done,
                ShallowCopy := iter -> make_init_record( iter!.path_queue ),
                PrintObj := print );
  end;
  init_queue := Filtered( Concatenation( Vertices( Q ), Arrows( Q ) ), filter );
  return IteratorByFunctions( make_init_record( init_queue ) );
end );

InstallMethod( PathList, [ IsQuiver ],
function( Q )
  local iter, list;
  if not IsAcyclicQuiver( Q ) then
    Error( "not an acyclic quiver" );
  fi;
  iter := PathIterator( Q );
  list := [];
  while not IsDoneIterator( iter ) do
    Add( list, NextIterator( iter ) );
  od;
  return list;
end );
