#! @Chapter Quivers

BindGlobal( "FamilyOfPaths", NewFamily( "paths" ) );
BindGlobal( "FamilyOfQuivers", CollectionsFamily( FamilyOfPaths ) );

DeclareRepresentation( "IsQuiverVertexRep", IsComponentObjectRep and IsAttributeStoringRep,
                       [ "quiver", "index" ] );
DeclareRepresentation( "IsArrowRep", IsComponentObjectRep and IsAttributeStoringRep,
                       [ "quiver", "label", "index", "source", "target" ] );
DeclareRepresentation( "IsCompositePathRep", IsComponentObjectRep and IsAttributeStoringRep,
                       [ "arrows" ] );
DeclareRepresentation( "IsQuiverRep", IsComponentObjectRep and IsAttributeStoringRep,
		       [ "label", "vertices", "arrows", "primitive_paths",
		         "vertices_desc", "arrows_desc"] );

DeclareDirectionOperations( IsPath, IsLeftPath, IsRightPath );
DeclareDirectionOperations( IsQuiver, IsLeftQuiver, IsRightQuiver );
DeclareDirectionOperations( Quiver, LeftQuiver, RightQuiver );

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

InstallMethodWithDirections( Quiver,
                             [ IsString, IsPosInt, IsDenseList ],
dir -> function( label_with_patterns, num_vertices, arrows )
  local tmp, label, vertex_label_pattern, arrow_label_pattern;
  tmp := DecomposeQuiverDescriptionString( label_with_patterns );
  label := tmp[ 1 ];
  vertex_label_pattern := tmp[ 2 ];
  arrow_label_pattern := tmp[ 3 ];
  return Quiver( dir,
                 [ label, vertex_label_pattern, arrow_label_pattern ],
                 num_vertices, [], arrows );
end );

InstallMethodWithDirections( Quiver,
                             [ IsString, IsDenseList, IsDenseList ],
dir -> function( label_with_patterns, vertex_labels, arrows )
  local tmp, label, vertex_label_pattern, arrow_label_pattern;
  tmp := DecomposeQuiverDescriptionString( label_with_patterns );
  label := tmp[ 1 ];
  vertex_label_pattern := tmp[ 2 ];
  arrow_label_pattern := tmp[ 3 ];
  return Quiver( dir,
                 [ label, vertex_label_pattern, arrow_label_pattern ],
                 Length( vertex_labels ), vertex_labels, arrows );
end );

InstallMethodWithDirections( Quiver,
                             [ IsString ],
dir -> function( description )
  return CallFuncList( Quiver^dir,
                       ParseQuiverDescriptionString( description ) );
end );

InstallMethodWithDirections( Quiver,
                             [ IsDenseList, IsPosInt, IsList, IsDenseList ],
dir -> function( label_with_patterns_list,
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
  return Quiver( dir, label, all_vertex_labels, arrow_labels,
                 source_indices, target_indices );
end );

InstallMethodWithDirections( Quiver,
                             [ IsObject, IsDenseList, IsDenseList,
                               IsDenseList, IsDenseList ],
dir -> function( label, vertex_labels, arrow_labels,
                 source_indices, target_indices )
  local num_vertices, num_arrows, path_cat, quiver_type,
        Q, vertex_type, make_vertex, arrow_type, make_arrow,
        incoming_arrows, outgoing_arrows, i;
  num_vertices := Length( vertex_labels );
  num_arrows := Length( arrow_labels );
  path_cat := IsPath^dir;
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

  quiver_type := NewType( FamilyOfQuivers, IsQuiver^dir and IsQuiverRep );
  Q := Objectify( quiver_type,
                  rec( label := label ) );
  SetDirection( Q, dir );

  vertex_type := NewType( FamilyOfPaths, IsQuiverVertex and IsQuiverVertexRep and path_cat );
  make_vertex := function( num, label )
    return Objectify( vertex_type,
                      rec( quiver := Q,
                           index := num,
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
                           index := num,
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
    SetOutdegree( Q!.vertices[ i ], Length( outgoing_arrows[ i ] ) );
    SetIndegree( Q!.vertices[ i ], Length( incoming_arrows[ i ] ) );
    SetDegreeOfVertex( Q!.vertices[ i ],
                       Length( outgoing_arrows[ i ] ) + Length( incoming_arrows[ i ] ) );
  od;

  return Intern( Q );
end );

InstallMethod( QuiverOfPath,
               "for vertex",
	       [ IsQuiverVertex and IsQuiverVertexRep ],
function( a ) return a!.quiver; end );
InstallMethod( QuiverOfPath,
               "for arrow",
	       [ IsArrow and IsArrowRep ],
function( a ) return a!.quiver; end );
InstallMethod( QuiverOfPath,
               "for composite path",
	       [ IsCompositePath and IsCompositePathRep ],
function( a ) return QuiverOfPath( a!.arrows[1] ); end );

InstallMethod( Direction, "for path", [ IsPath ], p -> Direction( QuiverOfPath( p ) ) );

InstallMethod( Source,
               "for vertex",
	       [ IsQuiverVertex and IsQuiverVertexRep ],
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
	       [ IsQuiverVertex and IsQuiverVertexRep ],
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

InstallMethod( Neighbors, "for vertex", [ IsQuiverVertex ],
function( v )
  local Q, n, vertices, is_neighbor, a, i, neighbors;
  Q := QuiverOfPath( v );
  n := NumberOfVertices( Q );
  vertices := Vertices( Q );
  is_neighbor := List( [ 1 .. n ], i -> false );
  for a in OutgoingArrows( v ) do
    is_neighbor[ VertexIndex( Target( a ) ) ] := true;
  od;
  for a in IncomingArrows( v ) do
    is_neighbor[ VertexIndex( Source( a ) ) ] := true;
  od;
  neighbors := [];
  for i in [ 1 .. n ] do
    if is_neighbor[ i ] then
      Add( neighbors, vertices[ i ] );
    fi;
  od;
  return neighbors;
end );

InstallMethod( Length,
               "for vertex",
	       [ IsQuiverVertex and IsQuiverVertexRep ],
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
	       [ IsQuiverVertex and IsQuiverVertexRep ],
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
	       [ IsQuiverVertex and IsQuiverVertexRep ],
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
	       [ IsQuiverVertex and IsQuiverVertexRep ],
function( v )
  return v!.label;
end );

## fallback method
InstallMethod( LabelAsLaTeXString,
          [ IsPrimitivePath ],
  LabelAsString
);

##
InstallMethod( SetLabelsAsLaTeXStrings,
          [ IsQuiver, IsList, IsList ],
  function( quiver, vertices_labels, arrows_labels )
    local vertices, n, arrows, i;
    
    vertices := Vertices( quiver );
    
    n := NumberOfVertices( quiver );
    
    for i in [ 1 .. n ] do
      
      SetLabelAsLaTeXString( vertices[ i ], vertices_labels[ i ] );
      
    od;
    
    arrows := Arrows( quiver );
    
    n := NumberOfArrows( quiver );
    
    for i in [ 1 .. n ] do
      
      SetLabelAsLaTeXString( arrows[ i ], arrows_labels[ i ] );
      
    od;
    
end );

## For example when the vertices are integers, then
## latex_string by the fallback method is the same as LabelAsString
##
InstallOtherMethod( SetLabelsAsLaTeXStrings,
          [ IsQuiver, IsList ],
  function( quiver, arrows_labels )
    local arrows, n, i;
    
    arrows := Arrows( quiver );
    
    n := NumberOfArrows( quiver );
    
    for i in [ 1 .. n ] do
      
      SetLabelAsLaTeXString( arrows[ i ], arrows_labels[ i ] );
      
    od;
    
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

InstallMethod( VertexIndex,
               "for vertex",
	       [ IsQuiverVertex and IsQuiverVertexRep ],
function( v )
  return v!.index;
end );

InstallMethod( ArrowIndex,
               "for arrow",
	       [ IsArrow and IsArrowRep ],
function( a )
  return a!.index;
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
	       [ IsQuiverVertex, IsPath ],
function( v, p )
  if Composable( v, p ) then
    return p;
  else
    return fail;
  fi;
end );

InstallMethod( ComposePaths2,
               "for path and vertex",
	       [ IsPath, IsQuiverVertex ],
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
  if Length( list ) > 1 then
    return Objectify( NewType( FamilyObj( list[ 1 ] ),
                               IsPath^Direction( list[ 1 ] ) and IsCompositePath
                               and IsCompositePathRep ),
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
  for i in [ 1 .. Length( list ) ] do
    if not IsArrow( list[ i ] ) then
      Error( "not an arrow: ", list[ i ] );
    fi;
  od;
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
               [ IsQuiverVertex, IsInt, IsInt ],
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
  elif IsQuiverVertex( p1 ) then
    return VertexIndex( p1 ) < VertexIndex( p2 );
  elif IsArrow( p1 ) then
    return ArrowIndex( p1 ) < ArrowIndex( p2 );
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
               [ IsQuiverVertex, IsQuiverVertex ],
function( v1, v2 )
  if v1 = v2 then
    return 0;
  else
    return fail;
  fi;
end );

InstallMethod( SubpathIndex, "for vertex and nontrivial path",
               IsIdenticalObj,
               [ IsQuiverVertex, IsNontrivialPath ],
               ReturnFail );

InstallMethod( SubpathIndex, "for nontrivial path and vertex",
               IsIdenticalObj,
               [ IsNontrivialPath, IsQuiverVertex ],
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
               [ IsQuiverVertex, IsPath ],
function( v, p )
  return [];
end );

InstallMethod( PathOverlaps, "for path and vertex",
               [ IsPath, IsQuiverVertex ],
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

InstallMethod( VerticesAttr,
               "for quiver",
	       [ IsQuiver and IsQuiverRep ],
function( Q )
  return Q!.vertices;
end );

InstallMethod( Vertices,
               "for quiver",
	       [ IsQuiver and IsQuiverRep ],
VerticesAttr );

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
  return List( Arrows( Q ), a -> VertexIndex( Source( a ) ) );
end );

InstallMethod( ArrowTargetIndices, "for quiver",
               [ IsQuiver ],
function( Q )
  return List( Arrows( Q ), a -> VertexIndex( Target( a ) ) );
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

InstallMethod( SourceVertices, "for quiver", [ IsQuiver ],
function( Q )
  return Filtered( Vertices( Q ), v -> Indegree( v ) = 0 );
end );

InstallMethod( SinkVertices, "for quiver", [ IsQuiver ],
function( Q )
  return Filtered( Vertices( Q ), v -> Outdegree( v ) = 0 );
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
	       [ IsQuiverVertex ],
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
             v -> Label( v ) = VertexIndex( v ) ) then
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

# InstallMethod( \=, "for vertices",
#  	       [ IsQuiverVertex and IsQuiverVertexRep, IsQuiverVertex and IsQuiverVertexRep ],
# function( v1, v2 )
#   return QuiverOfPath( v1 ) = QuiverOfPath( v2 )
#          and VertexIndex( v1 ) = VertexIndex( v2 );
# end );

# InstallMethod( \=,
#                "for arrows",
#  	       [ IsArrow and IsArrowRep, IsArrow and IsArrowRep ],
# function( a1, a2 )
#   return QuiverOfPath( a1 ) = QuiverOfPath( a2 )
#          and ArrowIndex( a1 ) = ArrowIndex( a2 );
# end );

InstallMethod( \=, "for vertices",
               [ IsQuiverVertex and IsQuiverVertexRep, IsQuiverVertex and IsQuiverVertexRep ],
               IsIdenticalObj );

InstallMethod( \=, "for arrows",
               [ IsArrow and IsArrowRep, IsArrow and IsArrowRep ],
               IsIdenticalObj );

InstallMethod( \=,
               "for vertex and nontrivial path",
 	       [ IsQuiverVertex, IsNontrivialPath ],
	       ReturnFalse );
InstallMethod( \=,
               "for nontrivial path and vertex",
 	       [ IsNontrivialPath, IsQuiverVertex ],
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


BindGlobal( "FamilyOfQuiverHomomorphisms", NewFamily( "quiver homomorphisms" ) );

SetFamilySource( FamilyOfQuiverHomomorphisms,
                 FamilyOfPaths );

InstallMethod( QuiverHomomorphism, "for two quivers and two dense lists",
               [ IsQuiver, IsQuiver, IsDenseList, IsDenseList ],
function( Q1, Q2, vertex_images, arrow_images )
  local i;
  if Length( vertex_images ) <> NumberOfVertices( Q1 ) then
    Error( "wrong number of vertex images" );
  fi;
  if Length( arrow_images ) <> NumberOfArrows( Q1 ) then
    Error( "wrong number of arrow images" );
  fi;
  AssumeAll( vertex_images, IsQuiverVertex, "not a vertex" );
  AssumeAll( vertex_images, v -> v in Q2, "vertex not in range quiver" );
  AssumeAll( arrow_images, IsArrow, "not an arrow" );
  AssumeAll( arrow_images, a -> a in Q2, "arrow not in range quiver" );
  for i in [ 1 .. NumberOfArrows( Q1 ) ] do
    if Source( arrow_images[ i ] ) <> vertex_images[ VertexIndex( Source( Arrow( Q1, i ) ) ) ] then
      Error( "wrong source for arrow image ", i );
    fi;
    if Target( arrow_images[ i ] ) <> vertex_images[ VertexIndex( Target( Arrow( Q1, i ) ) ) ] then
      Error( "wrong target for arrow image ", i );
    fi;
  od;
  return QuiverHomomorphismNC( Q1, Q2, vertex_images, arrow_images );
end );

InstallMethod( QuiverHomomorphism, "for two quivers and dense list",
               [ IsQuiver, IsQuiver, IsDenseList ],
function( Q1, Q2, images )
  local n, m, l;
  n := NumberOfVertices( Q1 );
  m := NumberOfArrows( Q1 );
  l := Length( images );
  if l <> n + m then
    Error( "list of images has wrong length" );
  fi;
  return QuiverHomomorphism( Q1, Q2, images{ [ 1 .. n ] }, images{ [ ( n + 1 ) .. l ] } );
end );

InstallMethod( QuiverHomomorphismNC, "for two quivers and two dense lists",
               [ IsQuiver, IsQuiver, IsDenseList, IsDenseList ],
function( Q1, Q2, vertex_images, arrow_images )
  local m;
  m := rec();
  ObjectifyWithAttributes
    ( m, NewType( FamilyOfQuiverHomomorphisms,
                  IsQuiverHomomorphism
                  and IsComponentObjectRep and IsAttributeStoringRep ),
      Source, Q1,
      Range, Q2,
      VertexImages, vertex_images,
      ArrowImages, arrow_images );
  return m;
end );

InstallMethod( ImageElm, "for quiver homomorphism and path",
               [ IsQuiverHomomorphism, IsPath ],
function( m, p )
  if not p in Source( m ) then
    Error( "path is not in source of homomorphism" );
  fi;
  if IsQuiverVertex( p ) then
    return VertexImages( m )[ VertexIndex( p ) ];
  else
    return PathFromArrowListNC( List( ArrowList( p ),
                                      a -> ArrowImages( m )[ ArrowIndex( a ) ] ) );
  fi;
end );


InstallMethod( RenameQuiver, "for a quiver and an object", [ IsObject, IsQuiver ],
function( label, Q )
  return Quiver( Direction( Q ), label,
                 VertexLabels( Q ), ArrowLabels( Q ),
                 ArrowSourceIndices( Q ), ArrowTargetIndices( Q ) );
end );

InstallMethod( OppositeQuiver,
               [ IsQuiver ],
function( Q )
  local Q_op;
  Q_op := Quiver( Direction( Q ),
                  ToggleSuffix( Label( Q ), "_op" ),
                  VertexLabels( Q ),
                  ArrowLabels( Q ),
                  ArrowTargetIndices( Q ),
                  ArrowSourceIndices( Q ) );
  SetOppositeQuiver( Q_op, Q );
  return Q_op;
end );

InstallMethod( OppositeQuiver,
               [ IsProductQuiver ],
function( QxR )
  local factors, Q, R;
  factors := ProductQuiverFactors( QxR );
  Q := factors[ 1 ];
  R := factors[ 2 ];
  return QuiverProduct( OppositeQuiver( Q ), OppositeQuiver( R ) );
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
               [ IsQuiverVertex ],
function( v )
  return Vertex( OppositeQuiver( QuiverOfPath( v ) ), VertexIndex( v ) );
end );

InstallMethod( OppositePath,
               [ IsArrow ],
function( a )
  return Arrow( OppositeQuiver( QuiverOfPath( a ) ), ArrowIndex( a ) );
end );

InstallMethod( OppositePath,
               [ IsCompositePath ],
function( p )
  return PathFromArrowListNC( Reversed( List( ArrowList( p ), OppositePath ) ) );
end );

InstallMethod( ProductQuiverVertexIndex,
               [ IsDenseList, IsDenseList ],
function( quivers, vertex_indices )
  local n, i, num_vertices, index, scale;

  n := Length( quivers );
  if n <> Length( vertex_indices ) then
    Error( "lists have different lengths" );
  fi;
  for i in [ 1 .. n ] do
    if not IsQuiver( quivers[ i ] ) then
      Error( "not a quiver: ", quivers[ i ] );
    elif not IsPosInt( vertex_indices[ i ] ) then
      Error( "not a positive integer: ", vertex_indices[ i ] );
    fi;
  od;

  num_vertices := List( quivers, NumberOfVertices );

  index := vertex_indices[ n ];
  scale := num_vertices[ n ];
  for i in Reversed( [ 1 .. ( n - 1 ) ] ) do
    index := index + ( vertex_indices[ i ] - 1 ) * scale;
    scale := scale * num_vertices[ i ];
  od;
  return index;
end );

InstallMethod( ProductQuiverArrowIndex,
               [ IsDenseList, IsPosInt, IsDenseList ],
function( quivers, i, path_indices )
  local n, prod_num_vertices, index, j, scale;

  n := Length( quivers );
  if n <> Length( path_indices ) then
    Error( "lists have different lengths" );
  fi;
  for j in [ 1 .. n ] do
    if not IsQuiver( quivers[ j ] ) then
      Error( "not a quiver: ", quivers[ j ] );
    elif not IsPosInt( path_indices[ j ] ) then
      Error( "not a positive integer: ", path_indices[ j ] );
    fi;
  od;

  prod_num_vertices := Product( List( quivers, NumberOfVertices ) );

  index := 0;
  for j in Reversed( [ ( i + 1 ) .. n ] ) do
    index := index + ( prod_num_vertices / NumberOfVertices( quivers[ j ] ) ) * NumberOfArrows( quivers[ j ] );
  od;

  index := index + path_indices[ n ];
  scale := 1;
  for j in Reversed( [ 2 .. n ] ) do
    if j = i then
      scale := scale * NumberOfArrows( quivers[ j ] );
    else
      scale := scale * NumberOfVertices( quivers[ j ] );
    fi;
    index := index + ( path_indices[ j - 1 ] - 1 ) * scale;
  od;

  return index;
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
  QxR := Quiver( Direction( Q ),
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
  SetProductQuiverFactorsLeftRight( QxR, [ Q ^ LEFT, R ^ RIGHT ] );
  return QxR;
end );

InstallMethod( QuiverProduct,
               [ IsDenseList ],
function( L )
  local n, i, dir, label, v, v_i, a, src, tgt, vertex_labels, 
        arrow_labels, arrow_sources, arrow_targets, labels, sources, 
        targets, prod;

  n := Length( L );

  if IsEmpty( L ) then
    Error( "can not make a quiver product of the empty list" );
  fi;
  i := PositionProperty( L, Q -> not IsQuiver( Q ) );
  if i <> fail then
    Error( "not a quiver: ", L[ i ] );
  fi;
  dir := Direction( L[ 1 ] );
  if not ForAll( L, Q -> Direction( Q ) = dir ) then
    Error( "can not take quiver product of quivers with different directions" );
  fi;

  label := List( L, Label );
  v := List( L, VertexLabels );
  v_i := List( L, Q -> [ 1 .. NumberOfVertices( Q ) ] );
  a := List( L, ArrowLabels );
  src := List( L, ArrowSourceIndices );
  tgt := List( L, ArrowTargetIndices );
  vertex_labels := Cartesian( v );
  arrow_labels := [];
  arrow_sources := [];
  arrow_targets := [];
  for i in Reversed( [ 1 .. n ] ) do
    labels := ShallowCopy( v );
    labels[ i ] := a[ i ];
    Append( arrow_labels, Cartesian( labels ) );
    sources := ShallowCopy( v_i );
    sources[ i ] := src[ i ];
    Append( arrow_sources, List( Cartesian( sources ), vs -> ProductQuiverVertexIndex( L, vs ) ) );
    targets := ShallowCopy( v_i );
    targets[ i ] := tgt[ i ];
    Append( arrow_targets, List( Cartesian( targets ), vs -> ProductQuiverVertexIndex( L, vs ) ) );
  od;

  prod := Quiver( dir, label, vertex_labels, arrow_labels,
                  arrow_sources, arrow_targets );
  
  SetIsProductQuiver( prod, true );
  SetProductQuiverFactors( prod, L );
  return prod;
end );

InstallMethod( IsProductQuiver,
               [ IsQuiver ],
function( Q )
  return ProductQuiverFactors( Q ) <> fail;
end );

InstallMethod( ProductQuiverFactors, "for quiver",
               [ IsQuiver ],
function( Q )
  local n, vertex_labels, num_vertices, v, v_i, step, i, first_vertex, 
        j, vertex_factor_indices, a, src, tgt, arrow_labels, 
        num_arrows, arrow_sources, arrow_targets, arrow_indices, 
        first, Qs;
  
  if not IsList( Label( Q ) ) then
    return fail;
  fi;

  n := Length( Label( Q ) );
  if not ForAll( PrimitivePaths( Q ),
                 p -> IsList( Label( p ) ) and
                      Length( Label( p ) ) = n ) then
    return fail;
  fi;

  # find vertex labels for each factor:
  vertex_labels := VertexLabels( Q );
  num_vertices := Length( vertex_labels );
  v := [];
  v_i := [];
  step := 1;
  for i in Reversed( [ 1 .. n ] ) do
    first_vertex := vertex_labels[ 1 ][ i ];
    v[ i ] := [ first_vertex ];
    for j in [ 1 + step, 1 + step + step .. ( num_vertices - step + 1 ) ] do
      if vertex_labels[ j ][ i ] = first_vertex then
        break;
      else
        Add( v[ i ], vertex_labels[ j ][ i ] );
      fi;
    od;
    step := step * Length( v[ i ] );
    v_i[ i ] := [ 1 .. Length( v[ i ] ) ];
    if num_vertices mod step <> 0 then
      return fail;
    fi;
  od;

  vertex_factor_indices := Cartesian( v_i );
  
  # find arrow labels, sources and targets for each factor:
  a := [];
  src := [];
  tgt := [];
  arrow_labels := ArrowLabels( Q );
  num_arrows := Length( arrow_labels );
  arrow_sources := ArrowSourceIndices( Q );
  arrow_targets := ArrowTargetIndices( Q );

  for i in [ 1 .. n ] do
    a[ i ] := [];
    src[ i ] := [];
    tgt[ i ] := [];
    arrow_indices := PositionsProperty( Arrows( Q ),
                                        a -> Label( Source( a ) )[ i ] <> Label( Target( a ) )[ i ] );
    if Length( arrow_indices ) = 0 then
      continue;
    elif Length( arrow_indices ) mod ( num_vertices / Length( v[ i ] ) ) <> 0 then
      return fail;
    fi;
    first := true;
    for j in arrow_indices do
      if ( not first ) then
        if arrow_labels[ j ][ i ] = a[ i ][ Length( a[ i ] ) ] then
          continue;
        elif arrow_labels[ j ][ i ] = a[ i ][ 1 ] then
          break;
        fi;
      fi;
      Add( a[ i ], arrow_labels[ j ][ i ] );
      Add( src[ i ], vertex_factor_indices[ arrow_sources[ j ] ][ i ] );
      Add( tgt[ i ], vertex_factor_indices[ arrow_targets[ j ] ][ i ] );
      first := false;
    od;
  od;

  Qs := ListN( List( [ 1 .. n ], i -> Direction( Q ) ),
               Label( Q ),
               v,
               a,
               src,
               tgt,
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
  return ComposePaths
         ( List( ArrowList( p ),
                 a -> ProjectPathFromProductQuiver( n, a ) ) );
end );

InstallMethod( ProductPathFactors, [ IsPath ],
function( p )
  return List( [ 1 .. Length( ProductQuiverFactors( QuiverOfPath( p ) ) ) ],
               i -> ProjectPathFromProductQuiver( i, p ) );
end );

InstallMethod( ProductPathFactorsLeftRight, [ IsPath ],
function( p )
  local factors;
  factors := ProductPathFactors( p );
  if Length( factors ) <> 2 then
    Error( "left/right factors not defined for products of more than two quivers" );
  fi;
  return [ factors[ 1 ] ^ LEFT, factors[ 2 ] ^ RIGHT ];
end );

InstallMethod( IncludePathInProductQuiver,
               [ IsProductQuiver, IsPosInt, IsList, IsPrimitivePath ],
function( PQ, n, vertices, p )
  local factors, i, paths;

  factors := ProductQuiverFactors( PQ );
  if factors[ n ] <> QuiverOfPath( p ) then
    Error( "The path ", p, " is not in factor ", n, " of the product quiver" );
  fi;
  for i in [ 1 .. Length( factors ) ] do
    if i = n then
      continue;
    elif not IsBound( vertices[ i ] ) then
      Error( "Vertex ", i, " missing" );
    elif not IsQuiverVertex( vertices[ i ] ) then
      Error( "Element ", i, " in vertex list is not a vertex" );
    elif factors[ i ] <> QuiverOfPath( vertices[ i ] ) then
      Error( "Vertex ", i, " is not in factor ", i, " of the product quiver" );
    fi;
  od;
  paths := List( vertices, VertexIndex );
  if IsQuiverVertex( p ) then
    paths[ n ] := VertexIndex( p );
    return Vertex( PQ, ProductQuiverVertexIndex( factors, paths ) );
  else # IsArrow( p )
    paths[ n ] := ArrowIndex( p );
    return Arrow( PQ, ProductQuiverArrowIndex( factors, n, paths ) );
  fi;
end );

InstallMethod( IncludePathInProductQuiver,
               [ IsProductQuiver, IsPosInt, IsList, IsCompositePath ],
function( PQ, n, vertices, p )
  return TranslatePath( p, a -> IncludePathInProductQuiver( PQ, n, vertices, a ) );
end );

InstallMethod( PathInProductQuiver,
               [ IsProductQuiver, IsDenseList, IsPerm ],
function( PQ, paths, permutation )
  local n, i, num_factors, sources, targets, included_paths, vertices, pos;
  num_factors := Length( ProductQuiverFactors( PQ ) );
  if Length( paths ) <> num_factors then
    Error( "Product quiver has ", num_factors,
           " factors, but ", Length( paths ), " paths were given" );
  fi;
  for n in [ 1 .. num_factors ] do
    if not IsPath( paths[ n ] ) then
      Error( "Path ", n, " is not a path" );
    fi;
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
               [ IsProductQuiver, IsDenseList ],
function( PQ, paths )
  return PathInProductQuiver( PQ, paths, () );
end );

InstallMethod( PathInProductQuiver, "for product quiver, two dense lists, and permutation",
               [ IsProductQuiver, IsDenseList, IsDenseList, IsPerm ],
function( PQ, factors, paths, permutation )
  local num_factors, paths_in_order, i;
  num_factors := Length( ProductQuiverFactors( PQ ) );
  if Length( factors ) <> num_factors then
    Error( "Product quiver has ", num_factors,
           " factors, but ", Length( factors ), " factor numbers were given" );
  fi;
  if Length( paths ) <> num_factors then
    Error( "Product quiver has ", num_factors,
           " factors, but ", Length( paths ), " paths were given" );
  fi;
  if PermList( factors ) = fail then
    Error( "Bad factors list" );
  fi;
  paths_in_order := [];
  for i in [ 1 .. num_factors ] do
    paths_in_order[ factors[ i ] ] := paths[ i ];
  od;
  return PathInProductQuiver( PQ, paths_in_order, permutation );
end );

InstallMethod( PathInProductQuiver, "for product quiver and two dense lists",
               [ IsProductQuiver, IsDenseList, IsDenseList ],
function( PQ, factors, paths )
  return PathInProductQuiver( PQ, factors, paths, () );
end );

InstallMethod( ProductQuiverInclusion, "for product quiver and positive integers",
               [ IsProductQuiver, IsPosInt, IsPosInt ],
function( PQ, i, j )
  return ProductQuiverInclusions( PQ )[ i ][ j ];
end );

InstallMethod( ProductQuiverInclusions, "for product quiver",
               [ IsProductQuiver ],
function( PQ )
  local Qs, pt, incs, i, Q, incs_Q, other_quivers, vertices_other, vs, m;
  Qs := ProductQuiverFactors( PQ );
  pt := Quiver( Direction( PQ ), "point(1)" );
  incs := [];
  for i in [ 1 .. Length( Qs ) ] do
    Q := Qs[ i ];
    incs_Q := [];
    other_quivers := ShallowCopy( Qs );
    other_quivers[ i ] := pt;
    vertices_other := Cartesian( List( other_quivers, Vertices ) );
    for vs in vertices_other do
      m := QuiverHomomorphism( Q, PQ,
                               List( Vertices( Q ),
                                     v -> PathInProductQuiver( PQ, Replace( vs, i, v ) ) ),
                               List( Arrows( Q ),
                                     a -> PathInProductQuiver( PQ, Replace( vs, i, a ) ) ) );
      Add( incs_Q, m );
    od;
    Add( incs, incs_Q );
  od;
  return incs;
end );

InstallMethod( FlipProductQuiver, "for product quiver",
               [ IsProductQuiver ],
function( P )
  local Qs, P_flip, images, path, path_factors, flipped_path, 
        images_inv, iso, iso_inv;
  Qs := ProductQuiverFactors( P );
  if Length( Qs ) <> 2 then
    Error( "product quiver to be flipped must have exactly two factors" );
  fi;
  P_flip := QuiverProduct( Qs[ 2 ], Qs[ 1 ] );
  images := [];
  for path in PrimitivePaths( P ) do
    path_factors := ProductPathFactors( path );
    flipped_path := PathInProductQuiver( P_flip, [ path_factors[ 2 ], path_factors[ 1 ] ] );
    Add( images, flipped_path );
  od;
  images_inv := [];
  for path in PrimitivePaths( P_flip ) do
    path_factors := ProductPathFactors( path );
    flipped_path := PathInProductQuiver( P, [ path_factors[ 2 ], path_factors[ 1 ] ] );
    Add( images_inv, flipped_path );
  od;
  iso := QuiverHomomorphism( P, P_flip, images );
  iso_inv := QuiverHomomorphism( P_flip, P, images_inv );
  SetInverseGeneralMapping( iso, iso_inv );
  SetInverseGeneralMapping( iso_inv, iso );
  # set IsIsomorphism?
  return iso;
end );

# InstallMethod( AsLeftQuiver, "for left quiver", [ IsLeftQuiver ], IdFunc );
# InstallMethod( AsLeftQuiver, "for right quiver", [ IsRightQuiver ], OppositeQuiver );
# InstallMethod( AsRightQuiver, "for left quiver", [ IsLeftQuiver ], OppositeQuiver );
# InstallMethod( AsRightQuiver, "for right quiver", [ IsRightQuiver ], IdFunc );

InstallMethod( \^, "for quiver and direction", [ IsQuiver, IsDirection ],
function( Q, dir )
  if dir = Direction( Q ) then
    return Q;
  else
    return OppositeQuiver( Q );
  fi;
end );

InstallMethod( \^, "for list of quivers and side", [ IsDenseList, IsSide ],
function( list, side )
  local Q1, Q2;
  if not ForAll( list, IsQuiver ) then
    TryNextMethod();
  fi;
  if side = LEFT_RIGHT and Length( list ) = 2 then
    Q1 := list[ 1 ];
    Q2 := list[ 2 ];
    return QuiverProduct( Q1 ^ LEFT, Q2 ^ RIGHT );
  else
    return fail;
  fi;
end );

InstallMethod( \^, "for quiver and side", [ IsQuiver, IsSide ],
function( Q, side )
  if side = LEFT_RIGHT then
    return ProductQuiverFactorsLeftRight( Q );
  else
    TryNextMethod();
  fi;
end );

# InstallMethod( AsLeftPath, "for left path", [ IsLeftPath ], IdFunc );
# InstallMethod( AsLeftPath, "for right path", [ IsRightPath ], OppositePath );
# InstallMethod( AsRightPath, "for left path", [ IsLeftPath ], OppositePath );
# InstallMethod( AsRightPath, "for right path", [ IsRightPath ], IdFunc );

InstallMethod( \^, "for path and direction", [ IsPath, IsDirection ],
function( p, dir )
  if dir = Direction( p ) then
    return p;
  else
    return OppositePath( p );
  fi;
end );

InstallMethod( \^, "for list of paths and side", [ IsDenseList, IsSide ],
function( list, side )
  local p1, p2, Q1, Q2;
  if not ForAll( list, IsPath ) then
    TryNextMethod();
  fi;
  if side = LEFT_RIGHT and Length( list ) = 2 then
    p1 := list[ 1 ];
    p2 := list[ 2 ];
    Q1 := QuiverOfPath( p1 );
    Q2 := QuiverOfPath( p2 );
    return PathInProductQuiver( [ Q1, Q2 ] ^ LEFT_RIGHT, [ p1 ^ LEFT, p2 ^ RIGHT ] );
  else
    return fail;
  fi;
end );

InstallMethod( \^, "for path and side", [ IsPath, IsSide ],
function( p, side )
  if side = LEFT_RIGHT then
    return ProductPathFactorsLeftRight( p );
  else
    TryNextMethod();
  fi;
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


InstallMethod( Subquiver, "for quiver and lists", [ IsQuiver, IsDenseList, IsDenseList ],
function( Q, vertices, arrows )
  local new_vertex_index, i, a, vertex_labels, arrow_labels,
        source_indices, target_indices, label;
  new_vertex_index := List( [ 1 .. NumberOfVertices( Q ) ], i -> fail );
  for i in [ 1 .. Length( vertices ) ] do
    new_vertex_index[ VertexIndex( vertices[ i ] ) ] := i;
  od;
  for a in arrows do
    if new_vertex_index[ VertexIndex( Source( a ) ) ] = fail then
      Error( "attempt to create subquiver containing arrow ", a, " but not its source vertex" );
    elif new_vertex_index[ VertexIndex( Target( a ) ) ] = fail then
      Error( "attempt to create subquiver containing arrow ", a, " but not its target vertex" );
    fi;
  od;
  vertex_labels := List( vertices, Label );
  arrow_labels := List( arrows, Label );
  source_indices := List( arrows, a -> new_vertex_index[ VertexIndex( Source( a ) ) ] );
  target_indices := List( arrows, a -> new_vertex_index[ VertexIndex( Target( a ) ) ] );
  if IsString( Label( Q ) ) then
    label := Concatenation( Label( Q ), "_sub" );
  else
    label := Label( Q );
  fi;
  return Quiver( Direction( Q ), label, vertex_labels, arrow_labels,
                 source_indices, target_indices );
end );

InstallMethod( Subquiver, "for quiver and list", [ IsQuiver, IsDenseList ],
function( Q, arrows )
  local vertex_included, a, vertices;
  vertex_included := List( [ 1 .. NumberOfVertices( Q ) ], i -> false );
  for a in arrows do
    vertex_included[ VertexIndex( Source( a ) ) ] := true;
    vertex_included[ VertexIndex( Target( a ) ) ] := true;
  od;
  vertices := Vertices( Q ){ Positions( vertex_included, true ) };
  return Subquiver( Q, vertices, arrows );
end );

InstallMethod( FullSubquiver, "for quiver and list", [ IsQuiver, IsDenseList ],
function( Q, vertices )
  local vertex_included, v, arrows, a;
  vertex_included := List( [ 1 .. NumberOfVertices( Q ) ], i -> false );
  for v in vertices do
    vertex_included[ VertexIndex( v ) ] := true;
  od;
  arrows := [];
  for a in Arrows( Q ) do
    if vertex_included[ VertexIndex( Source( a ) ) ] and
       vertex_included[ VertexIndex( Target( a ) ) ] then
      Add( arrows, a );
    fi;
  od;
  return Subquiver( Q, vertices, arrows );
end );


InstallMethod( IsConnected, "for quiver", [ IsQuiver ],
function( Q )
  local n, queue, i, visited, v;
  n := NumberOfVertices( Q );
  queue := [ Vertex( Q, 1 ) ];
  i := 1;
  visited := List( [ 1 .. n ], i -> false );
  while i <= Length( queue ) do
    v := queue[ i ];
    visited[ VertexIndex( v ) ] := true;
    for n in Neighbors( v ) do
      if not visited[ VertexIndex( n ) ] then
        Add( queue, n );
      fi;
    od;
    i := i + 1;
  od;
  return ForAll( visited, IdFunc );
end );

InstallMethod( ConnectedComponentsAttr, "for quiver", [ IsQuiver ],
function( Q )
  local component_indices, mark_component, num_components, v, component_name;

  component_indices := List( [ 1 .. NumberOfVertices( Q ) ], i -> 0 );

  mark_component := function( start_vertex, comp_nr )
    local queue, i, v, n;
    queue := [ start_vertex ];
    i := 1;
    while i <= Length( queue ) do
      v := queue[ i ];
      component_indices[ VertexIndex( v ) ] := comp_nr;
      for n in Neighbors( v ) do
        if component_indices[ VertexIndex( n ) ] = 0 then
          Add( queue, n );
          component_indices[ VertexIndex( n ) ] := comp_nr;
        fi;
      od;
      i := i + 1;
    od;
  end;

  num_components := 0;
  for v in Vertices( Q ) do
    if component_indices[ VertexIndex( v ) ] > 0 then
      continue;
    fi;
    num_components := num_components + 1;
    mark_component( v, num_components );
  od;

  if IsString( Label( Q ) ) then
    component_name := function( i )
      return Concatenation( Label( Q ), "_comp", String( i ) );
    end;
  else
    component_name := i -> Label( Q );
  fi;

  return List( [ 1 .. num_components ],
               i -> RenameQuiver
                    ( component_name( i ),
                      FullSubquiver( Q, Vertices( Q ){ Positions( component_indices, i ) } ) ) );
end );

InstallMethod( ConnectedComponentsAttr, "for quiver", [ IsQuiver ],
ConnectedComponents );

#######################################################################
##
#A  AdjacencyMatrixOfQuiver( Q )
##
##  Returns the adjacency matrix of the quiver  Q.
##  
InstallMethod ( AdjacencyMatrixOfQuiver, 
"for a quiver",
[ IsQuiver ], 
function( Q )
    
    local   arrows,  n,  mat,  a,  startpos,  endpos;
    
    arrows := Arrows( Q );
    n := Length( Vertices( Q ) );
    mat := NullMat( n, n );
    for a in arrows do
        startpos := VertexIndex( Source( a ) );
        endpos := VertexIndex( Target( a ) );
        mat[ startpos ][ endpos ] := mat[ startpos ][ endpos ] + 1;
    od;
    
    return mat;
end
  );

InstallMethod( SeparatedQuiver, 
"for a quiver",
[ IsQuiver ],
function( Q )

    local   vertices,  oldarrows,  label,  arrows,  sources,  targets,  
            a;
    #
    # Calling the vertices in the separated quiver for <label> and <label>', when
    # <label> is the name of a vertex in the old quiver. For an arrow  a : v ---> w 
    # in the quiver, we call the corresponding arrow  v ---> w'  also  a  in the 
    # separated quiver. 
    #
    vertices := List( Vertices( Q ), v -> String( v ) ); 
    Append( vertices, List( vertices, v -> Concatenation( v, "'" ) ) ); 
    oldarrows := Arrows( Q );
    label := Label( Q );
    arrows := List( oldarrows, a -> String( a ) );
    sources := List( oldarrows, a -> String( Source( a ) ) );
    sources := List( sources, s -> Position( vertices, s ) );
    targets := List( oldarrows, a -> Concatenation( String( Target( a ) ), "'" ) );
    targets := List( targets, t -> Position( vertices, t ) );
    
    return Quiver( Direction( Q ), Concatenation( label, "separated" ), vertices, arrows, sources, targets ); 
end
  );

#############################################################################
##
#P  IsAcyclicQuiver(<Q>)
##
##  This function returns true if a quiver <Q>  does not 
##  contain an oriented cycle. 
##
InstallMethod( IsAcyclicQuiver, "for a quiver",
               [ IsQuiver ],
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
                 a -> arrow_removed[ ArrowIndex( a ) ] ) then
        Add( to_remove, i );
        for a in OutgoingArrows( v ) do
          arrow_removed[ ArrowIndex( a ) ] := true;
        od;
      fi;
    od;
    for i in Reversed( to_remove ) do
      Remove( vertices, i );
    od;
  until Length( vertices ) = 0 or Length( to_remove ) = 0;
  return Length( vertices ) = 0;
end );

#############################################################################
##
#P  IsTreeQuiver(<Q>)
##
##  This function returns true if a quiver <Q> is a tree as a graph
##  (i.e. it is connected and contains no unoriented cycles).
##
InstallMethod( IsTreeQuiver,
"for quivers",
[ IsQuiver ],
function ( Q )
    
    return ( NumberOfArrows( Q ) = NumberOfVertices( Q ) - 1 ) and IsConnected( Q );
end
  ); #IsTreeQuiver

#############################################################################
##
#P  IsUnorientedAcyclicQuiver(<Q>)
##
##  This function returns true if a quiver <Q>  does not 
##  contain an unoriented cycle. 
##  Note: an oriented cycle is also an unoriented cycle
##
InstallMethod( IsUnorientedAcyclicQuiver,
"for quivers",
[ IsQuiver ],
function ( Q )
    
    local Visit, color, GRAY, BLACK, WHITE,
          vertex_list, res, vert, i;
    
    WHITE := 0; GRAY := 1; BLACK := -1;
    Visit := function( v, predecessor )
      local adj, result, vertex, arrow; 
        
      color[ v ] := GRAY;
      adj := [ ];
      for arrow in IncomingArrows( vertex_list[ v ] ) do
        vertex := Position( vertex_list, Source( arrow ) );
        if vertex in adj then
          return false; # (un)oriented cycle of length 1 or 2 detected!
        fi;
        Add( adj, vertex );
      od;
      for arrow in OutgoingArrows( vertex_list[ v ] ) do
        vertex := Position( vertex_list, Target( arrow ) );
        if vertex in adj then
          return false; # (un)oriented cycle of length 1 or 2 detected!
        fi;
        Add( adj, vertex );
      od; 
      # Now adj contains all vertices adjacent to v by arrows incoming to v and outgoing from v 
      
      for vertex in adj do
        if ( color[ vertex ] = GRAY ) and ( predecessor <> vertex ) then
          #cycle detected!
          return false;
        fi;
        if color[ vertex ] = WHITE then
          if not Visit( vertex, v ) then
            return false; 
          fi;
        fi;
      od;
      color[ v ] := BLACK;
      
      return true;
    end;
    
    color := [ ];
    vertex_list := Vertices( Q );
    
    for i in [ 1 .. Length( vertex_list ) ] do
      color[ i ] := WHITE;
    od;
     
    for vert in [ 1 .. Length( vertex_list ) ] do
      if color[ vert ] = WHITE then
        if not Visit( vert, -1 ) then
          return false;
        fi;
      fi;
    od;
    
    return true;
end
  ); #IsUnorientedAcyclicQuiver

#
#  TODO: Find isomorphism to "standard" Dynkin
#
#############################################################################
##
#A  DynkinType( <Q> )
##
##  This function returns the Dynkin type of the quiver <Q>  if it is a 
##  Dynkin quiver or extended Dynkin quiver, i.e. the underlying graph is a 
##  Dynkin diagram or extended Dynkin.
##  Note: function works only for connected quivers.
##
InstallMethod( DynkinType,
"for quivers",
[ IsQuiver ],
function ( Q )
  
    local   GoThrough,  vertices,  degrees,  fork_vertices,  v,  deg,  
            fork_arrows,  star_type,  arr,  neighbors1,  neighbors2,  
            neighbors1_deg,  neighbors2_deg,  pos1,  pos2,  pos,  
            subvertices,  indices,  Qsub,  test;
  
    # uaxiliary function computing the max. length of an unoriented path
    # starting from vertex s, going through the arrow
    # It makes sense only in this particular local situation of a "star" quiver 
    GoThrough := function( s, arrow )
      local temp_s, temp_arr, can_go, counter, arrows;
      
      counter := 0;
      can_go := true;
      temp_s := s;
      temp_arr := arrow;
      while can_go do
        if Target( temp_arr ) <> temp_s then
          temp_s := Target( temp_arr );
        else
          temp_s := Source( temp_arr );
        fi;
        counter := counter + 1;
        arrows := ShallowCopy( IncomingArrows( temp_s ) );
        Append( arrows, OutgoingArrows( temp_s ) );
        arrows := Difference( arrows, [ temp_arr ] );
        if Length( arrows ) = 1 then
          temp_arr := arrows[ 1 ];
        else
          can_go := false;
        fi;
      od;
      
      return counter;
    end; # GoThrough
    
    if not IsConnected( Q ) then
      Print( "Quiver is not connected.\n" );
      return false;
    fi;
  
    vertices := Vertices( Q );
    degrees := [ ];
    fork_vertices := [ ]; 
    
    # collecting info on degrees of vertices
    for v in vertices do
      deg := DegreeOfVertex( v );
      if deg > 2 then
        Add( fork_vertices, v );
        Add( degrees, deg );
      fi;
    od;
    
    if not ( NumberOfArrows( Q ) = NumberOfVertices( Q ) - 1 ) then
      for v in vertices do 
        if DegreeOfVertex( v ) <> 2 then 
          return false;
        fi;
      od;
      SetIsExtendedDynkinQuiver( Q, true );          
      return Concatenation( "A~", String( Length( vertices ) - 1 ) );;
    fi;
        
    if Length( degrees ) = 0 then
        # No vertex of degree > 2 and Q is tree => Q is  A_n
      SetIsDynkinQuiver( Q, true );
      return Concatenation( "A", String( Length( vertices ) ) );
    fi;
    
    if degrees = [ 3 ] then
    # necessary condition for being D_n, E_6,7,8 satisfied
    # (i.e. we have a "star" quiver with 3 arms)  
      fork_arrows := ShallowCopy( IncomingArrows( fork_vertices[ 1 ] ) );
      Append( fork_arrows, OutgoingArrows( fork_vertices[ 1 ] ) );
      star_type := [ ];
      for arr in fork_arrows do
        Add( star_type, GoThrough( fork_vertices[ 1 ], arr ) );
      od;
      Sort( star_type );
        
      if star_type{ [ 1, 2 ] } = [ 1, 1 ] then
        SetIsDynkinQuiver( Q, true );            
        return Concatenation( "D", String( Length( vertices ) ) );
      fi;
      if star_type in [ [ 1, 2, 2 ], [ 1, 2, 3 ], [ 1, 2, 4 ] ] then
          SetIsDynkinQuiver( Q, true );
          return Concatenation( "E", String( Length( vertices ) ) );
      fi;
      if star_type in [ [ 2, 2, 2 ], [ 1, 3, 3 ], [1, 2, 5 ] ] then
        SetIsExtendedDynkinQuiver( Q, true );
        return Concatenation( "E~", String( Length( vertices ) - 1 ) );
      fi;
    fi;
    if degrees = [ 4 ] and ( NumberOfArrows( Q ) = 4 ) and ( NumberOfVertices( Q ) = 5 ) then
      SetIsExtendedDynkinQuiver( Q, true );
      return Concatenation( "D~", String( Length( vertices ) - 1 ) );
    fi;
    if degrees = [ 3, 3 ] then
      neighbors1 := Neighbors( fork_vertices[ 1 ] );
      neighbors2 := Neighbors( fork_vertices[ 2 ] );
      neighbors1_deg := List( neighbors1, v -> DegreeOfVertex( v ) );
      neighbors2_deg := List( neighbors2, v -> DegreeOfVertex( v ) );
      pos1 := Positions( neighbors1_deg, 1 );
      pos2 := Positions( neighbors2_deg, 1 );
      if ( Length( pos1 ) <> 2 ) or ( Length( pos2 ) <> 2 ) then 
        return false;
      else
        pos := Position( neighbors1_deg, 1 );
        subvertices := vertices;
        indices := [ 1..Length( vertices ) ];
        Remove( indices, Position( vertices, neighbors1[ pos ] ) );
        Qsub := FullSubquiver( Q, subvertices{ indices } );
        test := DynkinType( Qsub );
        if test[ 1 ] = 'D' and IsDigitChar( test[2] ) then
          return Concatenation( "D~", String( Length( vertices ) - 1 ) );
        fi;
      fi;
    fi;    
    
    return false;
end
  );

#################################################################
##
#P  IsSpecialBiserialQuiver( <Q> ) 
## 
##  It tests if every vertex in quiver <Q> is a source (resp. target) 
##  of at most 2 arrows.
##
##  NOTE: e.g. path algebra of one loop IS NOT special biserial, but
##        one loop IS special biserial quiver.
##
InstallMethod( IsSpecialBiserialQuiver,
"for quivers",
[ IsQuiver ],
function ( Q )
    
    local vertex_list, v;
    
    vertex_list := Vertices( Q );
    for v in vertex_list do
        if Outdegree( v ) > 2 then
            return false;
        fi;
        if Indegree( v ) > 2 then
            return false;
        fi;
    od;
    
    return true;
end
); # IsSpecialBiserialQuiver

InstallOtherMethod( Quiver, 
"for a direction, label and a positive integral matrix",
[ IsDirection, IsString, IsMatrix ],
function( direction, label, matrix )
    
    local   dims,  vertices,  arrows,  arrow_count,  sources,  targets,  
            i,  j,  u,  v,  k;
    
    dims := DimensionsMat( matrix ); 
    if dims[ 1 ] <> dims[ 2 ] then
      Error( "The adjacency matrix must be square." );
    fi;
    vertices := List( [ 1..dims[ 1 ] ], i -> Concatenation( "v", String( i ) ) ); 
    
    arrows := [ ];
    arrow_count := 0;
    sources := [ ];
    targets := [ ]; 
    for i in [ 1 .. dims[ 1 ] ] do
        for j in [ 1..dims[ 2 ] ] do
            u := vertices[i];
            v := vertices[j];
            for k in [ 1..matrix[ i ][ j ] ] do
                arrow_count := arrow_count + 1; 
                arrows[ arrow_count ] := Concatenation( "a", String( arrow_count ) );
                sources[ arrow_count ] := Position( vertices, u );
                targets[ arrow_count ] := Position( vertices, v );
            od;
        od;
    od;
    
    return Quiver( direction, label , vertices, arrows, sources, targets );
end
  );
