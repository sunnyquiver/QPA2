DeclareRepresentation( "IsVertexRep", IsComponentObjectRep,
                       [ "quiver", "number" ] );
DeclareRepresentation( "IsArrowRep", IsComponentObjectRep,
                       [ "quiver", "label", "number", "source", "target" ] );
DeclareRepresentation( "IsCompositePathRep", IsComponentObjectRep,
                       [ "arrows" ] );
DeclareRepresentation( "IsQuiverRep", IsComponentObjectRep,
		       [ "name", "vertices", "arrows",
		         "vertices_desc", "arrows_desc"] );

InstallGlobalFunction( Quiver,
  function( name, num_vertices, arrows_desc )
    local pathFam, quiverFam, vertexType, arrowType, quiverType,
    	  makeVertex, makeArrow,
          vertices, arrows, Q, i, v, a;

    pathFam := NewFamily( Concatenation( "paths of ", name ), IsPath );
    quiverFam := CollectionsFamily( pathFam );

    quiverType := NewType( quiverFam, IsQuiver and IsQuiverRep );
    Q := Objectify( quiverType,
                    rec( name := name,
#		         vertices := vertices, arrows := arrows,
			 vertices_desc := num_vertices,
			 arrows_desc := arrows_desc ) );

    vertexType := NewType( pathFam, IsVertex and IsVertexRep );
    makeVertex := i -> Objectify( vertexType,
                                  rec( quiver := Q, number := i ) );
    vertices := List( [ 1 .. num_vertices ], makeVertex );
    # vertices := [];
    # for i in [ 1 .. Length( vertices_desc ) ] do
    #   v := Objectify( vertexType,
    #                   rec( number := i,
    # 		           label := vertices_desc[ i ] ) );
    #   Add( vertices, v);
    # od;
    Q!.vertices := vertices;

    arrowType := NewType( pathFam, IsArrow and IsArrowRep );
    makeArrow := function( a, i )
      return Objectify( arrowType,
             		rec( quiver := Q,
			     number := i,
			     label := a[ 1 ],
 			     source := vertices[ a[ 2 ] ],
 			     target := vertices[ a[ 3 ] ] ) );
    end;
    arrows := ListN( arrows_desc, [ 1 .. Length( arrows_desc ) ],
    	      	     makeArrow );
    Q!.arrows := arrows;

    return Q;
end );

InstallMethod( ContainingQuiver,
               "for vertex",
	       [ IsVertex and IsVertexRep ],
function( a ) return a!.quiver; end );
InstallMethod( ContainingQuiver,
               "for arrow",
	       [ IsArrow and IsArrowRep ],
function( a ) return a!.quiver; end );
InstallMethod( ContainingQuiver,
               "for composite path",
	       [ IsCompositePath and IsCompositePathRep ],
function( a ) return ContainingQuiver( a!.arrows[1] ); end );

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
  return Source( p!.arrows[ Length( p!.arrows ) ] );
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
  return Target( p!.arrows[ 1 ] );
end );

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

InstallMethod( Label,
               "for arrow",
	       [ IsArrow and IsArrowRep ],
function( a )
  return a!.label;
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
  return Source( p1 ) = Target( p2 );
end );

InstallMethod( \*,
               "for vertex and path",
	       [ IsVertex, IsPath ],
function( v, p )
  if v = Target( p ) then
    return p;
  else
    return fail;
  fi;
end );

InstallMethod( \*,
               "for path and vertex",
	       [ IsPath, IsVertex ],
function( p, v )
  if v = Source( p ) then
    return p;
  else
    return fail;
  fi;
end );

InstallMethod( \*,
               "for two arrows",
	       [ IsArrow, IsArrow ],
function( a1, a2 )
  if Source( a1 ) = Target( a2 ) then
    return PathFromArrowList( [ a1, a2 ] );
  else
    return fail;
  fi;
end );

InstallMethod( \*,
               "for two nontrivial paths",
	       [ IsNontrivialPath, IsNontrivialPath ],
function( p1, p2 )
  if Source( p1 ) = Target( p2 ) then
    return PathFromArrowList( Concatenation( ArrowList( p1 ), ArrowList( p2 ) ) );
  else
    return fail;
  fi;
end );

InstallMethod( PathFromArrowList,
               "for list",
               [ IsList ],
function( list )
  if Length( list ) > 1 then
    return Objectify( NewType( FamilyObj( list[ 1 ] ),
                               IsCompositePath and IsCompositePathRep ),
                      rec( arrows := list ) );
  elif Length( list ) = 1 then
    return list[ 1 ];
  else
    return fail;
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
    return Target( a );
  elif from = 0 and to = 1 then
    return a;
  elif from = 1 and to = 1 then
    return Source( a );
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
      return Target( p );
    else
      return Source( list[ from ] );
    fi;
  else
    return PathFromArrowList( list{ [ ( from + 1 ) .. to ] } );
  fi;
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

InstallMethod( Vertex, "for quiver and int",
	       [ IsQuiver and IsQuiverRep, IsInt ],
function( Q, i )
  return Q!.vertices[ i ];
end );

InstallMethod( \[\], "for quiver and int",
	       [ IsQuiver and IsQuiverRep, IsInt ],
function( Q, i )
  return Q!.vertices[ i ];
end );

InstallMethod( Arrow, "for quiver and object",
	       [ IsQuiver and IsQuiverRep, IsObject ],
function( Q, label )
  local a;
  for a in Q!.arrows do
    if a!.label = label then
      return a;
    fi;
  od;
  return fail;
end );

InstallMethod( \^, "for quiver and object",
	       [ IsQuiver and IsQuiverRep, IsObject ],
	       Arrow );
InstallMethod( PathFromString, "for quiver and string",
	       [ IsQuiver, IsString ],
function( Q, string )
  return Product( List( string, label -> Arrow( Q, label ) ) );
end );

InstallMethod( \., "for quiver and positive integer",
	       [ IsQuiver, IsPosInt ],
function( Q, string_as_int )
  return PathFromString( Q, NameRNam( string_as_int ) );
end );

InstallMethod( PrintObj,
               "for vertex",
	       [ IsVertex and IsVertexRep ],
function( v )
  Print( "<vertex ", VertexNumber( v ), " in ", Name( ContainingQuiver( v ) ), ">" );
end );

InstallMethod( ViewObj,
               "for vertex",
	       [ IsVertex and IsVertexRep ],
function( v )
  Print( "(", VertexNumber( v ), ")" );
end );

InstallMethod( PrintObj,
               "for arrow",
	       [ IsArrow and IsArrowRep ],
function( a )
  Print( "<arrow ", [Label( a )], ": ", VertexNumber( Source( a ) ), " -> ", VertexNumber( Target( a ) ),
         " in ", Name( ContainingQuiver( a ) ), ">" );
end );

InstallMethod( ViewObj,
               "for arrow",
	       [ IsArrow and IsArrowRep ],
function( a )
#  Print( "(", [Label( a )], ": ", VertexNumber( Source( a ) ), " -> ", VertexNumber( Target( a ) ), ")" );
  Print( "(", [Label( a )], ")" );
end );

InstallMethod( PrintObj,
               "for composite path",
	       [ IsCompositePath and IsCompositePathRep ],
function( p )
  Print( "(",
         Iterated( List( List( ArrowList( p ), Label ), l -> [l] ),# TODO! String ),
                   function( a1, a2 ) return Concatenation( a1, "*", a2 ); end ),
         ")" );
end );

InstallMethod( PrintObj,
               "for quiver",
	       [ IsQuiver and IsQuiverRep ],
function( Q )
  Print( "Quiver( ", Name( Q ), ", ", Q!.vertices_desc, ", ",
         Q!.arrows_desc, " )" );
end );

InstallMethod( Name,
	       "for quiver",
	       [ IsQuiver and IsQuiverRep ],
function( Q )
  return Q!.name;
end );

InstallMethod( \=,
               "for vertices",
 	       [ IsVertex and IsVertexRep, IsVertex and IsVertexRep ],
	       IsIdenticalObj );
InstallMethod( \=,
               "for arrows",
 	       [ IsArrow and IsArrowRep, IsArrow and IsArrowRep ],
	       IsIdenticalObj );
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
      fi;
    od;
    return false;
  fi;
end );



InstallMethod( \/, "for two vertices",
               IsIdenticalObj,
               [ IsVertex, IsVertex ],
function( v1, v2 )
  if v1 = v2 then
    return [ v1, v1 ];
  else
    return fail;
  fi;
end );

InstallMethod( \/, "for vertex and nontrivial path",
               IsIdenticalObj,
               [ IsVertex, IsNontrivialPath ],
               ReturnFail );

InstallMethod( \/, "for nontrivial path and vertex",
               IsIdenticalObj,
               [ IsNontrivialPath, IsVertex ],
function( p, v )
  local a, i, list;
  if Target( p ) = v then
    return [ v, p ];
  elif Source( p ) = v then
    return [ p, v ];
  fi;
  list := ArrowList( p );
  for i in [ 2 .. Length( list ) ] do
    a := list[ i ];
    if Target( a ) = v then
      return [ PathFromArrowList( List( [ 1 .. ( i - 1 ) ],
                                        j -> list[ j ] ) ),
               PathFromArrowList( List( [ i .. Length( list ) ],
                                        j -> list[ j ] ) ) ];
    fi;
  od;
  return fail;
end );

InstallMethod( \/, "for two nontrivial paths",
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
      arrows_left := List( [ 1 .. ( i - 1 ) ], n -> list_p[ n ] );
      arrows_right := List( [ ( i + len_q ) .. len_p ], n -> list_p[ n ] );
      if i = 1 then
        left := Target( p );
      else
        left := PathFromArrowList( arrows_left );
      fi;
      if i + len_q = len_p + 1 then
        right := Source( p );
      else
        right := PathFromArrowList( arrows_right );
      fi;
      return [ left, right ];
    fi;
  od;
  return fail;
end );

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
  list_p := ArrowList( p );
  list_q := ArrowList( q );
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
      b := Subpath( p, 0, len_p - overlap_len );
      c := Subpath( q, overlap_len, len_q );
      Add( overlaps, [ b, c ] );
    fi;
  od;
  return overlaps;
end );


# test

Q1 := Quiver( "Q1", 3, [['a', 1, 2], ['b', 2, 3]] );
Q2 := Quiver( "Q2", 1, [['a', 1, 1]] );
Q3 := Quiver( "Q3", 3,
              [ [ 'a', 1, 2 ],
                [ 'b', 2, 3 ],
                [ 'c', 3, 1 ],
                [ 'd', 1, 1 ] ] );
