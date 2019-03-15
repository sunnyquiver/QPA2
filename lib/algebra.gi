DeclareDirectionOperations( IsQuiverAlgebra, IsLeftQuiverAlgebra, IsRightQuiverAlgebra );

BindGlobal( "FamilyOfQuiverAlgebraElements", NewFamily( "quiver algebra elements" ) );
BindGlobal( "FamilyOfQuiverAlgebras", CollectionsFamily( FamilyOfQuiverAlgebraElements ) );

DeclareRepresentation( "IsPathAlgebraElementRep", IsComponentObjectRep,
                       [ "algebra", "paths", "coefficients" ] );

InstallMethod( QuiverAlgebraElement,
               "for path algebra and homogeneous lists",
               [ IsPathAlgebra, IsHomogeneousList, IsHomogeneousList ],
function( algebra, coefficients, paths )
  local field, Q, Cs, Ps, Cs_, Ps_, p, i, nonzeroIndices;
  field := LeftActingDomain( algebra );
  Q := QuiverOfAlgebra( algebra );
  if not ForAll( coefficients, c -> c in field ) then
    Error( "Coefficients not in field" );
  fi;
  if not ForAll( paths, p -> p in Q ) then
    Error( "Paths not in quiver" );
  fi;
  Cs_ := ShallowCopy( coefficients );
  Ps_ := ShallowCopy( paths );
  SortParallel( Ps_, Cs_ );
  p := fail;
  Cs := [];
  Ps := [];
  for i in [ 1 .. Length( Ps_ ) ] do
    if Ps_[ i ] = p then
      Cs[ 1 ] := Cs[ 1 ] + Cs_[ i ];
    else
      Add( Ps, Ps_[ i ], 1 );
      Add( Cs, Cs_[ i ], 1 );
    fi;
    p := Ps_[ i ];
  od;
  nonzeroIndices := PositionsProperty( Cs, c -> not IsZero( c ) );
  Cs := Cs{ nonzeroIndices };
  Ps := Ps{ nonzeroIndices };
  return QuiverAlgebraElementNC( algebra, Cs, Ps );
end );

InstallMethod( QuiverAlgebraElementNC,
               "for path algebra and homogeneous lists",
               [ IsPathAlgebra, IsHomogeneousList, IsHomogeneousList ],
function( algebra, coefficients, paths )
  return Objectify( NewType( FamilyOfQuiverAlgebraElements,
                             IsPathAlgebraElement and IsPathAlgebraElementRep ),
                    rec( algebra := algebra,
                         paths := paths,
                         coefficients := coefficients ) );
end );

InstallMethod( PathAsAlgebraElement, "for path algebra and path",
               [ IsPathAlgebra, IsPath ],
function( A, p )
  if QuiverOfPath( p ) <> QuiverOfAlgebra( A ) then
    Error( "Path from wrong quiver" );
  fi;
  return QuiverAlgebraElementNC( A, [ One( LeftActingDomain( A ) ) ], [ p ] );
end );

InstallMethod( VerticesAttr, [ IsQuiverAlgebra ],
function( A )
  return List( Vertices( QuiverOfAlgebra( A ) ),
               e -> PathAsAlgebraElement( A, e ) );
end );

InstallMethod( Vertices, [ IsQuiverAlgebra ],
VerticesAttr );

InstallMethod( Arrows, [ IsQuiverAlgebra ],
function( A )
  return List( Arrows( QuiverOfAlgebra( A ) ),
               e -> PathAsAlgebraElement( A, e ) );
end );

InstallMethod( PrimitivePaths, [ IsQuiverAlgebra ],
function( A )
  return List( PrimitivePaths( QuiverOfAlgebra( A ) ),
               e -> PathAsAlgebraElement( A, e ) );
end );

InstallMethod( String, "for element of path algebra",
               [ IsPathAlgebraElement ],
function( e )
  local Cs, Ps, i, s;
  Cs := Coefficients( e );
  Ps := Paths( e );
  if Length( Ps ) = 0 then
    return "0";
  fi;
  s := Concatenation( String( Cs[ 1 ] ), "*(", String( Ps[ 1 ] ), ")" );
  for i in [ 2 .. Length( Cs ) ] do
    if IsNegRat( Cs[ i ] ) then
      s := Concatenation( s, " - ", String( - Cs[ i ] ) );
    else
      s := Concatenation( s, " + ", String( Cs[ i ] ) );
    fi;
    s := Concatenation( s, "*(", String( Ps[ i ] ), ")" );
  od;
  return s;
end );

InstallMethod( PrintObj, "for element of path algebra",
               [ IsPathAlgebraElement ],
function( e )
  Print( String( e ) );
end );

InstallMethod( ViewObj, "for element of path algebra",
               [ IsPathAlgebraElement ],
function( e )
  Print( String( e ) );
end );

InstallMethod( AlgebraOfElement, "for element of path algebra",
               [ IsPathAlgebraElement and IsPathAlgebraElementRep ],
function( e )
  return e!.algebra;
end );

InstallMethod( CoefficientsAttr, "for element of path algebra",
               [ IsPathAlgebraElement and IsPathAlgebraElementRep ],
function( e )
  return e!.coefficients;
end );

InstallMethod( Coefficients, "for element of path algebra",
               [ IsQuiverAlgebraElement ],
CoefficientsAttr );

InstallMethod( Paths, "for element of path algebra",
               [ IsPathAlgebraElement and IsPathAlgebraElementRep ],
function( e )
  return e!.paths;
end );

InstallMethod( IsLeftUniform, "for element of quiver algebra",
               [ IsQuiverAlgebraElement ],
function( e )
  local v;
  if IsZero( e ) then
    return true;
  fi;
  v := LeftEnd( LeadingPath( e ) );
  return ForAll( Paths( e ), p -> LeftEnd( p ) = v );
end );

InstallMethod( IsRightUniform, "for element of quiver algebra",
               [ IsQuiverAlgebraElement ],
function( e )
  local v;
  if IsZero( e ) then
    return true;
  fi;
  v := RightEnd( LeadingPath( e ) );
  return ForAll( Paths( e ), p -> RightEnd( p ) = v );
end );

InstallMethod( IsUniform, "for element of quiver algebra",
               [ IsQuiverAlgebraElement ],
function( e )
  return IsLeftUniform( e ) and IsRightUniform( e );
end );

InstallMethod( Zero, "for element of quiver algebra",
               [ IsQuiverAlgebraElement ],
function( e )
  return Zero( AlgebraOfElement( e ) );
end );

InstallMethod( One, "for element of quiver algebra",
               [ IsQuiverAlgebraElement ],
function( e )
  return One( AlgebraOfElement( e ) );
end );

InstallMethod( AdditiveInverse, "for element of path algebra",
               [ IsPathAlgebraElement ],
function( e )
  return QuiverAlgebraElement
         ( AlgebraOfElement( e ),
           List( Coefficients( e ), AdditiveInverse ),
           Paths( e ) );
end );

InstallMethod( \=, "for elements of path algebra",
               [ IsPathAlgebraElement, IsPathAlgebraElement ],
function( e1, e2 )
  return AlgebraOfElement( e1 ) = AlgebraOfElement( e2 )
         and Paths( e1 ) = Paths( e2 )
         and Coefficients( e1 ) = Coefficients( e2 );
end );

InstallMethod( \+, "for elements of path algebra", IsIdenticalObj,
               [ IsPathAlgebraElement, IsPathAlgebraElement ],
function( e1, e2 )
  local Cs1, Ps1, Cs2, Ps2, Cs, Ps, i, j, c1, c2, p1, p2;
  Cs1 := Coefficients( e1 ); Ps1 := Paths( e1 );
  Cs2 := Coefficients( e2 ); Ps2 := Paths( e2 );
  Cs := []; Ps := [];
  i := 1; j := 1;
  while i <= Length( Cs1 ) and j <= Length( Cs2 ) do
    c1 := Cs1[ i ]; c2 := Cs2[ j ];
    p1 := Ps1[ i ]; p2 := Ps2[ j ];
    if p1 = p2 then
      if not IsZero( c1 + c2 ) then
        Add( Cs, c1 + c2 ); Add( Ps, p1 );
      fi;
      i := i + 1;
      j := j + 1;
    elif p1 < p2 then
      Add( Cs, c1 ); Add( Ps, p1 );
      i := i + 1;
    else
      Add( Cs, c2 ); Add( Ps, p2 );
      j := j + 1;
    fi;
  od;
  while i <= Length( Cs1 ) do
    Add( Cs, Cs1[ i ] ); Add( Ps, Ps1[ i ] );
    i := i + 1;
  od;
  while j <= Length( Cs2 ) do
    Add( Cs, Cs2[ j ] ); Add( Ps, Ps2[ j ] );
    j := j + 1;
  od;
  return QuiverAlgebraElement( AlgebraOfElement( e1 ), Cs, Ps );
end );

InstallMethod( \*, "for elements of path algebra", IsIdenticalObj,
               [ IsPathAlgebraElement, IsPathAlgebraElement ],
function( e1, e2 )
  local Cs, Ps, nonzeros;
  Cs := List( Cartesian( Coefficients( e1 ), Coefficients( e2 ) ), c -> c[ 1 ] * c[ 2 ] );
  Ps := List( Cartesian( Paths( e1 ), Paths( e2 ) ), p -> p[ 1 ] * p[ 2 ] );
  nonzeros := PositionsProperty( Ps, p -> p <> fail );
  return QuiverAlgebraElement
         ( AlgebraOfElement( e1 ),
           Cs{ nonzeros },
           Ps{ nonzeros } );
end );

InstallMethod( \*, "for path and element of quiver algebra",
               [ IsPath, IsQuiverAlgebraElement ],
function( p, e )
  return PathAsAlgebraElement( AlgebraOfElement( e ), p ) * e;
end );

InstallMethod( \*, "for element of quiver algebra and path",
               [ IsQuiverAlgebraElement, IsPath ],
function( e, p )
  return e * PathAsAlgebraElement( AlgebraOfElement( e ), p );
end );

InstallMethod( \*, "for multiplicative element and element of path algebra",
               [ IsMultiplicativeElement, IsPathAlgebraElement ],
function( c, e )
  if c in LeftActingDomain( AlgebraOfElement( e ) ) then
    return QuiverAlgebraElement
           ( AlgebraOfElement( e ),
             c * Coefficients( e ),
             Paths( e ) );
  else
    TryNextMethod();
  fi;
end );

InstallMethod( \*, "for element of quiver algebra and multiplicative element",
               [ IsQuiverAlgebraElement, IsMultiplicativeElement ],
function( e, c )
  if c in LeftActingDomain( AlgebraOfElement( e ) ) then
    return c * e;
  else
    TryNextMethod();
  fi;
end );

InstallMethod( ComposeElements, "for elements of quiver algebra",
               [ IsQuiverAlgebraElement, IsQuiverAlgebraElement ],
function( e1, e2 )
  local A;
  A := AlgebraOfElement( e1 );
  if AlgebraOfElement( e2 ) <> A then
    Error( "elements from different algebras" );
  fi;
  if IsLeftQuiverAlgebra( A ) then
    return e2 * e1;
  else
    return e1 * e2;
  fi;
end );

InstallMethod( PathAction, "for element of quiver algebra and path",
               [ IsQuiverAlgebraElement, IsPath ],
function( a, p )
  local Cs, Ps, nonzeros;
  Cs := Coefficients( a );
  Ps := List( Paths( a ), q -> ComposePaths( q, p ) );
  nonzeros := PositionsProperty( Ps, q -> q <> fail );
  return QuiverAlgebraElement
         ( AlgebraOfElement( a ),
           Cs{ nonzeros },
           Ps{ nonzeros } );
end );

InstallMethod( TranslateAlgebraElement,
               [ IsQuiverAlgebraElement, IsPathAlgebra, IsFunction ],
function( e, A, f )
  return QuiverAlgebraElement( A, Coefficients( e ),
                             List( Paths( e ), f ) );
end );

InstallMethod( TranslateAlgebraElement,
               [ IsQuiverAlgebraElement, IsQuotientOfPathAlgebra, IsFunction ],
function( e, A, f )
  return QuotientOfPathAlgebraElement
         ( A, TranslateAlgebraElement( e, PathAlgebra( A ), f ) );
end );

InstallMethod( LeadingPath, "for element of path algebra",
               [ IsPathAlgebraElement ],
function( e )
  return Paths( e )[ 1 ];
end );

InstallMethod( LeadingCoefficient, "for element of path algebra",
               [ IsPathAlgebraElement ],
function( e )
  return Coefficients( e )[ 1 ];
end );

InstallMethod( LeadingTerm, "for element of path algebra",
               [ IsPathAlgebraElement ],
function( e )
  local Cs, Ps, len;
  Cs := Coefficients( e );
  Ps := Paths( e );
  return QuiverAlgebraElementNC( AlgebraOfElement( e ),
                                 [ Cs[ 1 ] ], [ Ps[ 1 ] ] );
end );

InstallMethod( NonLeadingTerms, "for element of path algebra",
               [ IsPathAlgebraElement ],
function( e )
  local Cs, Ps, len;
  Cs := Coefficients( e );
  Ps := Paths( e );
  len := Length( Ps );
  return QuiverAlgebraElementNC( AlgebraOfElement( e ),
                                 Cs{ [ 2 .. len ] },
                                 Ps{ [ 2 .. len ] } );
end );

InstallMethod( DivideByList, "for element of path algebra and list",
               [ IsPathAlgebraElement, IsList ],
function( e, divisors )
  local A, remainder, Ps, Cs, d_lp, d_lc, n_div, quotients,
        divisionOccured, lp, lc, i, q, left_q, right_q;
  A := AlgebraOfElement( e );
  remainder := Zero( e );
  Ps := Paths( e );
  Cs := Coefficients( e );
  d_lp := List( divisors, LeadingPath );
  d_lc := List( divisors, LeadingCoefficient );
  n_div := Length( divisors );
  quotients := List( [ 1 .. n_div ], i -> [] );
  while not IsZero( e ) do
    #Print( "loop\n" );
    #Print( "e = ", e, "\n" );
    #Print( "r = ", remainder, "\n" );
    #if InputFromUser( "continue? " ) = 0 then break; fi;
    divisionOccured := false;
    for i in [ 1 .. n_div ] do
      lp := LeadingPath( e );
      lc := LeadingCoefficient( e );
      q := lp / d_lp[ i ];
      if q = fail then
        continue;
      fi;
      left_q := ( lc / d_lc[ i ] ) * PathAsAlgebraElement( A, q[ 1 ] );
      right_q := PathAsAlgebraElement( A, q[ 2 ] );
      e := e - left_q * divisors[ i ] * right_q;
      Add( quotients[ i ], [ left_q, right_q ] );
      divisionOccured := true;
      if IsZero( e ) then
        break;
      fi;
      #Print( "i = ", i, "\n" );
      #Print( "left_q = ", left_q, "; right_q = ", right_q, "\n" );
      #Print( "e = ", e, "\n" );
    od;
    if not divisionOccured then
      remainder := remainder + LeadingTerm( e );
      e := NonLeadingTerms( e );
    fi;
  od;
  return [ quotients, remainder ];
end );

InstallMethod( Reduce, "for element of path algebra and list",
               [ IsPathAlgebraElement, IsList ],
function( e, divisors )
  return DivideByList( e, divisors )[ 2 ];
end );

InstallMethod( OverlapRelation,
               "for elements of path algebra",
               [ IsPathAlgebraElement, IsPathAlgebraElement,
                 IsPath, IsPath ],
function( f, g, b, c )
  local A, fc, bg;
  # LeadingPath( f ) * c = b * LeadingPath( g )
  A := AlgebraOfElement( f );
  fc := f * PathAsAlgebraElement( A, c );
  bg := PathAsAlgebraElement( A, b ) * g;
  return ( 1 / LeadingCoefficient( f ) ) * fc - ( 1 / LeadingCoefficient( g ) ) * bg;
end );

InstallMethod( OverlapRelations,
               "for elements of path algebra",
               [ IsPathAlgebraElement, IsPathAlgebraElement ],
function( f, g )
  local lp_f, lp_g, pathOverlaps, overlapRelations, o;
  lp_f := LeadingPath( f );
  lp_g := LeadingPath( g );
  pathOverlaps := PathOverlaps( lp_f, lp_g );
  overlapRelations := [];
  for o in pathOverlaps do
    Add( overlapRelations, OverlapRelation( f, g, o[ 1 ], o[ 2 ] ) );
  od;
  return overlapRelations;
end );

InstallMethod( TipReduce, "for list",
               [ IsHomogeneousList ],
function( relations )
  local iteration, limit, len, didReductions, i, j, q, lc_i, lc_j, r;
  len := Length( relations );
  relations := ShallowCopy( relations );
  iteration := 0;
  limit := 0;
  #Print( "TipReduce\n" );
  while true do
    iteration := iteration + 1;
    if limit > 0 and iteration > limit then
      Error( "TipReduce iteration limit reached" );
    fi;
    #Print( "TipReduce iteration ", iteration, "\n" );
    #Print( "relations:\n" ); for i in [ 1 .. len ] do Print( "  ", relations[ i ], "\n" ); od;
    didReductions := false;
    for i in [ 1 .. len ] do
      for j in [ 1 .. len ] do
        #Print( "TipReduce i = ", i, ", j = ", j, "\n" );
        if i = j or IsZero( relations[ i ] ) or IsZero( relations[ j ] ) then
          continue;
        fi;
        q := LeadingPath( relations[ i ] ) / LeadingPath( relations[ j ] );
        if q <> fail then
          lc_i := LeadingCoefficient( relations[ i ] );
          lc_j := LeadingCoefficient( relations[ j ] );
          r := relations[ i ] - ( lc_i / lc_j ) * ( q[ 1 ] * relations[ j ] * q[ 2 ] );
          relations[ i ] := r;
          #if IsZero( r ) then Print( "!!!!!! r = 0 !!!!!!\n" ); fi;
          didReductions := true;
        fi;
      od;
    od;
    if not didReductions then
      break;
    fi;
  od;
  return Filtered( relations, e -> not IsZero( e ) );
end );

InstallMethod( ComputeGroebnerBasis, "for list",
               [ IsHomogeneousList ],
function( relations )
  local basis, newRelations, basisLen, i, j, overlapRelations, r,
        remainder, qr,
        limit, iteration;
  basis := TipReduce( relations );
  iteration := 0;
  #limit := 100;
  #for iteration in [ 1 .. limit ] do
  while true do
    iteration := iteration + 1;
    #Print( "ComputeGroebnerBasis iteration ", iteration, "\n" );
    #Print( "basis: ", basis, "\n" );
    newRelations := [];
    basisLen := Length( basis );
    for i in [ 1 .. basisLen ] do
      for j in [ 1 .. basisLen ] do
        overlapRelations := OverlapRelations( basis[ i ], basis[ j ] );
        #Print( "Overlaps (", i, ", ", j, "): ", overlapRelations, "\n" );
        for r in overlapRelations do
          qr := DivideByList( r, basis );
          remainder := qr[ 2 ];
          if not IsZero( remainder ) then
            Add( newRelations, remainder );
          fi;
        od;
      od;
    od;
    if Length( newRelations ) > 0 then
      Append( basis, newRelations );
      basis := TipReduce( basis );
    else
      break;
    fi;
  od;
  return basis;
end );


DeclareRepresentation( "IsPathAlgebraRep", IsComponentObjectRep and IsAttributeStoringRep,
                       [ "field", "quiver" ] );

InstallMethod( PathAlgebra, "for field and quiver",
               [ IsField, IsQuiver ],
function( k, Q )
  local type, A;
  type := NewType( FamilyOfQuiverAlgebras,
                   IsPathAlgebra and IsQuiverAlgebra ^ Direction( Q ) and IsPathAlgebraRep );
  A := Objectify( type,
                  rec( field := k,
                       quiver := Q ) );
  return Intern( A );
end );

InstallMethod( PrintObj, "for path algebra",
               [ IsPathAlgebra ],
function( A )
  Print( "<path algebra ", LeftActingDomain( A ), " * ",
         QuiverOfAlgebra( A ), ">" );
end );

InstallMethod( String, "for path algebra",
               [ IsPathAlgebra ],
function( A )
  return Concatenation( String( LeftActingDomain( A ) ), " * ",
                        LabelAsString( QuiverOfAlgebra( A ) ) );
end );

InstallMethod( ViewObj, "for path algebra",
               [ IsPathAlgebra ],
function( A )
  Print( String( A ) );
end );

InstallMethod( QuiverOfAlgebra, "for path algebra",
               [ IsPathAlgebra and IsPathAlgebraRep ],
function( A )
  return A!.quiver;
end );

InstallMethod( LeftActingDomain, "for path algebra",
               [ IsPathAlgebra and IsPathAlgebraRep ],
function( A )
  return A!.field;
end );

InstallMethod( GeneratorsOfAlgebra, "for quiver algebra",
               [ IsQuiverAlgebra ],
function( A )
  return PrimitivePaths( A );
end );

InstallMethod( RelationsOfAlgebra, "for path algebra",
               [ IsPathAlgebra ],
function( A )
  return [];
end );

InstallMethod( PathAlgebra, "for path algebra",
               [ IsPathAlgebra ],
function( A )
  return A;
end );

InstallMethod( Hash, "for quiver algebra",
               [ IsQuiverAlgebra ],
function( A )
  return ( Hash( QuiverOfAlgebra( A ) ) + Hash( LeftActingDomain( A ) ) ) mod HashMod;
end );

InstallMethod( \in, "for element of path algebra and path algebra",
               [ IsPathAlgebraElement, IsPathAlgebra ],
function( e, A )
  return A = AlgebraOfElement( e );
end );

InstallMethod( Zero, "for quiver algebra",
               [ IsQuiverAlgebra ],
function( A )
  return QuiverAlgebraElement( A, [], [] );
end );

InstallMethod( One, "for quiver algebra",
               [ IsQuiverAlgebra ],
function( A )
  local vertices;
  vertices := Vertices( QuiverOfAlgebra( A ) );
  return QuiverAlgebraElement( A,
                               List( vertices, v -> One( LeftActingDomain( A ) ) ),
                               vertices );
end );

InstallMethod( AlgebraElementFromString, "for quiver algebra and string",
               [ IsQuiverAlgebra, IsString ],
function( A, string )
  local path;
  path := PathFromString( QuiverOfAlgebra( A ), string );
  if path = fail then
    Error( "Algebra does not contain a path named \"", string, "\"" );
  fi;
  return PathAsAlgebraElement( A, path );
end );

InstallMethod( \., "for quiver algebra and positive integer",
	       [ IsQuiverAlgebra, IsPosInt ],
function( A, string_as_int )
  return AlgebraElementFromString( A, NameRNam( string_as_int ) );
end );

InstallMethod( AlgebraElementByLabel, "for quiver algebra and object",
               [ IsQuiverAlgebra, IsObject ],
function( A, label )
  local path;
  path := PrimitivePathByLabel( QuiverOfAlgebra( A ), label );
  if path = fail then
    Error( "Algebra does not contain a primitive path with label ", label );
  fi;
  return PathAsAlgebraElement( A, path );
end );

InstallMethod( \[\], "for quiver algebra and int",
	       [ IsQuiverAlgebra, IsObject ],
               AlgebraElementByLabel );


DeclareRepresentation( "IsQuotientOfPathAlgebraRep", IsComponentObjectRep and IsAttributeStoringRep,
                       [ "pathAlgebra", "relations", "ideal" ] );

InstallMethod( QuotientOfPathAlgebra, "for path algebra and homogeneous list",
               [ IsPathAlgebra, IsDenseList ],
function( A, relations )
  local I;
  I := QuiverAlgebraTwoSidedIdeal( A, relations );
  return QuotientOfPathAlgebra( A, I );
end );

InstallMethod( QuotientOfPathAlgebra, "for path algebra and path ideal",
               [ IsPathAlgebra, IsPathAlgebraIdeal ],
function( kQ, I )
  local A;
  if IsZeroIdeal( I ) then
    return kQ;
  fi;
  A := Objectify( NewType( FamilyOfQuiverAlgebras,
                           IsQuotientOfPathAlgebra and IsQuiverAlgebra^Direction( kQ )
                           and IsQuotientOfPathAlgebraRep ),
                  rec( pathAlgebra := kQ,
                       relations := GeneratorsOfIdeal( I ),
                       ideal := I ) );
  return Intern( A );
end );  

InstallMethod( PrintObj, "for quotient of path algebra",
               [ IsQuotientOfPathAlgebra ],
function( A )
  Print( "<quotient of path algebra ", LeftActingDomain( A ), " * ",
         QuiverOfAlgebra( A ), " / ", RelationsOfAlgebra( A ), ">" );
end );

InstallMethod( String, "for quotient of path algebra",
               [ IsQuotientOfPathAlgebra ],
function( A )
  return Concatenation( "(", String( PathAlgebra( A ) ), ") / ",
                        String( RelationsOfAlgebra( A ) ) );
end );

InstallMethod( ViewObj, "for quotient of path algebra",
               [ IsQuotientOfPathAlgebra ],
function( A )
  Print( String( A ) );
end );

InstallMethod( \/, "for path algebra and path ideal",
               [ IsPathAlgebra, IsPathAlgebraIdeal ],
               QuotientOfPathAlgebra );

InstallMethod( \/, "for path algebra and path ideal",
               [ IsPathAlgebra, IsDenseList ],
               QuotientOfPathAlgebra );

InstallMethod( \/, "for quotient of path algebra and quiver algebra ideal",
               [ IsQuotientOfPathAlgebra, IsQuiverAlgebraTwoSidedIdeal ],
function( A, I )
  return A / Generators( I );
end );

InstallMethod( \/, "for quotient of path algebra and dense list",
               [ IsQuotientOfPathAlgebra, IsDenseList ],
function( A, relations )
  if not ForAll( relations, r -> r in A ) then
    Error( "relations must be elements of the algebra" );
  fi;
  return QuotientOfPathAlgebra( PathAlgebra( A ),
                                Concatenation( RelationsOfAlgebra( A ),
                                               List( relations, Representative ) ) );
end );

InstallMethod( QuiverOfAlgebra, "for quotient of path algebra",
               [ IsQuotientOfPathAlgebra ],
function( A )
  return QuiverOfAlgebra( PathAlgebra( A ) );
end );

InstallMethod( LeftActingDomain, "for quotient of path algebra",
               [ IsQuotientOfPathAlgebra ],
function( A )
  return LeftActingDomain( PathAlgebra( A ) );
end );

InstallMethod( RelationsOfAlgebra, "for quotient of path algebra",
               [ IsQuotientOfPathAlgebra and IsQuotientOfPathAlgebraRep ],
function( A )
  return A!.relations;
end );

InstallMethod( IdealOfQuotient, "for quotient of path algebra",
               [ IsQuotientOfPathAlgebra and IsQuotientOfPathAlgebraRep ],
function( A )
  return A!.ideal;
end );

InstallMethod( IdealOfQuotient, "for path algebra",
               [ IsPathAlgebra ],
function( A )
  return QuiverAlgebraTwoSidedIdeal( A, [] );
end );

InstallMethod( PathAlgebra, "for quotient of path algebra",
               [ IsQuotientOfPathAlgebra and IsQuotientOfPathAlgebraRep ],
function( A )
  return A!.pathAlgebra;
end );


DeclareRepresentation( "IsQuotientOfPathAlgebraElementRep", IsComponentObjectRep,
                       [ "algebra", "representative" ] );

InstallGlobalFunction( QuotientOfPathAlgebraElement,
function( algebra, pathAlgebraElement )
  local representative;
  representative := Reduce( pathAlgebraElement,
                            GroebnerBasis( IdealOfQuotient( algebra ) ) );
  return Objectify( NewType( FamilyOfQuiverAlgebraElements,
                             IsQuotientOfPathAlgebraElement and IsQuotientOfPathAlgebraElementRep ),
                    rec( algebra := algebra,
                         representative := representative ) );
end );

InstallMethod( PathAsAlgebraElement, "for quotient of path algebra and path",
               [ IsQuotientOfPathAlgebra, IsPath ],
function( A, p )
  local representative;
  representative := PathAsAlgebraElement( PathAlgebra( A ), p );
  return QuotientOfPathAlgebraElement( A, representative );
end );

InstallMethod( QuiverAlgebraElement, "for quotient of path algebra and homogeneous lists",
               [ IsQuotientOfPathAlgebra, IsHomogeneousList, IsHomogeneousList ],
function( A, coefficients, paths )
  local representative;
  representative := QuiverAlgebraElement( PathAlgebra( A ), coefficients, paths );
  return QuotientOfPathAlgebraElement( A, representative );
end );

InstallMethod( QuiverAlgebraElementNC, "for quotient of path algebra and homogeneous lists",
               [ IsQuotientOfPathAlgebra, IsHomogeneousList, IsHomogeneousList ],
function( A, coefficients, paths )
  local representative;
  representative := QuiverAlgebraElementNC( PathAlgebra( A ), coefficients, paths );
  return QuotientOfPathAlgebraElement( A, representative );
end );

InstallMethod( String, "for element of quotient of path algebra",
               [ IsQuotientOfPathAlgebraElement ],
function( e )
  return Concatenation( "{ ", String(Representative( e )), " }" );
end );

InstallMethod( PrintObj, "for element of quotient of path algebra",
               [ IsQuotientOfPathAlgebraElement ],
function( e )
  Print( String( e ) );
end );

InstallMethod( AlgebraOfElement, "for element of quotient of path algebra",
               [ IsQuotientOfPathAlgebraElement and IsQuotientOfPathAlgebraElementRep ],
function( e )
  return e!.algebra;
end );

InstallMethod( Representative, "for element of quotient of path algebra",
               [ IsQuotientOfPathAlgebraElement and IsQuotientOfPathAlgebraElementRep ],
function( e )
  return e!.representative;
end );

InstallMethod( CoefficientsAttr, "for element of quotient of path algebra",
               [ IsQuotientOfPathAlgebraElement ],
function( e )
  return Coefficients( Representative( e ) );
end );

InstallMethod( Paths, "for element of quotient of path algebra",
               [ IsQuotientOfPathAlgebraElement ],
function( e )
  return Paths( Representative( e ) );
end );

InstallMethod( IsUniform, "for element of quotient of path algebra",
               [ IsQuotientOfPathAlgebraElement ],
function( e )
  return IsUniform( Representative( e ) );
end );

InstallMethod( \in, "for element of quotient of path algebra and quotient of path algebra",
               [ IsQuotientOfPathAlgebraElement, IsQuotientOfPathAlgebra ],
function( e, A )
  return A = AlgebraOfElement( e );
end );

InstallMethod( \=, "for elements of quotient of path algebra",
               [ IsQuotientOfPathAlgebraElement, IsQuotientOfPathAlgebraElement ],
function( e1, e2 )
  return AlgebraOfElement( e1 ) = AlgebraOfElement( e2 )
         and Representative( e1 ) = Representative( e2 );
end );

InstallMethod( AdditiveInverse, "for element of quotient of path algebra",
               [ IsQuotientOfPathAlgebraElement ],
function( e )
  return QuotientOfPathAlgebraElement
         ( AlgebraOfElement( e ),
           AdditiveInverse( Representative( e ) ) );
end );

InstallMethod( \+, "for elements of quotient of path algebra", IsIdenticalObj,
               [ IsQuotientOfPathAlgebraElement, IsQuotientOfPathAlgebraElement ],
function( e1, e2 )
  return QuotientOfPathAlgebraElement
         ( AlgebraOfElement( e1 ),
           Representative( e1 ) + Representative( e2 ) );
end );

InstallMethod( \*, "for elements of quotient of path algebra", IsIdenticalObj,
               [ IsQuotientOfPathAlgebraElement, IsQuotientOfPathAlgebraElement ],
function( e1, e2 )
  return QuotientOfPathAlgebraElement
         ( AlgebraOfElement( e1 ),
           Representative( e1 ) * Representative( e2 ) );
end );

InstallMethod( \*, "for multiplicative element and element of quotient of path algebra",
               [ IsMultiplicativeElement, IsQuotientOfPathAlgebraElement ],
function( c, e )
  if c in LeftActingDomain( AlgebraOfElement( e ) ) then
    return QuotientOfPathAlgebraElement
           ( AlgebraOfElement( e ),
             c * Representative( e ) );
  else
    TryNextMethod();
  fi;
end );

InstallMethod( \=, "for path algebras",
               [ IsPathAlgebra, IsPathAlgebra ],
function( A, B )
  return ( LeftActingDomain( A ) = LeftActingDomain( B ) )
         and ( QuiverOfAlgebra( A ) = QuiverOfAlgebra( B ) );
end );

InstallMethod( \=, "for quotients of path algebras",
               [ IsQuotientOfPathAlgebra, IsQuotientOfPathAlgebra ],
function( A, B )
  return ( PathAlgebra( A ) = PathAlgebra( B ) )
         and ( RelationsOfAlgebra( A ) = RelationsOfAlgebra( B ) );
end );

InstallMethod( \=, "for path algebra and quotient of path algebra",
               [ IsPathAlgebra, IsQuotientOfPathAlgebra ],
               ReturnFalse );

InstallMethod( \=, "for quotient of path algebra and path algebra",
               [ IsQuotientOfPathAlgebra, IsPathAlgebra ],
               ReturnFalse );

InstallMethod( OppositeAlgebra,
               [ IsPathAlgebra ],
function( A )
  return PathAlgebra( LeftActingDomain( A ), OppositeQuiver( QuiverOfAlgebra( A ) ) );
end );

InstallMethod( OppositeAlgebra,
               [ IsQuotientOfPathAlgebra ],
function( A )
  return QuotientOfPathAlgebra( OppositeAlgebra( PathAlgebra( A ) ),
                                List( RelationsOfAlgebra( A ), OppositeAlgebraElement ) );
end );

InstallMethod( OppositeAlgebraElement,
               [ IsQuiverAlgebraElement ],
function( e )
  return QuiverAlgebraElement( OppositeAlgebra( AlgebraOfElement( e ) ),
                               Coefficients( e ),
                               List( Paths( e ), OppositePath ) );
end );

InstallMethod( Direction, [ IsQuiverAlgebra ], A -> Direction( QuiverOfAlgebra( A ) ) );

InstallMethod( Direction, [ IsQuiverAlgebraElement ], a -> Direction( AlgebraOfElement( a ) ) );

InstallMethod( \^, [ IsQuiverAlgebra, IsDirection ],
function( A, dir )
  if dir = Direction( A ) then
    return A;
  else
    return OppositeAlgebra( A );
  fi;
end );

InstallMethod( \^, [ IsQuiverAlgebraElement, IsDirection ],
function( a, dir )
  if dir = Direction( a ) then
    return a;
  else
    return OppositeAlgebraElement( a );
  fi;
end );

InstallMethod( \^, [ IsDenseList, IsSide ],
function( list, side )
  local e1, e2;
  if not ( ForAll( list, IsQuiverAlgebra ) or ForAll( list, IsQuiverAlgebraElement ) ) then
    TryNextMethod();
  fi;
  if side = LEFT_RIGHT and Length( list ) = 2 then
    if IsQuiverAlgebra( list[ 1 ] ) and IsQuiverAlgebra( list[ 2 ] ) then
      return TensorProductOfAlgebras( list[ 1 ]^LEFT, list[ 2 ]^RIGHT );
    elif IsQuiverAlgebraElement( list[ 1 ] ) and IsQuiverAlgebraElement( list[ 2 ] ) then
      e1 := list[ 1 ]^LEFT;
      e2 := list[ 2 ]^RIGHT;
      return ElementaryTensor( e1, e2,
                               TensorProductOfAlgebras( AlgebraOfElement( e1 ), AlgebraOfElement( e2 ) ) );
    fi;
  else
    return fail;
  fi;
end );

InstallMethod( \^, [ IsQuiverAlgebra, IsSide ],
function( A, side )
  local algebras;
  if side = LEFT_RIGHT then
    return TensorProductFactorsLeftRight( A );
  else
    TryNextMethod();
  fi;
end );

InstallMethod( TensorProductOfAlgebras,
               [ IsQuiverAlgebra, IsQuiverAlgebra ],
function( A, B )
  local Qa, Qb, Q, kQ, commutativity_relations, rels_a, rels_b,
        make_commutativity_relation, inc_rel_a, inc_rel_b, T;
  if LeftActingDomain( A ) <> LeftActingDomain( B ) then
    Error( "Algebras over different fields" );
  fi;
  Qa := QuiverOfAlgebra( A );
  Qb := QuiverOfAlgebra( B );
  Q := QuiverProduct( Qa, Qb );
  kQ := PathAlgebra( LeftActingDomain( A ), Q );
  make_commutativity_relation := function( list )
    local comp1, comp2;
    comp1 := PathAsAlgebraElement( kQ, PathInProductQuiver( Q, list, () ) );
    comp2 := PathAsAlgebraElement( kQ, PathInProductQuiver( Q, list, (1,2) ) );
    return comp1 - comp2;
  end;
  commutativity_relations := List( Cartesian( Arrows( Qa ), Arrows( Qb ) ),
                                   make_commutativity_relation );
  inc_rel_a := function( list )
    return TranslateAlgebraElement( list[ 1 ], kQ, p -> PathInProductQuiver( Q, [ p, list[ 2 ] ] ) );
  end;
  inc_rel_b := function( list )
    return TranslateAlgebraElement( list[ 2 ], kQ, p -> PathInProductQuiver( Q, [ list[ 1 ], p ] ) );
  end;
  rels_a := List( Cartesian( RelationsOfAlgebra( A ), Vertices( Qb ) ), inc_rel_a );
  rels_b := List( Cartesian( Vertices( Qa ), RelationsOfAlgebra( B ) ), inc_rel_b );
  T := kQ / Concatenation( commutativity_relations, rels_a, rels_b );
  SetTensorProductFactors( T, [ A, B ] );
  SetTensorProductFactorsLeftRight( T, [ A^LEFT, B^RIGHT ] );
  SetIsTensorProductOfAlgebras( T, true );
  return T;
end );

InstallMethod( TensorProductOfAlgebras, "for list of quiver algebras",
               [ IsDenseList ],
function( As )
  local k, Qs, Q, kQ, n, commutativity_relations, i, j, path_lists, 
        path_lists_prod, p, comp1, comp2, included_relations, rels, rel,
        vertices1, vertices2, vs1, vs2, new_rel, relations, T;

  # TODO: check that algebras are compatible (same field, same direction)
  #       and that list is nonempty
  k := LeftActingDomain( As[ 1 ] );
  Qs := List( As, QuiverOfAlgebra );
  Q := QuiverProduct( Qs );
  kQ := PathAlgebra( k, Q );
  n := Length( As );

  commutativity_relations := [];
  for i in [ 1 .. ( n - 1 ) ] do
    for j in [ ( i + 1 ) .. n ] do
      path_lists := List( Qs, Vertices );
      path_lists[ i ] := Arrows( Qs[ i ] );
      path_lists[ j ] := Arrows( Qs[ j ] );
      path_lists_prod := Cartesian( path_lists );
      for p in path_lists_prod do
        comp1 := PathAsAlgebraElement( kQ, PathInProductQuiver( Q, p, () ) );
        comp2 := PathAsAlgebraElement( kQ, PathInProductQuiver( Q, p, (i,j) ) );
        Add( commutativity_relations, comp1 - comp2 );
      od;
    od;
  od;
             
  included_relations := [];
  for i in [ 1 .. n ] do
    rels := RelationsOfAlgebra( As[ i ] );
    vertices1 := Cartesian( List( [ 1 .. ( i - 1 ) ], j -> Vertices( Qs[ j ] ) ) );
    vertices2 := Cartesian( List( [ ( i + 1 ) .. n ], j -> Vertices( Qs[ j ] ) ) );
    for rel in rels do
      for vs1 in vertices1 do
        for vs2 in vertices2 do
          new_rel :=
            TranslateAlgebraElement( rel, kQ,
                                     p -> PathInProductQuiver( Q, Concatenation( vs1, [ p ], vs2 ) ) );
          Add( included_relations, new_rel );
        od;
      od;
    od;
  od;

  relations := Concatenation( commutativity_relations, included_relations );
  T := kQ / relations;
  SetTensorProductFactors( T, As );
  SetIsTensorProductOfAlgebras( T, true );
  return T;
end );

InstallMethod( IsomorphismToFlatTensorProduct, "for tensor product of algebras",
               [ IsTensorProductOfAlgebras ],
function( T )
  local unpack, As_iter, As_flat, T_iter, T_flat, Q_iter, Q_flat, 
        images;

  unpack := function( A )
    if IsTensorProductOfAlgebras( A ) then
      return List( TensorProductFactors( A ), unpack );
    fi;
    return A;
  end;
  As_iter := unpack( T );
  As_flat := Flat( As_iter );
  
  T_iter := T;
  T_flat := TensorProductOfAlgebras( As_flat );

  Q_iter := QuiverOfAlgebra( T_iter );
  Q_flat := QuiverOfAlgebra( T_flat );

  images := List( PrimitivePaths( Q_iter ),
                  p -> AlgebraElementByLabel( T_flat, Flat( Label( p ) ) ) );

  return QuiverAlgebraHomomorphism( T_iter, T_flat, images );
end );

InstallMethod( IsTensorProductOfAlgebras, "for quiver algebra",
               [ IsQuiverAlgebra ], ReturnFalse ); # TODO should actually check

# InstallMethod( IsTensorProductOfAlgebras,
#                [ IsQuiverAlgebra, IsQuiverAlgebra, IsQuiverAlgebra ],
# function( T, A, B )
#   # TODO: Write a better implementation.
#   # Should not be necessary to recompute the tensor algebra.
#   # Should at least avoid recomputing the Groebner basis.
#   return T = TensorProductOfAlgebras( A, B );
# end );

InstallMethod( TensorProductFactors,
               [ IsQuiverAlgebra ],
function( T )
  # TODO
end );

InstallMethod( ElementaryTensor,
        "for two elements in quiveralgebras",
        [ IsQuiverAlgebraElement, IsQuiverAlgebraElement, IsQuiverAlgebra ],
        function( a, b, T )
    
    local   A1,  A2,  paths_in_a,  coefficients_of_a,  paths_in_b,  
            coefficients_of_b,  paths,  coefficients,  i,  j;
    
    A1 := AlgebraOfElement( a );
    A2 := AlgebraOfElement( b );
    if not ( IsTensorProductOfAlgebras( T ) and
             TensorProductFactors( T ) = [ A1, A2 ] ) then
        Error("Entered elements are not in the entered tensor algebra");
    fi;
    paths_in_a := Paths( a );
    coefficients_of_a := Coefficients( a );
    paths_in_b := Paths( b );
    coefficients_of_b := Coefficients( b );
    paths := [ ];
    coefficients := [ ];
    for i in [ 1..Length( paths_in_a ) ] do
        for j in [ 1..Length( paths_in_b ) ] do
            Add( coefficients, coefficients_of_a[ i ] * coefficients_of_b[ j ] );
            Add( paths, PathInProductQuiver( QuiverOfAlgebra( T ), [ paths_in_a[ i ], paths_in_b[ j ] ] ) );
        od;
    od;
    
    return QuiverAlgebraElement( T, coefficients, paths );
end
  );

InstallMethod( EnvelopingAlgebra, "for quiver algebra",
               [ IsQuiverAlgebra ],
function( A )
  return TensorProductOfAlgebras( A^LEFT, A^RIGHT );
end );


BindGlobal( "FamilyOfQuiverAlgebraBases",
            NewFamily( "quiver algebra bases" ) );

DeclareRepresentation( "IsQuiverAlgebraBasisRep", IsComponentObjectRep and IsAttributeStoringRep,
                       [ ] );

InstallMethod( CanonicalBasis, "for path algebra",
               [ IsPathAlgebra ],
function( A )
  local Q, basis_paths, basis_elems, basis;
  Q := QuiverOfAlgebra( A );
  if not IsAcyclicQuiver( Q ) then
    Error( "algebra is infinite-dimensional" );
  fi;
  basis_paths := PathList( Q );
  basis_elems := List( basis_paths, p -> PathAsAlgebraElement( A, p ) );
  basis := rec();
  ObjectifyWithAttributes( basis,
                           NewType( FamilyOfQuiverAlgebraBases,
                                    IsQuiverAlgebraBasis and IsQuiverAlgebraBasisRep ),
                           BasisPaths, basis_paths,
                           BasisVectors, basis_elems,
                           UnderlyingLeftModule, A,
                           IsCanonicalBasis, true );
  return basis;
end );

InstallMethod( CanonicalBasis, "for quotient of path algebra",
               [ IsQuotientOfPathAlgebra ],
function( A )
  local Q, is_nonreducible, iter, basis_paths, basis_elems, basis;
  Q := QuiverOfAlgebra( A );
  is_nonreducible :=
    p ->
    ( Representative( PathAsAlgebraElement( A, p ) )
      = PathAsAlgebraElement( PathAlgebra( A ), p ) );
  iter := FilteredPathIterator( Q, is_nonreducible );
  basis_paths := [];
  while not IsDoneIterator( iter ) do
    Add( basis_paths, NextIterator( iter ) );
  od;
  basis_elems := List( basis_paths, p -> PathAsAlgebraElement( A, p ) );
  basis := rec();
  ObjectifyWithAttributes( basis,
                           NewType( FamilyOfQuiverAlgebraBases,
                                    IsQuiverAlgebraBasis and IsQuiverAlgebraBasisRep ),
                           BasisPaths, basis_paths,
                           BasisVectors, basis_elems,
                           UnderlyingLeftModule, A,
                           IsCanonicalBasis, true );
  return basis;
end );

InstallMethod( Basis, "for quiver algebra",
               [ IsQuiverAlgebra ], CanonicalBasis );

InstallMethod( BasisPathsBetweenVertices, [ IsQuiverAlgebra, IsQuiverVertex, IsQuiverVertex ],
function( A, v1, v2 )
  return Filtered( BasisPaths( CanonicalBasis( A ) ),
                   p -> Source( p ) = v1 and Target( p ) = v2 );
end );

InstallMethod( Dimension, "for quiver algebra", 
        [ IsQuiverAlgebra ], A -> Length( Basis( A ) ) ); 

InstallMethod( Coefficients,
               "for quiver algebra basis and quiver algebra element",
               [ IsQuiverAlgebraBasis, IsQuiverAlgebraElement ],
function( B, e )
  return CoefficientsOfPathsSorted( BasisPaths( B ), e );
end );

InstallMethod( CoefficientsOfPathsSorted, "for list and quiver algebra element",
               [ IsList, IsQuiverAlgebraElement ],
function( paths, e )
  local Ps, Cs, zero, coeffs, j, i;
  Ps := Paths( e );
  Cs := Coefficients( e );
  zero := Zero( LeftActingDomain( AlgebraOfElement( e ) ) );
  #coeffs := List( paths, p -> zero );
  coeffs := [];
  j := Length( Ps );
  for i in [ 1 .. Length( paths ) ] do
    if j = 0 then
      while i <= Length( paths ) do
        coeffs[ i ] := zero;
        i := i + 1;
      od;
      break;
    fi;
    if paths[ i ] = Ps[ j ] then
      coeffs[ i ] := Cs[ j ];
      j := j - 1;
    else
      coeffs[ i ] := zero;
    fi;
  od;
  return coeffs;
end );

InstallMethod( CoefficientsOfPaths, "for list and quiver algebra element",
               [ IsList, IsQuiverAlgebraElement ],
function( paths, e )
  local sorted, i, Ps, Cs, zero, coefficient_of_path;
  sorted := true;
  for i in [ 1 .. ( Length( paths ) - 1 ) ] do
    if not ( paths[ i ] < paths[ i + 1 ] ) then
      sorted := false;
      break;
    fi;
  od;
  if sorted then
    return CoefficientsOfPathsSorted( paths, e );
  fi;
  Ps := Paths( e );
  Cs := Coefficients( e );
  zero := Zero( LeftActingDomain( AlgebraOfElement( e ) ) );
  coefficient_of_path := function( p )
    local pos;
    pos := Position( Ps, p );
    if pos = fail then
      return zero;
    else
      return Cs[ pos ];
    fi;
  end;
  return List( paths, coefficient_of_path );
end );

BindGlobal( "FamilyOfQuiverAlgebraHomomorphisms", NewFamily( "quiver algebra homomorphisms" ) );

InstallMethod( QuiverAlgebraHomomorphism, "for two quiver algebras and two lists of images",
        [ IsPathAlgebra, IsQuiverAlgebra, IsHomogeneousList, IsHomogeneousList ], 
        function( A, B, verteximages, arrowimages )
    local   QA,  verticesA,  arrowsA,  QB,  verticesB,  arrowsB,  
            arguments,  images,  n,  i,  j,  a,  position,  leftend,  
            rightend,  type,  map;
     
    QA := QuiverOfAlgebra( A ); 
    verticesA := Vertices( QA );
    arrowsA := Arrows( QA );
    QB := QuiverOfAlgebra( B );
    verticesB := Vertices( QB );
    arrowsB := Arrows( QB );
    if Length( verteximages ) <> Length( verticesA ) then
        Error( "The enter number of images of the vertices doesn't match the number of vertices in the domain," );
    fi;
    if Length( arrowimages ) <> Length( arrowsA ) then
        Error( "The enter number of images of the arrows doesn't match the number of arrows in the domain," );
    fi;       
    if not ForAll( verteximages, v -> v in B ) then
        Error( "The images of the vertices are not in the range,");
    fi;
    if not ForAll( arrowimages, a -> a in B ) then
        Error( "The images of the arrows are not in the range,");
    fi;
    if Sum( verteximages ) <> One( B ) then
        Error( "The identity is not mapped to the identity," );
    fi;
    arguments := Concatenation( verticesA, arrowsA );
    images := Concatenation( verteximages, arrowimages );
    n := Length( arguments );
    for i in [ 1..n ] do
        for j in [ 1..n ] do
            if arguments[ i ] * arguments[ j ] = fail then
                if images[ i ] * images[ j ] <> Zero( B ) then
                    Error( "The mapping is not well-defined. The pair (",i,", ",j,") is the culprit." );
                fi;
            fi;
        od;
    od;
    if not ForAll( verteximages, v -> v = v^2 ) then
        Error( "The vertices are not mapped to idempotents." );
    fi;
    for a in arrowsA do
        position := ArrowIndex( a );
        leftend := LeftEnd( a ); 
        rightend := RightEnd( a );
        if not ( verteximages[ VertexIndex( leftend ) ] * arrowimages[ position ] = arrowimages[ position ] ) and
                 ( arrowimages[ position ] * verteximages[ VertexIndex( rightend ) ] = arrowimages[ position ] ) then 
            Error( "The mapping is not well-defined. The arrow ",a," is the culprit." );
        fi;
    od;
    type := NewType( FamilyOfQuiverAlgebraHomomorphisms,
                     IsQuiverAlgebraHomomorphism and IsComponentObjectRep and IsAttributeStoringRep );
    map := rec( ); 
    ObjectifyWithAttributes( map, type,
                             VertexImages, verteximages, ArrowImages, arrowimages, Source, A, Range, B );
    return map; 
end );

InstallMethod( QuiverAlgebraHomomorphism, "for two quiver algebras and two lists of images",
        [ IsQuotientOfPathAlgebra, IsQuiverAlgebra, IsHomogeneousList, IsHomogeneousList ], 
        function( A, B, verteximages, arrowimages )
    local   kQ,  f,  relations,  type,  map;
    
    kQ := PathAlgebra( A );
    f := QuiverAlgebraHomomorphism( kQ, B, verteximages, arrowimages ); 
    relations := RelationsOfAlgebra( A ); 
    if not ForAll( relations, r -> ImageElm( f, r ) = Zero( B ) ) then
        Error( "The mapping is not a well-defined homomorphism on the domain." );
    fi;
    type := NewType( FamilyOfQuiverAlgebraHomomorphisms,
                     IsQuiverAlgebraHomomorphism and IsComponentObjectRep and IsAttributeStoringRep );
    map := rec( ); 
    ObjectifyWithAttributes( map, type,
                             VertexImages, verteximages, ArrowImages, arrowimages, Source, A, Range, B );
    return map; 
end );

InstallMethod( QuiverAlgebraHomomorphism, "for two quiver algebras and a list of images",
        [ IsQuiverAlgebra, IsQuiverAlgebra, IsHomogeneousList ], 
        function( A, B, genimages )
    local   num_vertices,  num_arrows,  verteximages,  arrowimages;
    
    num_vertices := NumberOfVertices( QuiverOfAlgebra( A ) );
    num_arrows := NumberOfArrows( QuiverOfAlgebra( A ) );
    if num_vertices + num_arrows <> Length( genimages ) then
        Error( "Number of images doesn't match number of generators.\n" );
    fi;
    verteximages := genimages{ [ 1..num_vertices ] };
    arrowimages := genimages{ [ num_vertices + 1..Length( genimages ) ] };
    
    return QuiverAlgebraHomomorphism( A, B, verteximages, arrowimages ); 
end );

InstallMethod( QuiverAlgebraHomomorphism, "for two quiver algebras and a function",
               [ IsQuiverAlgebra, IsQuiverAlgebra, IsFunction ],
function( A, B, f )
  local vertex_images, arrow_images;
  vertex_images := List( Vertices( A ), f );
  arrow_images := List( Arrows( A ), f );
  return QuiverAlgebraHomomorphism( A, B, vertex_images, arrow_images );
end );


InstallMethod( ImageElm, "for a homomorphism of quiver algebras",
        [ IsQuiverAlgebraHomomorphism, IsQuiverAlgebraElement ],
        function( f, x )
    local   coefficients,  paths,  imageofpaths,  p,  temp,  q;
    
    if not x in Source( f ) then
        Error( "The argument is not in the domain." );
    fi;
    coefficients := Coefficients( x );
    paths := Paths( x ); 
    imageofpaths := [];
    for p in paths do
        if IsQuiverVertex( p ) then
            Add( imageofpaths, VertexImages( f )[ VertexIndex( p ) ] );
        else
            temp := One( Range( f ) );
            for q in ArrowListLR( p ) do
                temp := temp * ArrowImages( f )[ ArrowIndex( q ) ];
            od;
            Add( imageofpaths, temp );
        fi;
    od;
    
    return Sum( List( [ 1..Length( paths ) ], i -> coefficients[ i ] * imageofpaths[ i ] ) );
end );

InstallMethod( TensorAlgebraInclusions, "for a tensor product of algebras",
        [ IsTensorProductOfAlgebras ], 
        function ( T )
    local   decomp,  A,  B,  images1,  images2;
    
    decomp := TensorProductFactors( T );
    A := decomp[ 1 ];
    B := decomp[ 2 ];
    images1 := List( PrimitivePaths( A ), x -> ElementaryTensor( x, One( B ), T ) );
    images2 := List( PrimitivePaths( B ), x -> ElementaryTensor( One( A ), x, T ) );
    
    return [ QuiverAlgebraHomomorphism( A, T, images1), QuiverAlgebraHomomorphism( B, T, images2 ) ];
end );

InstallMethod( TensorAlgebraRightIdentification, "for a tensor product of algebras",
        [ IsTensorProductOfAlgebras ], 
        function ( T )
    local   decomp,  A,  B,  images,  f;
    
    decomp := TensorProductFactors( T );
    A := decomp[ 1 ];
    B := decomp[ 2 ];
    if Dimension( B ) <> 1 then
        Error( "The algebra on the right of the tensor product is not one dimensional.\n" );
    fi;          
    images := List( PrimitivePaths( QuiverOfAlgebra( T ) ), p -> PathAsAlgebraElement( A, ProjectPathFromProductQuiver( 1, p ) ) ); 
    f := QuiverAlgebraHomomorphism( T, A, images);
    SetIsIsomorphism( f, true );
    
    return f;
end );

InstallMethod( TensorAlgebraLeftIdentification, "for a tensor product of algebras",
        [ IsTensorProductOfAlgebras ], 
        function ( T )
    local   decomp,  A,  B,  images,  f;
    
    decomp := TensorProductFactors( T );
    A := decomp[ 1 ];
    B := decomp[ 2 ];
    if Dimension( A ) <> 1 then
        Error( "The algebra on the left of the tensor product is not one dimensional.\n" );
    fi;          
    images := List( PrimitivePaths( QuiverOfAlgebra( T ) ), p -> PathAsAlgebraElement( B, ProjectPathFromProductQuiver( 2, p ) ) ); 
    f := QuiverAlgebraHomomorphism( T, B, images);
    SetIsIsomorphism( f, true );
    
    return f;
end );

DeclareDirectionOperations( FieldAsQuiverAlgebra, FieldAsLeftQuiverAlgebra, FieldAsRightQuiverAlgebra );

InstallMethodWithDirections( FieldAsQuiverAlgebra,
        [ IsField ],
        dir -> K -> PathAlgebra( K, Quiver( dir, "point(1)" ) )
);


InstallMethod( IsFiniteDimensional, [ IsPathAlgebra ],
               kQ -> IsAcyclicQuiver( QuiverOfAlgebra( kQ ) ) );


InstallMethod( IsFiniteDimensional, [ IsQuotientOfPathAlgebra ],
function( A )
  local Q, I, is_nonreducible, path_length, paths, next_paths,
        cycle_length_limit, p, a;

  Q := QuiverOfAlgebra( A );
  I := IdealOfQuotient( A );

  is_nonreducible :=
    p ->
    ( Representative( PathAsAlgebraElement( A, p ) )
      = PathAsAlgebraElement( PathAlgebra( A ), p ) );

  path_length := 1;
  paths := Arrows( Q );
  next_paths := [];

  cycle_length_limit := Maximum( List( GeneratorsOfIdeal( I ),
                                       g -> Length( LeadingPath( g ) ) ) );

  while not IsEmpty( paths ) do
    for p in paths do
      if is_nonreducible( p ) then
        for a in OutgoingArrows( Target( p ) ) do
          Add( next_paths, ComposePaths( p, a ) );
        od;
        if Source( p ) = Target( p ) and path_length >= cycle_length_limit
           and is_nonreducible( p * p ) then
          return false;
        fi;
      fi;
    od;
    path_length := path_length + 1;
    paths := next_paths;
    next_paths := [];
  od;

  return true;
end );


InstallMethod( IsAdmissibleQuiverAlgebra, "for quiver algebra",
               [ IsQuiverAlgebra ],
               A -> IsAdmissibleIdeal( IdealOfQuotient( A ) ) );


InstallMethod( NaturalHomomorphismByIdeal, [ IsPathAlgebra, IsQuiverAlgebraTwoSidedIdeal ],
# TODO: case where the algebra is a quotient of a path algebra
function( kQ, I )
  local Q, A, images;
  Q := QuiverOfAlgebra( kQ );
  A := kQ / I;
  images := List( PrimitivePaths( Q ), p -> PathAsAlgebraElement( A, p ) );
  return QuiverAlgebraHomomorphism( kQ, A, images );
end );

InstallMethod( NaturalHomomorphismByIdeal,
               "for quiver algebra and quiver algebra ideal",
               [ IsQuiverAlgebra, IsQuiverAlgebraTwoSidedIdeal ],
function( A, I )
  local A_I;
  A_I := A / I;
  return QuiverAlgebraHomomorphism( A, A_I, PrimitivePaths( A_I ) );
end );

InstallMethod( RadicalOfAlgebra, 
"for an quiver algebra", [ IsQuiverAlgebra ], 0,
function( A );
  
  if not IsAdmissibleQuiverAlgebra( A ) then 
    TryNextMethod( ); 
  fi;
  
  return Ideal( A, Arrows( A ) );
end
  );

#######################################################################
##
#P  IsDistributiveAlgebra( <A> ) 
##  
##  This function returns true if the algebra  <A>  is finite 
##  dimensional and distributive. Otherwise it returns false.
##
InstallMethod ( IsDistributiveAlgebra, 
"for an QuotientOfPathAlgebra",
[ IsQuiverAlgebra ],
function( A )

    local   pids,  f,  AA,  idems,  localrings,  radicalseries,  
            uniserialtest,  radlocalrings,  i,  j,  module,  
            testspace,  flag,  radtestspace;
    
    if not IsFiniteDimensional( A ) then 
        Error( "the entered algebra is not finite dimensional,\n" );
    fi;
    if not IsAdmissibleQuiverAlgebra( A ) then
        TryNextMethod( );
    fi;
    #  Finding a complete set of primitive idempotents in  A.
    pids := List( Vertices( QuiverOfAlgebra( A ) ), v -> v * One( A ) );
    f := IsomorphismMatrixAlgebra( A );
    AA := Range( f );
    idems := List( pids, x -> ImageElm( f, x ) );
    #  
    #  For each primitive idempotent e, compute eAe and check if 
    #  eAe is a uniserial algebra for all e.
    localrings := List(idems, i -> Subalgebra( AA, i * BasisVectors( Basis( AA ) ) * i ) );  
    # 
    #  Check if all algebras in  localrings  are unisersial.
    #
    radicalseries := List( localrings, R -> RadicalSeriesOfAlgebra( R ) );
    uniserialtest := Flat( List( radicalseries, series -> 
                             List( [ 1..Length( series ) - 1 ], i -> Dimension( series[ i ] ) - Dimension( series[ i + 1 ] ) ) ) );
    if not ForAll( uniserialtest, x -> x = 1) then 
       return false;
    fi;
    #  Check if the eAAe-module eAAf is uniserial or 
    #  the fAAf-module eAAf is uniserial for all pair
    #  of primitive idempotents e and f. 
    radlocalrings := List( localrings, R -> Subalgebra( R, Basis( RadicalOfAlgebra( R ) ) ) );
    for i in [ 1..Length( idems ) ] do
        for j in [ 1..Length( idems ) ] do 
            if i <> j then 
                module := LeftAlgebraModule( localrings[ i ],\* ,Subspace( AA, idems[ i ] * BasisVectors( Basis( AA ) ) * idems[ j ] ) );
                # compute the radical series of this module over localrings[i] and 
                # check if all layers are one dimensional.
                testspace := BasisVectors( Basis( module ) ); 
                flag := true;
                while Length( testspace ) <> 0 and flag do
                    if Dimension( radlocalrings[ i ] ) = 0 then
                        radtestspace := [];
                    else
                        radtestspace := Filtered( Flat( List( testspace, t -> 
                                                List( BasisVectors( Basis( radlocalrings[ i ] ) ), b -> b^t) ) ), x -> x <> Zero( x ) );
                    fi;
                    if Length( radtestspace ) = 0 then
                        if Length( testspace ) <> 1 then 
                            flag := false;
                        else
                            testspace := radtestspace;
                        fi;
                    else
                        radtestspace := Subspace( module, radtestspace ); 
                        if Length( testspace ) - Dimension( radtestspace ) <> 1 then
                            flag := false;
                        else
                            testspace := BasisVectors( Basis( radtestspace ) );
                        fi;
                    fi;
                od;
                if not flag then 
                    module := RightAlgebraModule( localrings[ j ],\* , Subspace( AA, idems[ i ] * BasisVectors( Basis( AA ) ) * idems[ j ] ) );
                    # compute the radical series of this module over localrings[j] and
                    # check if all layers are one dimensional.
                    testspace := BasisVectors( Basis( module ) ); 
                    while Length( testspace ) <> 0 do
                        if Dimension( radlocalrings[ j ] ) = 0 then
                            radtestspace := [];
                        else
                            radtestspace := Filtered( Flat( List( testspace, t -> 
                                                    List( BasisVectors( Basis( radlocalrings[ j ] ) ), b -> t^b) ) ), x -> x <> Zero( x ) );
                        fi;
                        if Length( radtestspace ) = 0 then
                            if Length( testspace ) <> 1 then 
                                return false;
                            else
                                testspace := radtestspace;
                            fi;
                        else
                            radtestspace := Subspace( module, radtestspace ); 
                            if Length( testspace ) - Dimension( radtestspace ) <> 1 then
                                return false;
                            else
                                testspace := BasisVectors( Basis( radtestspace ) );
                            fi;
                        fi;
                    od;
                fi;
            fi;
        od;
    od;
    
    return true;
end 
); 

#######################################################################
##
#P  IsBasicAlgebra( <A> )
##
##  This function returns true if the entered algebra  <A>  is a (finite
##  dimensional) basic algebra and false otherwise. This method applies 
##  to algebras over finite fields. 
##
InstallMethod( IsBasicAlgebra,
"for an algebra",
[ IsAlgebra ],
function( A )

    local   K,  AA,  J,  L;
    #
    # Only finite dimensional algebras are regarded as basic.
    # 
    if not IsFiniteDimensional( A ) then
        Error( "the entered algebra is not finite dimensional,\n" );
    fi;
    K := LeftActingDomain( A );
    #
    # Here we only can deal with algebras over finite fields.
    # First we find a decomposition of the algebra modulo the
    # radical, and then we find all the primitive idempotents 
    # in each block. If each block only contains one primitive
    # idempotent, then the algebra is basic. 
    #
    if IsFinite( K ) then 
        AA := Range( IsomorphismMatrixAlgebra( A ) );
        J := RadicalOfAlgebra( AA );
        L := DirectSumDecomposition( AA/J ); 
        L := List( L, IsCommutative );
        return ForAll( L, l -> l = true );
    else
        TryNextMethod( );
    fi;
end
);

#######################################################################
##
#P  IsElementaryAlgebra( <A> )
##
##  This function returns true if the entered algebra  <A>  is a (finite
##  dimensional) elementary algebra and false otherwise. This method 
##  applies to algebras over finite fields. 
##
InstallMethod( IsElementaryAlgebra,
"for an algebra",
[ IsAlgebra ],
function( A )

    local   K,  AA,  J,  D,  L;
    
    K := LeftActingDomain( A );
    #
    # Here we only can deal with algebras over finite fields.
    # First we find a decomposition of the algebra modulo the
    # radical, and then we find all the primitive idempotents 
    # in each block. If each block only contains one primitive
    # idempotent, then the algebra is basic. 
    #
    AA := Range( IsomorphismMatrixAlgebra( A ) );
    if IsFinite( K ) and IsBasicAlgebra( AA ) then
        J := RadicalOfAlgebra( AA );
        D := DirectSumDecomposition( AA / J );
                # Since  A  is basic, we know that  D  only consists
                # of simple blocks of division algebras. Hence  A  is 
                # elementary if all the blocks have the same dimension 
                # over  K, as then they are all the same finite field.
                #
        L := List( D, Dimension ); 
        if ForAll(L, x -> x = L[ 1 ] ) then
            return true;
        else
            return false;
        fi;
    else
        TryNextMethod( );
    fi;
end
  );

InstallOtherMethod( CartanMatrix, 
"for a finite dimensional IsQuiverAlgebra",
[ IsQuiverAlgebra ],
function( A ) 

    local   P,  C,  i;
   
    if not IsFiniteDimensional( A ) then 
	Error( "Algebra is not finite dimensional.\n" );
    fi;
    P := IndecProjRepresentations( A );
    C := [];
    for i in [ 1..Length( P ) ] do
        Add( C, DimensionVector( P[ i ] ) );
    od;

    return C;
end
  ); # CartanMatrix

InstallMethod( CoxeterMatrix, 
"for a finite dimensional IsQuiverAlgebra",
[ IsQuiverAlgebra ],
function( A ) 

    local P, C, i;

    C := CartanMatrix( A );
    if C = fail then
	Error( "Unable to determine the Cartan matrix.\n" );
    fi;
    if DeterminantMat( C ) <> 0 then 
        return ( -1 ) * C^( -1 ) * TransposedMat( C );
    else
        Error( "The Cartan matrix is not invertible.\n" );
	return fail;
    fi;
end
  ); # CoxeterMatrix

InstallMethod( CoxeterPolynomial, 
"for a finite dimensional IsQuiverAlgebra",
[ IsQuiverAlgebra ],
function( A ) 

    local P, C, i;

    C := CoxeterMatrix( A );
    if C <> fail then 
        return CharacteristicPolynomial( C );
    else
        return fail;
    fi;
end
  ); # CoxeterPolynomial

#######################################################################
##
#O  LoewyLength ( <A> )
##
##  This function returns the Loewy length of the algebra  A, for a 
##  finite dimensional (quotient of a) path algebra (by an admissible
##  ideal).
##
InstallOtherMethod( LoewyLength, 
"for (a quotient of) a path algebra",
[ IsQuiverAlgebra ],
function( A ) 

    local fam, N;
    
    if not IsFiniteDimensional( A ) then
        Error( "the entered algebra is not finite dimensional,\n" );
    fi;
    if not IsAdmissibleQuiverAlgebra( A ) then 
        TryNextMethod();
    fi;
    N := IndecProjRepresentations( A );
    N := List( N, x -> LoewyLength( x ) );
    
    return Maximum( N );
end
  ); 

InstallMethod( Centre,
"for a path algebra",
[ IsQuotientOfPathAlgebra ],
function( A ) 

  local B, Q, K, num_vert, vertices, arrows, cycle_list, b, 
        commutators, center, cycles, c, a, matrix, j, i, solutions, x;
   
   if not IsFiniteDimensional( A ) then
       Error( "The entered algebra is not finite dimensional.\n");
   fi;
   B := CanonicalBasis( A );
   Q := QuiverOfAlgebra( A ); 
   K := LeftActingDomain( A );
   num_vert := NumberOfVertices( Q );
   vertices := Vertices( Q );
   arrows   := Arrows( Q );
  
   cycle_list := [];
   for b in B do
      if Source( LeadingPath( Representative( b ) ) ) = Target( LeadingPath( Representative( b ) ) ) then 
         Add( cycle_list, b );
      fi;
   od;
   commutators := [ ];
   center := [ ];
   cycles := Length( cycle_list );
   for c in cycle_list do
      for a in arrows do
         Add( commutators, c * a - a * c );
      od;
   od;

   matrix := NullMat( cycles, Length( B ) * NumberOfArrows( Q ), K );

   for j in [ 1..cycles ] do
      for i in [ 1..NumberOfArrows( Q ) ] do
         matrix[ j ]{ [ Length( B ) * ( i - 1 ) + 1..Length( B ) * i ] } := 
           Coefficients( B, commutators[ i + ( j - 1 ) * NumberOfArrows( Q ) ] ); 
      od;
   od;

   solutions := NullspaceMat( matrix );

   center := [ ];
   for i in [ 1..Length( solutions ) ] do
      x := Zero( A );
      for j in [ 1..cycles ] do 
         if solutions[ i ][ j ] <> Zero( K ) then 
            x := x + solutions[ i ][ j ] * cycle_list[ j ];
         fi;
      od;
      center[ i ] := x;
   od;
   
   return Subalgebra( A, center, "basis" );
end
  );

InstallMethod( Centre,
"for a path algebra",
[ IsPathAlgebra ],
function( A ) 

    local Q, vertexlabels, arrowlabels, vertices, arrows, components, basis, i,
          compverticespositions, temp, comparrows, cycle, lastone, v, n, tempx, j, 
          k, index;
    #
    # If  <A>  is an indecomposable path algebra, then the center of  <A>  is the 
    # linear span of the identity if the defining quiver is not an oriented cycle
    # (an A_n-tilde quiver).  If the defining quiver of  <A>  is an oriented cycle, 
    # then the center is generated by the identity and powers of the sum of all the 
    # different shortest cycles in the defining quiver.  
    #
    Q := QuiverOfAlgebra( A ); 
    components := ConnectedComponents( Q );

    vertexlabels := List(Vertices( Q ), String);
    arrowlabels := List(Arrows( Q ), String);
    vertices := Vertices( Q ) * One( A ); 
    arrows := Arrows( Q ); 

    basis := [ ];
    for i in [ 1..Length( components ) ] do
        #
        # Constructing the linear span of the sum of the vertices, ie. the 
        # identity for this block of the path algebra.
        #
        compverticespositions := List( Vertices( components[ i ] ), String );
        compverticespositions := List( compverticespositions, v -> Position( vertexlabels, v ) ); 
        temp := Zero( A );
        for n in compverticespositions do
            temp := temp + vertices[ n ];
        od;
        Add( basis, temp );
        # 
        # Next, if the component is given by an oriented cycle, construct the sum of all the 
        # different cycles in the component.
        #
        if ForAll( Vertices( components[ i ] ), v -> Length( IncomingArrows( v ) ) = 1 ) and 
           ForAll( Vertices( components[ i ] ), v -> Length(OutgoingArrows( v ) ) = 1 ) then
            # define linear span of the sum of cycles in the quiver
            comparrows := List( Arrows( components[ i ] ), String );
            comparrows := List( comparrows, a -> arrows[ Position( arrowlabels, a ) ] );
            if Length( comparrows ) > 0 then
                cycle := [ ];
                lastone := comparrows[ 1 ];
                Add( cycle, lastone );
                for j in [ 2..Length( comparrows ) ] do
                    v := Target( lastone );
                    lastone := First( comparrows, a -> Source( a ) = v );
                    Add( cycle, lastone );
                od;
                temp := Zero( A );
                n := Length( comparrows );
                for j in [ 0..Length( comparrows ) - 1 ] do
                    tempx := One( A );
                    for k in [ 0..Length( comparrows ) - 1 ] do
                        tempx := tempx * cycle[ ( ( j + k ) mod n ) + 1 ];
                    od;
                    temp := temp + tempx;
                od;
                Add( basis, temp ); 
            fi;
        fi;
    od;
    
    return Subalgebra( A, basis, "basis" );
end
  );

#######################################################################
##
#O  FindMultiplicativeIdentity( <A> )
##
##  This function finds the multiplicative identity of a finite 
##  dimensional algebra, if it exists. 
##
InstallMethod ( FindMultiplicativeIdentity, 
"for a finite dimensional algebra",
[ IsAlgebra ],
function( A )
    
    local   B,  constantterm,  matrix,  i,  b1,  partialmatrix,  b2,  
            totalmatrix,  totalconstantterm,  identity;
    
    if not IsFiniteDimensional( A ) then
        Error( "The entered algebra is not finite dimensional.\n" );
    fi;
    B := Basis( A ); 
    
    constantterm := [ ];
    matrix := [ ];
    i := 1;
    for b1 in B do
        partialmatrix := [ ];
        for b2 in B do 
            Add( partialmatrix, Coefficients( B, b2 * b1 ) );
        od;
        Add( constantterm, Coefficients( B, b1 ) );
        Add( matrix, [ 1, i, partialmatrix ] );
        i := i + 1;
    od;
    totalmatrix := BlockMatrix( matrix, 1, i - 1 );
    totalconstantterm := Flat( constantterm );
    identity := SolutionMat( totalmatrix, totalconstantterm );
    
    if identity = fail then
        return fail;
    else
        return LinearCombination( B, identity );
    fi;
end
  );