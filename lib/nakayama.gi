InstallMethod( IsAdmissibleSequence, [ IsDenseList ],
function( seq )
  local i;
  if not ForAll( seq, IsPosInt ) then
    Error( "sequence to be tested for admissibility should be a ",
           "list of positive integers" );
  fi;
  for i in [ 1 .. ( Length( seq ) - 1 ) ] do
    if seq[ i + 1 ] < seq[ i ] - 1 or seq[ i ] = 1 then
      return false;
    fi;
  od;
  if seq[ 1 ] < seq[ Length( seq ) ] - 1 then
    return false;
  fi;
  return true;
end );

DeclareDirectionOperations( NakayamaAlgebra, LeftNakayamaAlgebra, RightNakayamaAlgebra );

InstallMethodWithDirections( NakayamaAlgebra,
                             [ IsField, IsDenseList ],
dir -> function( K, seq )
  local n, Q, arrows, kQ, rels, i, rel_path, A, index;

  n := Length( seq );

  if not IsAdmissibleSequence( seq ) then
    Error( "not an admissible sequence: ", seq );
  fi;

  if seq[ n ] = 1 then
    # A_n case
    Q := DynkinQuiver( dir, "A", n );
    arrows := Arrows( Q );
    kQ := PathAlgebra( K, Q );
    rels := [];
    for i in [ 1 .. ( n - 1 ) ] do
      if seq[ i ] - 1 <> seq[ i + 1 ] then
        rel_path := PathFromArrowList( arrows{ [ i .. ( i + seq[ i ] - 1 ) ] } );
        Add( rels, PathAsAlgebraElement( kQ, rel_path ) );
      fi;
    od;
    A := kQ / rels;
  else
    # A~_n case
    Q := DynkinQuiver( dir, "A~", n - 1 );
    arrows := Arrows( Q );
    kQ := PathAlgebra( K, Q );
    rels := [];
    index := i -> ( i - 1 ) mod n + 1; # for cyclic lookup of vertices/arrows
    for i in [ 1 .. n ] do
      if seq[ i ] - 1 <> seq[ index( i + 1 ) ] then
        rel_path := PathFromArrowList( arrows{ List( [ i .. ( i + seq[ i ] - 1 ) ], index ) } );
        Add( rels, PathAsAlgebraElement( kQ, rel_path ) );
      fi;
    od;
    A := kQ / rels;
  fi;

  SetIsNakayamaAlgebra( A, true );
  SetAdmissibleSequence( A, seq );
  return A;
  
end );

InstallMethod( IsNakayamaAlgebra, [ IsQuiverAlgebra ],
function( A )
  local Q;
  Q := QuiverOfAlgebra( A );
  # TODO: need to check that A = kQ/I where I is an admissible ideal
  return Length( SourceVertices( Q ) ) <= 1 and
         Length( SinkVertices( Q ) ) <= 1 and
         ForAll( Vertices( Q ), v -> Indegree( v ) <= 1 and Outdegree( v ) <= 1 );
end );

#######################################################################
##
#O  KroneckerAlgebra( <K>, <n> )
##
##  Given a field  K and a positive integer  n, this function constructs  
##  the Kronecker algebra with  n  arrows. 
##
DeclareDirectionOperations( KroneckerAlgebra, LeftKroneckerAlgebra, RightKroneckerAlgebra );

InstallMethodWithDirections( KroneckerAlgebra,
                             [ IsField, IS_INT ],
dir -> function( K, n )

  local vertices, arrows, i, sources, targets, Q, KQ;

    if n < 2 then
        Error( "The number of arrows in the Kronecker quiver must be greater or equal to 2.\n" );
    fi;
    vertices := [ 1, 2 ];
    arrows := [ ];
    for i in [ 1..n ] do
        Add( arrows, Concatenation( "a", String( i ) ) );
    od;
    sources := [ ];
    for i in [ 1..n ] do
        Add( sources, 1 );
    od;
    targets := sources + sources;
    Q := Quiver( dir, Concatenation( "Kronecker(", String( n ), ")" ) , vertices, arrows, sources, targets );
    KQ := PathAlgebra( K, Q );
    SetFilterObj( KQ, IsKroneckerAlgebra and IsPathAlgebra );
    
    return KQ;
end
);

#######################################################################
##
#O  CanonicalAlgebra( <K>, <weights>, <relcoeff> )
##
##  Given a field  K, a sequence of  weights  and a sequence of 
##  coefficients for the relations, this function constructs the 
##  canonical algebra with this data. If the number of weights is
##  two, then the number of coefficients for the relations must be
##  zero, that is, represented by an empty list, [].
##
DeclareDirectionOperations( CanonicalAlgebra, LeftCanonicalAlgebra, RightCanonicalAlgebra );

InstallMethodWithDirections( CanonicalAlgebra,
                             [ IsField, IsList, IsList ],
dir -> function( K, weights, relcoeff )

  local n, vertices, i, j, sources, targets, arrows, Q, KQ, num_vert, 
        generators, arms, start, temp, relations, A;
    #
    # Checking the input.
    #
    if Length( weights ) <> Length( relcoeff ) + 2 then
        Error( "Number of arms in the quiver don't match the number of coeffiecients for the relations in the quiver.\n" );
    fi;
    if not ForAll( weights, x -> x >= 2 ) then
        Error( "Not all weights are greater or equal to 2.\n" );
    fi;
    if not ForAll( relcoeff, x -> x in K ) then
        Error( "Not all coefficients for the relations are in the entered field.\n" );
    fi;
    if not ForAll( relcoeff, x -> x <> Zero( K ) ) then
        Error( "Not all coefficients for the relations are non-zero.\n");
    fi;
    #
    # When input is good, start constructing the algebra. 
    # First construct the vertices.
    #
    n := Length( weights );
    vertices := [ ];
    Add( vertices, "v" );
    for i in [ 1..n ] do
        for j in [ 2..weights[ i ] ] do
            Add( vertices, Concatenation( "v", String( i ), String( j ) ) );
        od;
    od;
    Add( vertices, "w" );
    #
    # Next construct the arrows.
    #
    sources := [ ];
    targets := [ ];
    arrows := [ ];
    for i in [ 1..n ] do
        Add( sources, "v" );
        Add( targets, Concatenation( "v", String( i ), String( 2 ) ) );
        Add( arrows, Concatenation( "a", String( i ) ) );
        for j in [ 2..weights[ i ] - 1 ] do
            Add( sources, Concatenation("v", String( i ), String( j ) ) );
            Add( targets, Concatenation( "v", String( i ), String( j + 1 ) ) );
            Add( arrows, Concatenation( "a", String( i ), String( j ) ) );
        od;
        Add( sources, Concatenation( "v", String( i ), String( weights[ i ] ) ) );
        Add( targets, "w" );
        Add( arrows, Concatenation( "b", String( i ) ) );
    od;
    sources := List( sources, s -> Position( vertices, s ) );
    targets := List( targets, t -> Position( vertices, t ) );
    # 
    # Constructing the quiver and path algebra.
    #
    Q := Quiver( dir, "Canonical", vertices, arrows, sources, targets );
    KQ := PathAlgebra( K, Q );
    #
    # If n = 2, it is a path algebra, otherwise construct relations 
    # and quotient of the path algbra just constructed.
    #
    if n = 2 then 
        SetFilterObj(KQ, IsCanonicalAlgebra and IsPathAlgebra );

        return KQ;
    fi;
    # 
    # First finding all the arrows as elements in the path algebra.
    #
    num_vert := NumberOfVertices( Q );
    generators := GeneratorsOfAlgebra(KQ){ [ num_vert + 1..num_vert + NumberOfArrows( Q ) ] };
    #
    # Computing the products of all arrows in an arm of the quiver as 
    # elements in the path algebra. 
    # 
    arms := [ ];
    start := 1;
    for i in [ 1..n ] do
        temp := One( KQ );
        for j in [ start..start + weights[ i ] - 1 ] do
            temp := temp * generators[ j ];
        od;
        start := start + weights[ i ];
        Add( arms, temp );
    od;
    relations := [];
    # 
    # Constructing the relations and the quotient.
    #
    for i in [ 3..n ] do
        Add( relations, arms[ i ] - arms[ 2 ] + relcoeff[ i - 2 ] * arms[ 1 ] );
    od;
    A := KQ/relations;
    SetFilterObj( A, IsCanonicalAlgebra and IsAdmissibleQuiverAlgebra );
    
    return A;
end
);

#######################################################################
##
#O  CanonicalAlgebra( <dir>, <K>, <weights> )
##
##  CanonicalAlgebra with only two arguments, the field and the 
##  two weights.
##
InstallOtherMethod ( CanonicalAlgebra,
"for a field and a list of lengths of two arms", 
[ IsDirection, IsField, IsList ],
function( dir, K, weights );

    if Length( weights ) <> 2 then 
        Error( "The list of weights is different from two, need to enter coefficients for the relations then.\n");
    fi;
    
    return CanonicalAlgebra( dir, K, weights, [ ] );
end
);

#######################################################################
##
#O  AssociatedMonomialAlgebra( <M> )
##
##  Takes as an argument a quiver algebra  <A>  and returns the 
##  associated monomial algebra by using the Groebner basis  <A>  is 
##  endoved with and in particular the ordering of the vertices and the
##  arrows.  Taking another ordering of the vertices and the arrows
##  might change the associated algebra. 
##  
InstallMethod( AssociatedMonomialAlgebra, 
"for a finite dimensional quiver algebra",
[ IsQuiverAlgebra ], 
function( A )

    local   I,  gb,  relations,  kQ;

    if IsPathAlgebra( A ) then 
        return A;
    fi;
    I := IdealOfQuotient( A );
    if IsMonomialIdeal( I ) then
        return A;
    fi;
    
    gb := GroebnerBasis( I );
    relations := List( gb, r -> LeadingPath( r ) );
    kQ := PathAlgebra( A );
    relations := List( relations, r -> r * One( kQ ) );
    
    return kQ / relations;
end
  );

InstallMethod( TruncatedPathAlgebra, 
"for a path algebra",
[ IsField, IsQuiver, IS_INT ],
function( K, Q, n ) 

  local KQ, relations;

  KQ := PathAlgebra( K, Q );
  relations := NthPowerOfArrowIdeal( KQ, n );

  return KQ/relations;
end
  );

########################################################################
##
#P  IsSchurianAlgebra( <A> ) 
##
##  It tests if an algebra <A> is a Schurian  algebra.
##  By definition it means that:
##  for all x,y\in Q_0 dim A(x,y)<=1.
##
InstallMethod( IsSchurianAlgebra,
"for a IsQuiverAlgebra",
[ IsQuiverAlgebra ],
function( A )
    
    local   test;
    
    if IsFiniteDimensional( A ) then
        test := Flat( List( IndecProjRepresentations( A ), x -> DimensionVector( x ) ) );
        return ForAll( test, x -> ( x <= 1 ) );
    else
        Error( "the entered algebra is not finite dimensional.\n" );
    fi;    
end 
); 

#################################################################
##
#P  IsSemicommutativeAlgebra( <A> ) 
##
##  It tests if a path algebra <A> is a semicommutative  algebra. 
##  Note that for a path algebra it is an empty condition
##  (when <A> is schurian + acyclic, cf. description for quotients below).
##		
InstallMethod( IsSemicommutativeAlgebra,
"for a path algebra",
[ IsPathAlgebra ],
function ( A )
    
    local Q; 
    
    if not IsSchurianAlgebra( A ) then
        return false;
    fi;
    Q := QuiverOfAlgebra( A );
    
    return IsAcyclicQuiver( Q );  
end
  ); # IsSemicommutativeAlgebra for IsPathAlgebra

########################################################################
##
#P  IsSemicommutativeAlgebra( <A> ) 
##  <A> = a quotient of a path algebra
##
##  It tests if a quotient of a path algebra <A>=kQ/I is a semicommutative  algebra.
##  By definition it means that:
##  1. A is schurian (i.e. for all x,y\in Q_0 dim A(x,y)<=1).
##  2. Quiver Q of A is acyclic.
##  3. For all pairs of vertices (x,y) the following condition is satisfied:
##     for every two paths P,P' from x to y:
##     P\in I <=> P'\in I
##
InstallMethod( IsSemicommutativeAlgebra,
"for quotients of path algebras",
[ IsQuotientOfPathAlgebra ],
function ( A )
    
  local kQ, Q, I, vertices, paths, path, noofverts, noofpaths, eA, v, 
        p, pp, vt, inI, notinI;
    
    kQ := PathAlgebra( A );
    Q := QuiverOfAlgebra( kQ );
    if ( not IsAcyclicQuiver( Q ) ) 
      or ( not IsSchurianAlgebra( A ) )
      then
        return false;
    fi;
    
    I := IdealOfQuotient( A ); 
    
    vertices := Vertices( Q );
    paths := [ ];
    for path in Q do
      if Length( path ) > 0 then
        Add( paths, path );
      fi;
    od;
    noofverts := Length( vertices );
    noofpaths := Length( paths );
    eA := [ ];
    for v in [ 1..noofverts ] do
      eA[ v ] := []; # here will be all paths starting from v
      for p in [ 1..noofpaths ] do
        pp := vertices[ v ] * paths[ p ];
        if pp <> Zero( Q ) then
          Add( eA[ v ], pp );
        fi;  
      od;
    od;
    
    for v in [ 1..noofverts ] do
      for vt in [ 1..noofverts ] do
        # checking all paths from v to vt if they belong to I
        inI := 0;
        notinI := 0;
        for pp in eA[ v ] do # now pp is a path starting from v
          pp := pp * vertices[ vt ];
          if pp <> Zero( Q ) then
            if PathAsAlgebraElement( kQ, pp ) in I then
              inI := inI + 1;
            else
              notinI := notinI + 1;
            fi;
            if ( inI > 0 ) and ( notinI > 0 ) then
              return false;
            fi;
          fi;
        od;
      od;
    od;
    
    return true;
end
    );

#################################################################
##
#P  IsSpecialBiserialAlgebra( <A> ) 
##  <A> = a path algebra
##
##  It tests if a path algebra <A> is an algebra of  a quiver Q which is
##  (IsSpecialBiserialQuiver and IsAcyclicQuiver) and the ideal 0 satisfies
##  the "special biserial" conditions (cf. comment below).
##  
##  NOTE: e.g. path algebra of one loop IS NOT special biserial, but
##        one loop IS special biserial quiver.
##		
InstallMethod( IsSpecialBiserialAlgebra,
"for path algebras",
[ IsPathAlgebra ],
function ( A )
    
    local Q, alpha; 
    
    Q := QuiverOfAlgebra( A );
    if not IsSpecialBiserialQuiver( Q ) then
        return false;
    fi;
    
    for alpha in Arrows( Q ) do
        if Outdegree( Target( alpha ) ) > 1 then
            return false;
        fi;          
        if Indegree( Source( alpha ) ) > 1 then  
            return false;
        fi;
    od;
    
    return IsAcyclicQuiver( Q );  # <=> 0 is an admissible ideal
end
); # IsSpecialBiserialAlgebra for IsPathAlgebra

########################################################################
##
#P  IsSpecialBiserialAlgebra( <A> ) 
##  <A> = a quotient of a path algebra
##
##  It tests if an original path algebra is an algebra of a quiver Q which is
##  IsSpecialBiserialQuiver, I is an admissible ideal and I satisfies 
##  the "special biserial" conditions, i.e.:
##  for any arrow a there exists at most one arrow b such that ab\notin I
##  and there exists at most one arrow c such that ca\notin I.
##
  
InstallMethod( IsSpecialBiserialAlgebra,
"for quotients of path algebras",
[ IsQuotientOfPathAlgebra ],
function ( A )
    
    local   kQ,  Q,  I,  alpha,  not_in_ideal,  beta;
    
    kQ := PathAlgebra( A );
    Q := QuiverOfAlgebra( kQ );
    if not IsSpecialBiserialQuiver( Q ) then
        return false;
    fi;
    I := IdealOfQuotient( A ); 
    if not IsAdmissibleIdeal( I ) then
        return false;
    fi;
    for alpha in Arrows( Q ) do
        not_in_ideal := 0;
        for beta in OutgoingArrows( Target( alpha ) ) do
            if not PathAsAlgebraElement( kQ, alpha * beta ) in I then
                not_in_ideal := not_in_ideal + 1;
            fi;
        od;
        if not_in_ideal > 1 then
            return false;
        fi;          
        not_in_ideal := 0;
        for beta in IncomingArrows( Source( alpha ) ) do
            if not PathAsAlgebraElement( kQ, beta * alpha ) in I then
                not_in_ideal := not_in_ideal + 1;
            fi;
        od;
        if not_in_ideal > 1 then
            return false;
        fi;
    od;
    
    return true;
end
  ); # IsSpecialBiserialAlgebra for IsQuotientOfPathAlgebra

#################################################################
##
#P  IsStringAlgebra( <A> )
##  <A> = a path algebra
##
##  Note: A path algebra is a string algebra <=> it is a special biserial algebra
##
InstallMethod( IsStringAlgebra,
"for quotients of path algebras",
[ IsPathAlgebra ],
function ( A )
    
    return IsSpecialBiserialAlgebra( A );
end
); # IsStringAlgebra for IsPathAlgebra                      


#################################################################
##
#P  IsStringAlgebra( <A> )
##  <A> = a quotient of a path algebra
##
##  Note: kQ/I is a string algebra <=> kQ/I is a special biserial algebra
##                                     and I is a monomial ideal.                                         
InstallMethod( IsStringAlgebra,
"for quotients of path algebras",
[ IsQuotientOfPathAlgebra ],
function ( A )
    
    local I;
    
    if not IsSpecialBiserialAlgebra( A ) then
        return false;
    fi;
    
    I := IdealOfQuotient( A );
    
    return IsMonomialIdeal( I );
end
  );

#######################################################################
##
#P  IsGentleAlgebra( <A> )
##
##  The argument of this function is a quiver algebra  <A>. The function
##  returns true is  <A>  is a gentle algebra, and false otherwise.
##  
InstallMethod( IsGentleAlgebra, 
"for a quiver algebra",
[ IsQuiverAlgebra ],
function( A )

    local   I,  minimalgenerators,  Q,  kQ,  beta,  test;
    
    if IsPathAlgebra( A ) then 
        return IsSpecialBiserialAlgebra( A );
    fi;

    I := IdealOfQuotient( A );
    #
    # Checking if  A  is a special biserial algebra, and if it is 
    # quotient of a path algebra by an admissible ideal.
    #
    if not IsSpecialBiserialAlgebra( A ) then
        return false;
    fi;
    if not IsAdmissibleQuiverAlgebra( A ) then
        return false;
    fi;
    #
    # Checking if the ideal is generated by paths.
    #
    minimalgenerators := MinimalGeneratingSetOfIdeal( I );
    if not ForAll( minimalgenerators, x -> Length( Paths( x ) ) = 1) then
        return false;
    fi;
    # 
    # Checking if the ideal is generated by paths of length  2.
    #    
    if not IsQuadraticElements( minimalgenerators ) then
        return false;
    fi;
    #
    # Checking if for any arrow beta, there is at most one arrow gamma with beta*gamma in I, and
    # if for any arrow beta, there is at most one arrow gamma with gamma*beta in I.
    #
    Q := QuiverOfAlgebra( A ); 
    kQ := PathAlgebra( A );
    for beta in Arrows( Q ) do
        test := Filtered( OutgoingArrows( Target( beta ) ), gamma -> PathAsAlgebraElement( kQ, beta * gamma) in I );
        if Length( test ) > 1 then 
            return false;
        fi;
        test := Filtered( IncomingArrows( Source( beta ) ), gamma -> PathAsAlgebraElement( kQ, gamma * beta ) in I );
        if Length( test ) > 1 then 
            return false;
        fi;
    od;
    #
    # By now, all is good, return true.
    #
    return true;
end
  );

#######################################################################
##
#P  IsRadicalSquareZeroAlgebra( <A> ) 
##  
##  This function returns true if the algebra has the property that 
##  the radical squares to zero. Otherwise it returns false.
##
InstallMethod( IsRadicalSquareZeroAlgebra,
"for an algebra",
[ IsAlgebra ],
function ( A )
    
    local radical; # radical of the algebra <A>
    
    radical := RadicalOfAlgebra( A );
    
    return Dimension( ProductSpace( radical, radical ) ) = 0;
end
  );

#######################################################################
##
#P  IsWeaklySymmetricAlgebra( <A> )
##
##  This function determines if the algebra  A  is a weakly symmetric 
##  algebra, if it is a (quotient of a) path algebra. 
##
InstallMethod( IsWeaklySymmetricAlgebra, 
"for a quotient of a path algebra",
[ IsQuiverAlgebra ],
function( A ) 

   local P;
   
   if IsSelfinjectiveAlgebra( A ) then
       P := IndecProjRepresentations( A );
       return ForAll( P, p -> DimensionVector( SocleOfRepresentation( p ) ) = DimensionVector( TopOfRepresentation( p ) ) );
   else   
       return false; 
   fi;
end
    );

