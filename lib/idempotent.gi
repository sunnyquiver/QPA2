##########################################################################
##
#P DeclareProperty( "IsSemisimpleAlgebra", [ A ] )
##
## The function returns true if  <A>  is a finite dimensional semisimple 
## algebra and searches for other methods oherwise.
## 
InstallMethod( IsSemisimpleAlgebra,
"for an algebra", [ IsAlgebra ], 0, 
function( A )
    
  if IsFiniteDimensional( A ) then 
    return Dimension( RadicalOfAlgebra( A ) ) = 0;
  else
    TryNextMethod( );
  fi;
end 
  );


#######################################################################
##
#A  CompleteSetOfPrimitiveIdempotents( <A> )
##
##  This attribute takes as an argument a finite dimensional  
##  algebra  <A>  for a finite field and returns a complete set of 
##  primitive idempotents  { e_i }  such that  
##                     A \simeq  Ae_1 + ... + Ae_n.
##
##  This function is based on Eberly, W., "Decomposition of algebras 
##  over finite fields and number fields", Computational Complexity 1 
##  (1991), 183–210. See page 84 in Craig A. Struble, "Analysis and 
##  implementation of algorithms for noncommutative algebra", PhD-
##  thesis, Virginia Tech, 2000. 
##
InstallMethod( CompleteSetOfPrimitiveIdempotents,
"for an algebra", true,
[ IsAlgebra ], 0,
function( A )

  local F, d, e, eA, r, m, E, s, one, b, i, j, x, e_hat, map, semi, 
        cents, prims, simp;

  if not IsFiniteDimensional( A ) then
    Error( "The entered algebra is not finite dimensional.\n" ); 
  fi;
  F := LeftActingDomain( A );
  if not IsFinite( F ) then
    Error( "the entered algebra is not over a finite field, \n" );
  fi;
  
  if IsSimpleAlgebra( A ) then 
    d := Dimension( A );
    e := SingularIdempotent( A );
    eA := e * A;
    
    r := Dimension( eA );  # Dimension of a copy of the center of A. 
    m := d / r;            # Matrix dimension (rows and cols)
    E := List( [ 1..m ], x -> Zero( A ) );
    E[ 1 ] := e;
    s := Zero( A );
    one := MultiplicativeNeutralElement( A );
    b := BasisVectors( Basis( A ) );
    
    for i in [ 2..m ] do
      s := s + E[ i - 1 ];
      j := 0;
      repeat
        j := j + 1;
        x := ( one - s ) * b[ j ] * e;
      until x <> Zero( A );
      e_hat := LeftIdentity( x * A );
      E[i] := e_hat * ( one - s );
    od;
    return E;
  fi;
  
  map := NaturalHomomorphismByIdeal( A, RadicalOfAlgebra( A ) );
  semi := Range( map );
  cents := CentralIdempotentsOfAlgebra( semi );
  prims := [ ];
  
  for i in cents do
    simp := FLMLORByGenerators( LeftActingDomain( A ), BasisVectors( Basis( semi * i ) ) );
    SetParent( simp, A ); 
    SetOne( simp, i );
    SetMultiplicativeNeutralElement( simp, i );   
    SetFilterObj( simp, IsAlgebraWithOne );
    prims := Concatenation( prims, CompleteSetOfPrimitiveIdempotents( simp ) );
  od;
  
  return LiftCompleteSetOfOrthogonalIdempotents( map, prims );
end
  );


#######################################################################
##
#O  ZeroDivisor( <A> )
##
##  This function takes as an argument a finite dimensional simple 
##  algebra  <A>  over a finite field and returns a list of two elements  
##  [a, b]  in  <A>  such that  a*b = 0  in <A>, if  <A> is not 
##  commutative. If  <A>  is commutative, it returns an empty list.
##
##  This function is based on Ronyai, L., "Simple algebras are difficult", 
##  In 19th ACM Symposium on Theory of Computing (1987), pp. 398–408, and 
##  Ro ́nyai, L., "Computing the structure of finite algebras", Journal of 
##  Symbolic Computation 9 (1990), 355–373. See page 83 in Craig A. Struble,
##  "Analysis and implementation of algorithms for noncommutative algebra", 
##  PhD-thesis, Virginia Tech, 2000. 
##   
InstallMethod( ZeroDivisor,
"for a finite dimensional simple algebra", true,
[ IsSimpleAlgebra ], 
function( A )
  local F, d, b, bv, cf, x, m, facts, one, f, g;

  if not IsFiniteDimensional( A ) then 
    Error( "Algebra must be finite dimensional.\n" );
  fi; 
  if IsCommutative( A ) then
    return [];
  fi;
  
  F := LeftActingDomain( A );
  if not ( IsFinite( F ) and IsFiniteDimensional( A ) ) then
    Error( "Algebra must be finite." );
  fi;
  d := Dimension( A );
  b := CanonicalBasis( A );
  bv := BasisVectors( b );
  repeat
    cf := List( [ 1..d ], x -> Random( F ) );
    x := LinearCombination( bv, cf );
    m := MinimalPolynomial( F, x );    
    facts := Factors( PolynomialRing( F ), m );
  until Length( facts ) > 1;
  one := MultiplicativeNeutralElement( A );
  f := facts[ 1 ];
  g := Product( facts{ [ 2..Length( facts ) ] } );
  return [ Value( f, x, one ), Value( g, x, one ) ];
end
  );

#######################################################################
##
#O  LeftIdentity( <A> )
##
##  This function takes as an argument a finite dimensional algebra 
##  <A>  and returns a left identity for  <A>, if such a thing exists.   
##  Otherwise it returns fail.
##
InstallMethod( LeftIdentity,
"for a finite dimension algebra", true,
[ IsAlgebra ],
function( A )
  local F, b, bv, n, equ, zero, one, zerovec, vec, row, p, sol,
        i, j;
  
  if not IsFiniteDimensional( A ) then 
    Error( "Algebra must be finite dimensional.\n" );
  fi; 
  F := LeftActingDomain( A );
  b := CanonicalBasis( A );
  bv := BasisVectors( b );
  n := Dimension( A );
  
  equ := [ ];
  zero := Zero( F );
  one := One( F );
  zerovec := ListWithIdenticalEntries( n^2, zero );
  vec := ShallowCopy( zerovec );
  
  for i in [ 1..n ] do
    row := ShallowCopy( zerovec );
    for j in [ 1..n ] do
      p := ( j - 1 ) * n;
      row{ [ p + 1..p + n ] } := Coefficients( b, b[ i ] * b[ j ] );
    od;
    Add( equ, row );
    vec[ ( i - 1 ) * n + i ] := one;
  od;
  sol := SolutionMat( equ, vec );
  if sol <> fail then
    sol := LinearCombination( bv, sol );
  fi;
  
  return sol;
end
  );

#######################################################################
##
#O  SingularIdempotent( <A> )
##
##  This function takes as an argument a finite dimensional simple 
##  algebra  <A>  and returns a primitive idempotent for  <A>.
## 
##  This function is based on Ronyai, L., "Simple algebras are difficult", 
##  In 19th ACM Symposium on Theory of Computing (1987), pp. 398–408, and 
##  Ro ́nyai, L., "Computing the structure of finite algebras", Journal of 
##  Symbolic Computation 9 (1990), 355–373. See page 82 in Craig A. Struble,
##  "Analysis and implementation of algorithms for noncommutative algebra", 
##  PhD-thesis, Virginia Tech, 2000. 
##
InstallMethod( SingularIdempotent,
"for an finite dimensional simple algebra", true,
[ IsSimpleAlgebra ], 
function( A )
  local z, b, bv, x, li, e;

  if not IsFiniteDimensional( A ) then 
    Error( "Algebra must be finite dimensional.\n" );
  fi; 
  while not IsCommutative( A ) do
    z := ZeroDivisor( A );
    b := Basis( A );
    bv := BasisVectors( b );
    x := z[ 1 ];
    li := x * A;
    e := LeftIdentity( li );
    
    A := Algebra( LeftActingDomain( A ), List( bv, x -> e * x * e ) );
  od;
  
  return MultiplicativeNeutralElement( A );
end
  );

#######################################################################
##
#O  LiftIdempotent( <f>, <v> )
##
##  Given an onto homomorphism  <f>  of finite dimensional algebras and
##  an idempotent  <v>  in the range of  <f>, such that the kernel of  
##  <f>  is nilpotent, this function returns an idempotent  e  in the 
##  source of  <f>, such that  f(e) = v.
##
##  See for instance Andersen & Fuller: Rings and categories of modules, 
##  Proposition 27.1 for the algorithm used here.
##
InstallMethod( LiftIdempotent, 
"for a morphism of algebras and one idempotent in the range",
[ IsAlgebraGeneralMapping, IsRingElement ], 0,
function( f, v )
  local g, x, y, nilindex, series, t, k;

  if not IsFiniteDimensional( Source( f ) ) then 
    Error( "The source of the algebra homomorphism must be finite dimensional.\n" );
  fi; 
  #
  # TODO: Check if the kernel of <f> is nilpotent.
  #
  # I := Kernel( f );
  # repeat
  #   L := I;
  #   I := ProductSpace( I, I );
  # until
  #   Dimension( I ) = Dimension( L );
  # if Dimension( I ) <> 0 then
  #    Error( "Kernel of the entered algebra homomorphisms is not nilpotent.\n" );
  # fi;
  #
  if not ( v in Range(f) ) or not v^2 = v then
    Error( "the entered element is not in the range of the map or not an idempotent.\n" );
  fi;      
  g := PreImagesRepresentative( f, v );
  x := g - g * g;
  y := x;
  nilindex := 1;
  series := One( g );
  while not IsZero( y ) do 
    nilindex := nilindex + 1;
    y := x * y;
  od;
  if nilindex = 1 then
      return g;
  else
      t := Zero( y );
      for k in [ 1..nilindex ] do
          t := t + (-1)^( k - 1 ) * Binomial( nilindex, k ) * g^( k - 1 );
      od;
      return g^nilindex * t^nilindex;
  fi;
end
  );

#######################################################################
##
#O  LiftTwoOrtogonalIdempotents( <f>, <v>, <w> )
##
##  Given an onto homomorphism  <f>  of finite dimensional algebras, an 
##  idempotent  <v>  in the source of  <f>  and an idempotent  <w>  in 
##  the range of  <f>, such that the kernel of  <f>  is nilpotent and 
##  f(v)  and  w  are two ortogonal idempotents in the range of  <f>, 
##  this function returns an idempotent  e  in the source of  <f>, such
##  that  \{v, e\}  is a pair of orthogonal idempotents in the source of 
##  <f>. 
##
##  See for instance in Lam: A first course in noncommutative rings, 
##  Proposition 21.25 for the algorithm used here.
##
InstallMethod( LiftTwoOrthogonalIdempotents, 
"for a morphism of algebras, an idempotent in the source and one in the range",
[ IsAlgebraGeneralMapping, IsRingElement, IsRingElement ], 0,
function( f, v, w )
  local g, x, y, series, nilindex, temp;

  if not IsFiniteDimensional( Source( f ) ) then 
    Error( "The source of the algebra homomorphism must be finite dimensional.\n" );
  fi; 
  if ( not IsZero( ImageElm( f, v ) * w ) )  or ( not IsZero( w * ImageElm( f, v ) ) ) then 
      Error("the entered idempotents are not orthogonal in the range of the algebra homomorphism,");
  fi;
  g := LiftIdempotent( f, w );
  x := g * v;
  y := x;
  series := One( g );
  nilindex := 1;       
  while not IsZero( y ) do 
    series := series + y;
    nilindex := nilindex + 1;
    y := x * y;
  od;
  temp := ( One( g ) - v) * series * g * ( One( g ) - x );
  return [ v, temp ];
end
  );

#######################################################################
##
#O  LiftCompleteSetOfOrthogonalIdempotents( <map>, <idempotents> )
##
##  Given a map  <map> :  A --> B  and a complete set  <idempotents>
##  of orthogonal idempotents in  B, which all are in the image
##  of  <map>,  this function computes (when possible) a complete set of
##  orthogonal idempotents of preimages of the idempotents in  B. 
##
InstallMethod( LiftCompleteSetOfOrthogonalIdempotents,
    "for an algebra mapping and list of idempotents",
    true,
    [ IsAlgebraGeneralMapping, IsHomogeneousList ],
    0,
    function( map, idempotents )

    local n, i, j, idems, A, liftidem, temp;    
    #
    # Input OK?
    # Checking if the domain of  <map>  is a finite dimensional algebra.
    #
    if not IsFiniteDimensional( Source( map ) ) then
        Error( "the source of the entered  <map>  is not finite dimensional, \n" );
    fi;
    #
    # Checking if the entered elements are a complete set of orthogonal
    # idempotents. 
    #
    n := Length( idempotents ); 
    for i in [ 1..n ] do
        for j in [ 1..n ] do
            if i = j then
                if not IsIdempotent( idempotents[ i ] ) then
                    Error( "one of the entered elements is not an idempotent,\n" );
                fi;
            else
                if idempotents[ i ] * idempotents[ j ] <> Zero( Range( map ) ) then
                    Error( "the enter elements are not orthogonal,\n" );
                fi;
            fi;
        od;
    od;
    if Sum( idempotents ) <> MultiplicativeNeutralElement( Range( map ) ) then
        Error( "the entered elements are not a complete set of idempotents,\n" );
    fi;
    #
    # Lift the idempotents in the range of  <map>  to idempotents in the 
    # domain of  <map>.
    #
    liftidem := [];
    Add( liftidem, LiftIdempotent( map, idempotents[ 1 ] ) );
    for i in [ 1..Length( idempotents ) - 1 ] do
        temp := LiftTwoOrthogonalIdempotents( map, Sum( liftidem{ [ 1..i ] } ), idempotents[ i + 1 ] );
        Add( liftidem, temp[ 2 ] );
    od;

    return liftidem;
end
);


