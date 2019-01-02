#######################################################################
##
#O  ComplexityOfRepresentation( <R>, <n> )
##
##  Recall: If a function  f(x)  is a polynomial in  x, the degree of  
##  f(x)  is given by lim_{n --> infinity} log |f(n)|/log n. 
##  
##  Given a representation  R  this function computes an estimate of the 
##  complexity of the module by approximating the complexity by 
##  considering the limit  lim_{m --> infinity} log dim(P(R)(m))/log m
##  where P(R)(m) is the m-th projective in a minimal projective 
##  resolution of  R  at stage  m.  This limit is estimated by 
##  log dim(P(R)(n))/log n.
##  
InstallMethod ( ComplexityOfRepresentation, 
"for a module of a fin. dim. quotient of a path algebra and a positive integer",
[ IsQuiverRepresentation, IsPosInt ], 
function( R, n )

  local A, projres, temp;
    
  A := AlgebraOfRepresentation( R );
  if HasIsFiniteGlobalDimensionAlgebra( A ) and IsFiniteGlobalDimensionAlgebra( A ) then
    return 0;
  fi;
    
  if IsAdmissibleQuiverAlgebra( A ) then 
    projres := AsChainComplex( ProjectiveResolution( R ) ); 
    temp := Dimension( projres[ n ] ) * 1.0;
    if Dimension( projres[ n ] ) = 0 then
      return 0;
    else
      temp := Log10( temp ) / Log10( n * 1.0 );
      return Int( Round( temp + 1.0 ) );
    fi;
  else
    Error( "entered algebra is not finite dimensional or an admissible quotient of a path algebra,\n" );
  fi;
end 
  );

#######################################################################
##
#O  ComplexityOfModule( <M>, <n> )
##
##  Recall: If a function  f(x)  is a polynomial in  x, the degree of  
##  f(x)  is given by lim_{n --> infinity} log |f(n)|/log n. 
##  
##  Given a module  M  this function computes an estimate of the 
##  complexity of the module by approximating the complexity by 
##  considering the limit  lim_{m --> infinity} log dim(P(M)(m))/log m
##  where P(M)(m) is the m-th projective in a minimal projective 
##  resolution of  M  at stage  m.  This limit is estimated by 
##  log dim(P(M)(n))/log n.
##  
InstallMethod ( ComplexityOfModule, 
"for a module of a fin. dim. quotient of a path algebra and a positive integer",
[ IsQuiverModule, IsPosInt ], 
function( M, n )

  local R;
    
  R := UnderlyingRepresentation( M );
  
  return ComplexityOfRepresentation( R, n );
end
  );


#######################################################################
##
#O  ComplexityOfAlgebra( <A>, <n> )
##
##  Recall: If a function  f(x)  is a polynomial in  x, the degree of  
##  f(x)  is given by lim_{n --> infinity} log |f(n)|/log n. 
##  
##  Given an algebra  A  this function computes an estimate of the 
##  maximal complexity of the simple modules by approximating the 
##  complexity of each simple  S  by  considering the limit  
##  lim_{m --> infinity} log dim(P(S)(m))/log m
##  where P(S)(m) is the m-th projective in a minimal projective 
##  resolution of a simple module  S  at stage  m.  This limit is 
##  estimated by  log dim(P(S)(n))/log n.
##  
InstallMethod ( ComplexityOfAlgebra, 
"for a fin. dim. quotient of a path algebra and a positive integer",
[ IsQuiverAlgebra, IsPosInt ], 
function( A, n )

  local S, temp;
  #
  #
  if HasIsFiniteGlobalDimensionAlgebra( A ) and IsFiniteGlobalDimensionAlgebra( A ) then
    return 0;
  fi;
  if IsAdmissibleQuiverAlgebra( A ) then 
    S := SimpleRepresentations( A );
    temp := List( S, s -> ComplexityOfRepresentation( s, n ) );
    return Maximum( temp );
  else
    Error( "entered algebra is not finite dimensional or an admissible quotient of a path algebra,\n" );
  fi;
end 
  );
