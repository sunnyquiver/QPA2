#######################################################################
##
#O  DTr(<M>)
##
##  This function returns the dual of the transpose of a module <M>,
##  sometimes denoted by "tau". 
##  
InstallMethod( DTr,
"for a quiver module",
[ IsQuiverModule ], 0,
function( M )
    
    local   side;
    
    side := Side( M );
    if side = LEFT_RIGHT then 
      Error( "The construction of DTr is on one-sided modules.\n" );
    fi;
  
    return DualOfModule( TransposeOfModule( M ) );
end
  );

#######################################################################
##
#O  DTr(<f>)
##
##  This function returns the dual of the transpose of a homomorphism 
##  <f>. 
##  
InstallOtherMethod( DTr,
"for a quiver module homomorphism",
[ IsQuiverModuleHomomorphism ],
function( f )
    
    local   side;
    
    side := Side( Source( f ) );
    if side = LEFT_RIGHT then 
      Error( "The construction of DTr is on one-sided modules.\n" );
    fi;
  
    return DualOfModuleHomomorphism( TransposeOfModuleHomomorphism( f ) );
end
  );

#######################################################################
##
#O  TrD(<M>)
##
##  This function returns the transpose of the dual of a module <M>,
##  sometimes denoted by "tau inverse". It uses the previous methods DualOfModule
##  and TransposeOfModule.
##  
InstallMethod( TrD,
"for a quiver module",
[ IsQuiverModule ],
function( M )
    
    local   side;
    
    side := Side( M );
    if side = LEFT_RIGHT then 
      Error( "The construction of DTr is on one-sided modules.\n" );
    fi;
    
    return TransposeOfModule( DualOfModule( M ) ); 
end
  );

#######################################################################
##
#O  TrD(<M>)
##
##  This function returns the transpose of the dual of a module <M>,
##  sometimes denoted by "tau inverse". It uses the previous methods DualOfModule
##  and TransposeOfModule.
##  
InstallOtherMethod( TrD,
"for a quiver module homomorphism",
[ IsQuiverModuleHomomorphism ],
function( f )
    
    local   side;
    
    side := Side( Source( f ) );
    if side = LEFT_RIGHT then 
      Error( "The construction of DTr is on one-sided modules.\n" );
    fi;
    
    return TransposeOfModuleHomomorphism( DualOfModuleHomomorphism( f ) ); 
end
);

#######################################################################
##
#O  DTr(<M>, <n>)
##
##  This function returns returns DTr^n(<M>) of a module <M>.
##  <n> must be an integer.
##  
InstallOtherMethod( DTr,
"for a quiver module",
[ IsQuiverModule, IS_INT ],
function( M, n )
    
  local U, i;

  U := M;
  if n < 0 then 
      for i in [ 1..-n ] do
          U := TrD( U );
      od;
   else 
       if n >= 1 then 
         for i in [ 1..n ] do
            U := DTr( U );
         od;
      fi;
   fi;

   return U;
end
);

#######################################################################
##
#O  TrD(<M>, <n>)
##  
##  This function returns returns TrD^n(<M>) of a module <M>.
##  <n> must be an integer.
##  
##
InstallOtherMethod( TrD,
"for a quiver module",
[ IsQuiverModule, IS_INT ],
function( M, n )

   local U, i;

   U := M;
   if n < 0 then 
      for i in [ 1..-n ] do
         U := DTr( U );
      od;
   else 
      if n >= 1 then 
         for i in [ 1..n ] do
            U := TrD( U );
         od;
      fi;
   fi;

   return U;
end
  );

#######################################################################
##
#O  AlmostSplitSequence( <M>, side )
##
##  This function finds the almost split sequence ending or starting 
##  in the module <M> depending on the second argument <side>.  It
##  checks if the module is not injective or not projective also 
##  depending on the argument <side>. It returns fail if the module 
##  is projective and side = RIGHT and if the module is injective and 
##  side = LEFT. The almost split sequence is returned as a pair of 
##  maps, the monomorphism and the epimorphism. The function assumes 
##  that the module  <M>  is indecomposable. The entered endterm and the
##  returned endterm might be module that is isomorphic to the input, 
##  not necessarily identical. 
##
InstallMethod( AlmostSplitSequence, 
"for a quiver module",
[ IsQuiverModule, IsDirection ],
function( M, direction )

  local K, startterm, endterm, ext, Bext, extension, cycle, 
        Endstartterm, radEndstartterm, rad2Endstartterm, g, BImage_g, 
        topradEndstartterm, temp, n, i, t, ass;
#
# ToDo: Add test of input with respect to being indecomposable.
#
  if direction = LEFT_RIGHT then 
    Error( "Enter LEFT or RIGHT as a direction.\n" );
  fi;
  K := LeftActingDomain( M );
  if direction = RIGHT and IsProjectiveModule( M ) then 
      return fail;
  fi;
  if direction = LEFT and IsInjectiveModule( M ) then 
      return fail;
  fi;
  if direction = RIGHT then
    startterm := DTr( M );
    endterm := M;
  else
    startterm := M;
    endterm := TrD( M );
  fi;
  
  ext := Ext( 1, endterm, startterm );
  Bext := BasisVectors( Basis( ext ) );
  extension := Bext[ Length( Bext ) ];
  cycle := AsCycle( extension ); 
  #
  # Finding the radical of End(startterm)
  #
  Endstartterm := EndomorphismAlgebra( startterm );
  radEndstartterm := RadicalOfAlgebra( Endstartterm );
  rad2Endstartterm := ProductSpace( radEndstartterm, radEndstartterm );
  g := NaturalHomomorphismByIdeal( radEndstartterm, rad2Endstartterm );
  BImage_g := BasisVectors( Basis( Range( g ) ) );
  if Length( BImage_g ) = 0 then
      return AsExactSequence( extension );
  fi;
  topradEndstartterm := List( BImage_g, x -> FromEndMToHomMM( startterm, PreImagesRepresentative( g, x ) ) );
  #
  # Finding an element in the socle of Ext^1( endterm, startterm )
  #
  temp := cycle;
  n := Length( topradEndstartterm );
  i := 1;
  repeat
    t := topradEndstartterm[ i ];
    if not IsZero( ExtensionFromCycle( ext, temp * t ) ) then 
      temp := temp * t;
      i := 1;
    else
      i := i + 1;
    fi;
  until
    ( i = n + 1 );
  #
  # Constructing the almost split sequence in Ext^1(endterm,startterm)
  #
  ass := ExtensionFromCycle( ext, temp );

  return AsExactSequence( ass );
end
);

