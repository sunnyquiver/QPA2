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
