#######################################################################
##
#P  IsSimpleQuiverModule( <M> )
##
##  This function checks if a module <M> is a simple module over an 
##  admissible quotient of a path algebra. 
##
InstallMethod ( IsSimpleQuiverModule, 
"for a QuiverModule",
[ IsQuiverModule ], 
function( M )

  return IsSimpleRepresentation( UnderlyingRepresentation( M ) );
end
  );

DeclareSideOperations( SimpleModules, SimpleLeftModules, SimpleRightModules,
                       SimpleBimodules );

InstallMethodWithSides( SimpleModules, [ "algebra" ],
side -> function( A )
  
  local simples, s;
  
  simples := List( SimpleRepresentations( A^side ), AsModule^side );
  for s in simples do
    SetIsSimpleQuiverModule( s, true );
  od;

  return simples;
end );

DeclareSideOperations( IndecProjModules, IndecProjLeftModules, IndecProjRightModules,
                       IndecProjBimodules );

InstallMethodWithSides( IndecProjModules, [ "algebra" ],
side -> function( A )
  return List( IndecProjRepresentations( A^side ), AsModule^side );
end );

DeclareSideOperations( IndecInjModules, IndecInjLeftModules, IndecInjRightModules,
                       IndecInjBimodules );

InstallMethodWithSides( IndecInjModules, [ "algebra" ],
side -> function( A )

  return List( IndecInjRepresentations( A^side ), AsModule^side );
end );

