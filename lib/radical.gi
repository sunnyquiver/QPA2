#######################################################################
##
#A  RadicalOfRepresentation( <R> )
##
##  This function computes the radical of the representation R. 
##
InstallMethod( RadicalOfRepresentation, 
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ],
function( R )
    
    return Source( RadicalInclusion( R ) );
end
);

#######################################################################
##
#A  RadicalOfModule( <M> )
##
##  This function computes the radical of the module M. 
##
InstallMethod( RadicalOfModule, 
"for a IsQuiverModule",
[ IsQuiverModule ],
function( M )
    local   side,  rad;
        
    side := Side( M ); 
    rad := RadicalOfRepresentation( UnderlyingRepresentation( M ) );
    return AsModule( side, rad );
end
  );

#######################################################################
##
#O  RadicalSeries ( <M> )
##
##  This function returns the radical series of the module  M, for a 
##  module over a (quotient of a) path algebra. It returns a list of 
##  dimension vectors of the modules: [ M/rad M, rad M/rad^2 M, 
##  rad^2 M/rad^3 M, .....].
##
InstallMethod( RadicalSeries, 
"for a IsQuiverModule",
[ IsQuiverModule ], 0,
function( M ) 

   local N, radlayers, i;

   if Dimension( M ) = 0 then 
      return DimensionVector( M );
   else
      radlayers := [ ];
      i := 0;
      N := M;
      repeat     
         Add( radlayers, DimensionVector( TopOfModule( N ) ) );
         N := RadicalOfModule( N );
         i := i + 1;
      until
         Dimension( N ) = 0;
      return radlayers;
   fi;
end
  );

#######################################################################
##
#O  RadicalSeries ( <M> )
##
##  This function returns the radical series of the representation  M, 
##  for a representation over a (quotient of a) path algebra. It 
##  returns a list of dimension vectors of the representations: 
##  [ M/rad M, rad M/rad^2 M, rad^2 M/rad^3 M, .....].
##
InstallMethod( RadicalSeries, 
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ], 0,
function( M ) 

   local N, radlayers, i;

   if Dimension( M ) = 0 then 
      return DimensionVector( M );
   else
      radlayers := [ ];
      i := 0;
      N := M;
      repeat     
         Add( radlayers, DimensionVector( TopOfRepresentation( N ) ) );
         N := RadicalOfRepresentation( N );
         i := i + 1;
      until
         Dimension( N ) = 0;
      return radlayers;
   fi;
end
  );