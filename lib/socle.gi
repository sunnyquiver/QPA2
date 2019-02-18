#######################################################################
##
#A  SocleOfRepresentationInclusion( <R> )
##
##  This function computes the map from the socle of  M  to the 
##  representation R, soc( R ) ---> R.
##
InstallMethod( SocleOfRepresentationInclusion, 
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ],
function( R )

    local   f,  g;

    f := TopProjection( DualOfRepresentation( R ) );
    g := DualOfRepresentationHomomorphism( f );
#        SetSocleOfRepresentation( R, Source( g ) );
    
    return g;
end
  );

#######################################################################
##
#A  SocleOfRepresentation( <R> )
##
##  This function computes the socle  soc( R )  of the representation R. 
##
InstallMethod( SocleOfRepresentation, 
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ],
function( R )

    return Source (SocleOfRepresentationInclusion( R ) );
end
);

#######################################################################
##
#O  SocleSeries ( <R> )
##
##  This function returns the socle series of the representation  R, 
##  for a representation over a (quotient of a) path algebra. It 
##  returns a list of dimension vectors of the modules: 
##  [..., soc(R/soc^3 R), soc(R/soc^2 R), soc(R/soc R), soc R].
##
InstallMethod( SocleSeries, 
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ],
function( R ) 

   local N, series, i;

   if Dimension( R ) = 0 then 
      return DimensionVector( R );
   fi;
   series := [];
   i := 0;
   N := DualOfRepresentation( R );
   repeat     
       Add( series, DimensionVector( TopOfRepresentation( N ) ) );
       N := RadicalOfRepresentation( N );
       i := i + 1;
   until
     Dimension( N ) = 0;
   
   return Reversed( series );
end
  );