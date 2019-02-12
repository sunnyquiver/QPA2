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