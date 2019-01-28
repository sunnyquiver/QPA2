#######################################################################
##
#A  TopOfRepresentation( <R> )
##
##  This function computes the top  R/rad(R)  of the representation R. 
##
InstallMethod( TopOfRepresentation, 
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ],
function( R )
    
    return Range( TopProjection( R ) );
end
);

#######################################################################
##
#A  TopOfModule( <M> )
##
##  This function computes the top  M/rad(M)  of the module M. 
##
InstallMethod( TopOfModule, 
"for a IsQuiverModule",
[ IsQuiverModule ],
function( M )
    local   side,  top;
        
    side := Side( M ); 
    top := TopOfRepresentation( UnderlyingRepresentation( M ) );
    return AsModule( side, top );
end
);
