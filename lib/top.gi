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

InstallMethod( TopBasis, "for quiver representation",
               [ IsQuiverRepresentation ],
function( R )
  local top_proj, top, basis;
  top_proj := TopProjection( R );
  top := Range( top_proj );
  basis := BasisVectors( Basis( top ) );
  return List( basis, b -> PreImagesRepresentative( top_proj, b ) );
end );

InstallMethod( TopBasis, "for quiver module",
               [ IsQuiverModule ],
function( M )
  local top_proj, top, basis;
  top_proj := TopProjection( M );
  top := Range( top_proj );
  basis := BasisVectors( Basis( top ) );
  return List( basis, b -> PreImagesRepresentative( top_proj, b ) );
end );
