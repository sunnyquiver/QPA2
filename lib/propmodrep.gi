#######################################################################
##
#O  IsInAdditiveClosure( <M>, <N> )
##
##  This function returns true if the object  <M>  is in the additive
##  closure of the object  <N>, an error message if  <M>  and  <N>  are 
##  not objects in the same category and false otherwise.
##
InstallMethod( IsInAdditiveClosure, 
    "for two IsFieldCategoryObject",
    [ IsFieldCategoryObject, IsFieldCategoryObject ],
    function( M, N ) 

    local   HomMN,  HomNM,  BMN,  BNM,  MM,  mn,  nm,  HomMM,  V_M;

    if CapCategory( M ) <> CapCategory( N ) then 
        return fail;
    fi;
#
# Computing Hom(M,N) and Hom(N,M), finding the subspace in Hom(M,M)
# spanned by Hom(M,N)*Hom(M,N), if they have the same dimension, then 
# the identity on  M  is in the linear span of Hom(M,N)*Hom(M,N) and  
# module M is in the additive closure of N. 
#
    HomMN := Hom( M, N );
    HomNM := Hom( N, M );
    BMN := BasisVectors( Basis( HomMN ) );
    BNM := BasisVectors( Basis( HomNM ) );
    MM := [];
    for mn in BMN do
        for nm in BNM do
            Add( MM, PreCompose( mn, nm ) );
        od;
    od;
    HomMM := Hom( M, M );
    V_M := Source( SubspaceInclusion( HomMM, MM ) );  # Cannot take dimension of this! TODO!
    if Dimension( V_M ) = Dimension( Hom( M, M ) ) then 
        return true;
    else
        return false;
    fi;
end
  );

#######################################################################
##
#O  IsomorphicModules( <M>, <N> )
##
##  This function returns true if the modules  <M>  and  <N>  are 
##  isomorphic, an error message if  <M>  and  <N>  are not modules over 
##  the same algebra and false otherwise.
##  
InstallMethod( IsomorphicModules, 
"for two IsQuiverModule",
[ IsQuiverModule, IsQuiverModule  ],
function( M, N ) 

    local L;
    
    if DimensionVector( M ) <> DimensionVector( N ) then 
        return false;
    elif Dimension( M ) = 0 and Dimension( N ) = 0 then 
        return true; 
    fi; 
    L := MaximalCommonDirectSummand( M, N );
    if L = false then 
        return false;
    fi;
    if Dimension( L[ 2 ] ) = 0 and Dimension( L[ 3 ] ) = 0 then 
        return true;
    else
        return false;
    fi;
end
  ); 

#######################################################################
##
#O  IsomorphicRepresentations( <R1>, <R2> )
##
##  This function returns true if the representations  <R1>  and  <R2>  
##  are isomorphic, an error message if  <R1>  and  <R2>  are not 
##  representations over the same algebra and false otherwise.
##  
InstallMethod( IsomorphicRepresentations, 
"for two IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation  ],
function( M, N ) 

    local L;
    
    if DimensionVector( M ) <> DimensionVector( N ) then 
        return false;
    elif Dimension( M ) = 0 and Dimension( N ) = 0 then 
        return true; 
    fi; 
    L := MaximalCommonDirectSummand( M, N );
    if L = false then 
        return false;
    fi;
    if Dimension( L[ 2 ] ) = 0 and Dimension( L[ 3 ] ) = 0 then 
        return true;
    else
        return false;
    fi;
end
  );

#######################################################################
##
#O  LoewyLength ( <R> )
##
##  This function returns the Loewy length of the representation  M, 
##  for a representation over a (quotient of a) path algebra.
##
InstallMethod( LoewyLength, 
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ],
function( R ) 

   local N, i;

   N := R;
   i := 0;
   if Dimension( R ) = 0 then 
      return 0;
   fi;
   repeat 
       N := RadicalOfRepresentation( N );
       i := i + 1;
   until
     Dimension( N ) = 0;
   
   return i;
end
);