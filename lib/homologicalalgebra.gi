##########################################################################
##
#P DeclareProperty( "IsSemisimpleAlgebra", [ A ] )
##
## The function returns true if  <A>  is a finite dimensional semisimple 
## algebra and searches for other methods oherwise.
## 
InstallMethod( IsSemisimpleAlgebra,
"for an algebra", [ IsAlgebra ], 0, 
function( A )
    
  if IsFiniteDimensional( A ) then 
    return Dimension( RadicalOfAlgebra( A ) ) = 0;
  else
    TryNextMethod( );
  fi;
end 
  );

