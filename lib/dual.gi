InstallMethod( DualFunctor, "for a category of vector spaces",
        [ IsVectorSpaceCategory ],
        function( C )
    
    local   dual,  morphism;
    
    dual := CapFunctor( "DualFunctor", [ [ C, true ] ], C );
    
    morphism := function( V1, h, V2 ) 
        local   mat;
        
        mat := TransposedMat( LeftMatrixOfLinearTransformation( h ) ); 
        return LinearTransformationByLeftMatrix( V1, V2, mat );
    end;
    
    AddObjectFunction( dual, X -> X ); 
    
    AddMorphismFunction( dual, morphism );
    
    return dual;
end 
  ); 

InstallMethod( DualFunctor, "for a category of quiver representations",
        [ IsQuiverRepresentationCategory ],
        function( C )
    
    local   cat,  vecdual,  D,  dual,  morphism,  object;
    
    cat := VectorSpaceCategory( C ); 
    vecdual := DualFunctor( cat );
    D := CategoryOfQuiverRepresentations( OppositeAlgebra( AlgebraOfCategory( C ) ) ); 
    dual := CapFunctor( "DualFunctor", [ [ C, true ] ], D );
    
    morphism := function( R1, h, R2 ) 
        local   mats;
        
        mats := List( MapsOfRepresentationHomomorphism( h ), f -> ApplyFunctor( vecdual, f ) ); 
        return QuiverRepresentationHomomorphism( R1, R2, mats );
    end;
    
    object := function( R )
        local maps, spaces;
        
        maps := List( MapsOfRepresentation( R ), f -> ApplyFunctor( vecdual, f ) );
        spaces := List( VectorSpacesOfRepresentation( R ), V -> ApplyFunctor( vecdual, V ) );
        
        return QuiverRepresentation( D, spaces, maps );
    end;
    
    AddObjectFunction( dual, object );
    
    AddMorphismFunction( dual, morphism );
    
    return dual;
end 
  ); 

InstallMethod( DualFunctor, "for a category of quiver modules",
        [ IsQuiverModuleCategory ],
        function( C )
    
    local   side,  F,  RC,  repdual,  RD,  G;
    
    if Side( C ) = LEFT_RIGHT then
      side := LEFT_RIGHT;
    else
      side := Opposite( Side( C ) );
    fi;
    F := UnderlyingRepresentationFunctor( C );
    RC := AsCapCategory( Range( F ) );
    repdual := DualFunctor( RC );
    RD := AsCapCategory( Range( repdual ) ); 
    G := AsModuleFunctor( side, RD );
    
    return PreComposeFunctors( PreComposeFunctors( F, repdual ), G );
end 
  );

#######################################################################
##
#A  DualOfModule( <M> )
##
##  This function computes the dual DM of the module <M>, that is, 
##  it computes Hom_k(M, k). If <M> is a module over A, then DM is a
##  module over the opposite algebra of A.
##
InstallMethod ( DualOfModule,
"for a IsQuiverModule",
[ IsQuiverModule ], 0,
function( M )
    
    local C, D;
    
    C := CapCategory( M );
    D := DualFunctor( C );
    
    return ApplyFunctor( D, M );
end
  );

#######################################################################
##
#A  DualOfRepresentation( <R> )
##
##  This function computes the dual DR of the representation <R>, that 
##  is, it computes Hom_k(R, k). If <M> is a module over A, then DR is
##  a module over the opposite algebra of A.
##
InstallMethod ( DualOfRepresentation,
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ], 0,
function( R )
    
    local C, D;
    
    C := CapCategory( R );
    D := DualFunctor( C );
    
    return ApplyFunctor( D, R );
end
  );

#######################################################################
##
#A  DualOfRepresentationHomomorphism( <f> )
##
##  This function computes the dual of a homomorphism from the module  
##  <M>  to the module  <N>.
##
InstallMethod ( DualOfRepresentationHomomorphism,
"for a map between two representations of a quiver",
[ IsQuiverRepresentationHomomorphism ],
function( f )

    local   C,  D;
    
    C := CapCategory( f );
    D := DualFunctor( C );
    
    return ApplyFunctor( D, f );
end
  );

#######################################################################
##
#A  DualOfModuleHomomorphism( <f> )
##
##  This function computes the dual of a homomorphism from the module  
##  <M>  to the module  <N>.
##
InstallMethod ( DualOfModuleHomomorphism,
"for a map between two representations of a quiver",
[ IsQuiverModuleHomomorphism ],
function( f )

    local   C,  D;
    
    C := CapCategory( f );
    D := DualFunctor( C );
    
    return ApplyFunctor( D, f );
end
  );