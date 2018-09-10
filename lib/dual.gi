InstallMethod( DualFunctor, "for a category of vector spaces",
        [ IsVectorSpaceCategory ],
        function( C )
    
    local   dual,  morphism;
    
    dual := CapFunctor( "DualFunctor", [ [ C, true ] ], C );
    
    morphism := function( V1, h, V2 ) 
        local   mat;
        
        mat := TransposedMat( LeftMatrixOfLinearTransformation( h ) ); 
        return LinearTransformationByLeftMatrix( V2, V1, mat );
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
        return QuiverRepresentationHomomorphism( R2, R1, mats );
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
    
    side := Opposite( Side( C ) );
    F := UnderlyingRepresentationFunctor( C );
    RC := AsCapCategory( Range( F ) );
    repdual := DualFunctor( RC );
    RD := AsCapCategory( Range( repdual ) ); 
    G := AsModuleFunctor( side, RD );
    
    return PreCompose( [ F, repdual, G ] );
end 
  ); 

