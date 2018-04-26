


InstallMethod( RestrictionFunctor, "for a homomorphism of quiver algebras",
        [ IsQuiverAlgebraHomomorphism, IsQuiverRepresentationCategory, IsQuiverRepresentationCategory ],
        function( f, C, D )
    
    local   restriction,  verteximages,  arrowimages,  A,  
            representation,  morphism;
    
    restriction := CapFunctor( "Restriction", C, D );
    
    verteximages := VertexImages( f );
    arrowimages := ArrowImages( f ); 
    A := Source( f );
    
    representation := function( R ) 
        local   arrows,  newspanningsetbyvertex,  inclusionsbyvertex,  
                projectionsbyvertex,  linear_transformations,  i,  
                source,  target,  lintrans,  rep;
         
        arrows := Arrows( PathAlgebra( A ) ); 
        newspanningsetbyvertex := List( verteximages, v -> List( Basis( R ), b -> AsVector ( PathAction( b, v ) ) ) );
        inclusionsbyvertex := List( newspanningsetbyvertex, s -> SubspaceInclusion( AsQPAVectorSpace( R ), s ) ); 
        projectionsbyvertex := List( inclusionsbyvertex, LeftInverse );
        linear_transformations := [ ];
        for i in [ 1..Length( arrows ) ] do
            source := VertexNumber( Source( arrows[ i ] ) ); 
            target := VertexNumber( Target( arrows[ i ] ) );
            lintrans := PreCompose( [ inclusionsbyvertex[ source ], 
                                QuiverAlgebraActionAsLinearTransformation( arrowimages[ i ] ), projectionsbyvertex[ target ] ] ); 
            Add( linear_transformations, lintrans );
        od;
        rep := QuiverRepresentationByObjectsAndMorphisms( D, List( inclusionsbyvertex, Source ), linear_transformations );
        
        return [ rep, inclusionsbyvertex, projectionsbyvertex ];
    end;
    
    morphism := function( h ) 
        local   rep1,  inc,  rep2,  proj,  hlintrans,  morphisms;
        
        rep1 := representation( Source( h ) );
        inc := rep1[ 2 ];
        rep2 := representation( Range( h ) );
        proj := rep2[ 3 ];
        hlintrans := AsLinearTransformation( h ); 
        morphisms := List( [ 1..Length( rep1[ 2 ] ) ], i -> PreCompose( [ inc[ i ], hlintrans, proj[ i ] ] ) ); 
                    
        return QuiverRepresentationHomomorphismByMorphisms( rep1[ 1 ], rep2[ 1 ], morphisms );
    end;
    
    AddObjectFunction( restriction, X -> representation( X )[ 1 ] ); 
    
    AddMorphismFunction( restriction, morphism );
    
    return restriction;
end 
  ); 

InstallMethod( RestrictionToLeftFunctor, "for a bimodule category",
        [ IsQuiverBimoduleCategory ],
        function( C )
    
    local   D,  f;
    
    D := UnderlyingRepresentationCategory( C );
    f := TensorAlgebraInclusions( AlgebraOfCategory( D ) )[ 1 ];
    return PreCompose( [ UnderlyingRepresentationFunctor( C ), 
                   RestrictionFunctor( f, D, CategoryOfQuiverRepresentations( Source( f ) ) ), 
                   AsLeftModuleFunctor( Source( f ) ) ] ); 
end );

InstallMethod( RestrictionToRightFunctor, "for a bimodule category",
        [ IsQuiverBimoduleCategory ],
        function( C )
    
    local   D,  f;
    
    D := UnderlyingRepresentationCategory( C );
    f := TensorAlgebraInclusions( AlgebraOfCategory( D ) )[ 2 ];
    return PreCompose( [ UnderlyingRepresentationFunctor( C ), 
                   RestrictionFunctor( f, D, CategoryOfQuiverRepresentations( Source( f ) ) ), 
                   AsRightModuleFunctor( Source( f ) ) ] ); 
end );

