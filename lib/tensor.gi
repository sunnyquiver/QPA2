InstallMethod( TensorProductOfRepresentations,
        "for two representations of (two) quivers",
        [ IsQuiverRepresentation, IsQuiverRepresentation ],
        function( R1, R2 )
    
    local   B1,  B2,  A1,  A2,  A3,  K,  QB1,  QB2,  B3,  QB3,  
            verticesB3,  verticesA2,  partialtensor,  projections,  v,  
            tempdim,  i,  l,  j,  ij,  jl,  temprelations,  a,  start,  
            target,  istart,  targetl,  b1,  b2,  V,  arrowsB3,  maps,  
            dimension,  alpha,  source,  basis_i_j,  images_iprime_j,  
            beta,  iprime,  iB1,  iprimeB1,  basisR1_i_l,  
            basisR2_l_j,  b,  bprime,  matrix,  images_i_jprime,  
            jprime,  jB2,  jprimeB2,  tensorproduct,  
            elementarytensor;
    
    B1 := AlgebraOfRepresentation( R1 );
    B2 := AlgebraOfRepresentation( R2 );
    if not ( IsTensorProductOfAlgebras( B1 ) and IsTensorProductOfAlgebras( B2 ) ) then
        Error( "Entered representations are not representations over a tensor product,\n" );
    fi;
    A1 := TensorProductFactors( B1 )[ 1 ];
    A2 := TensorProductFactors( B1 )[ 2 ];
    A3 := TensorProductFactors( B2 )[ 2 ];
    K := LeftActingDomain( A1 );
    if OppositeAlgebra( A2 ) <> TensorProductFactors( B2 )[ 1 ] then 
        Error( "Entered modules are compatible for taking the tensor product,\n" );
    fi;
    
    QB1 := QuiverOfAlgebra( B1 );
    QB2 := QuiverOfAlgebra( B2 );
    B3 := TensorProductOfAlgebras( A1, A3 );
    QB3 := QuiverOfAlgebra( B3 );
    
    verticesB3 := Vertices( QB3 );
    verticesA2 := Vertices( QuiverOfAlgebra( A2 ) );
    
    partialtensor := function( r1, r2, v )
        local   i,  l,  temp,  j,  ij,  jl,  r1elem,  r2elem;
        
        i := ProjectPathFromProductQuiver( 1, v );  # vertex in A1
        l := ProjectPathFromProductQuiver( 2, v );  # vertex in A3
        temp := [ ];
        for j in verticesA2 do
            ij := PathInProductQuiver( QB1, [ i, j ] );
            jl := PathInProductQuiver( QB2, [ OppositePath( j ), l ] );
            r1elem := ElementVector( r1, ij );
            r2elem := ElementVector( r2, jl );
            if not IsEmptyVector( r1elem ) and not IsEmptyVector( r2elem ) then 
                Append( temp, KroneckerProduct( [ AsList( r1elem ) ], [ AsList( r2elem ) ] )[ 1 ] );
            fi;
        od;
        
        return temp;
    end;
 
    projections := [ ];
    for v in verticesB3 do
        tempdim := 0;
        i := ProjectPathFromProductQuiver( 1, v );  # vertex in A1
        l := ProjectPathFromProductQuiver( 2, v );  # vertex in A3
        for j in verticesA2 do
            ij := PathInProductQuiver( QB1, [ i, j ] );
            jl := PathInProductQuiver( QB2, [ OppositePath( j ), l ] );
            tempdim := tempdim + VertexDimension( R1, ij ) * VertexDimension( R2, jl );
        od;
        temprelations := [ ];
        for a in Arrows( QuiverOfAlgebra( A2 ) ) do
            start := Source( a ); 
            target := Target( a );
            istart := PathInProductQuiver( QB1, [ i, start ] );
            targetl := PathInProductQuiver( QB2, [ OppositePath( target ), l ] );
            
            for b1 in BasisVectorsByVertex( Basis( R1 ) )[ VertexIndex( istart ) ] do
                for b2 in BasisVectorsByVertex( Basis( R2 ) )[ VertexIndex( targetl ) ] do
                    Add( temprelations, partialtensor( QuiverAlgebraAction( b1, ElementaryTensor( One( A1 ), One( A2 ) * a, B1 ) ), b2, v) - 
                         partialtensor( b1, QuiverAlgebraAction( b2, ElementaryTensor( One( OppositeAlgebra( A2 ) ) * OppositePath( a ), One( A3), B2) ), v ) );
                od;
            od;
        od;
        V := K^tempdim;
        if tempdim = 0 then
          Add( projections, IdentityMapping( V ) );
        else
          Add( projections, NaturalHomomorphismBySubspace( V, Subspace( V, temprelations ) ) );
        fi;
    od;
    
    arrowsB3 := [ ];
    maps := [ ];
    dimension := List( projections, p -> Dimension( Range( p ) ) );
    for a in Arrows( QuiverOfAlgebra( A1 ) ) do
        for j in Vertices( QuiverOfAlgebra( A3 ) ) do
            alpha := PathInProductQuiver( QB3, [ a, j ] );
            source := Source( alpha );
            target := Target( alpha );
            if dimension[ VertexIndex( source ) ] = 0 or dimension[ VertexIndex( target ) ] = 0 then
                continue;
            fi;
            Add( arrowsB3, alpha );
            basis_i_j := [ ];
            images_iprime_j := [ ];
            for l in Vertices( QuiverOfAlgebra( A2 ) ) do
                beta := PathInProductQuiver( QB1, [ a, l ] );
                i := Source( a );
                iprime := Target( a );
                iB1 := PathInProductQuiver( QB1, [ i, l ] );
                iprimeB1 := PathInProductQuiver( QB1, [ iprime, l ] );
                basisR1_i_l := BasisVectorsByVertex( Basis( R1 ) )[ VertexIndex( iB1 ) ];
                basisR2_l_j := BasisVectorsByVertex( Basis( R2 ) )[ VertexIndex( PathInProductQuiver( QB2, [ OppositePath( l ), j ] ) ) ];
                for b in basisR1_i_l do
                    for bprime in basisR2_l_j do
                        Add( basis_i_j, partialtensor( b, bprime, PathInProductQuiver( QB3, [ i, j ] ) ) );  
                        Add( images_iprime_j, partialtensor( PathAction( b, beta ), bprime, PathInProductQuiver( QB3, [ iprime, j ] ) ) ); 
                    od;
                od;
            od;
            matrix := [ ];
            for b in BasisVectors( Basis( Range( projections[ VertexIndex( source ) ] ) ) ) do
                bprime := PreImagesRepresentative( projections[ VertexIndex( source ) ], b ); 
                Add( matrix, ImageElm( projections[ VertexIndex( target ) ], bprime * basis_i_j^( -1 ) * images_iprime_j ) );
            od;
            Add( maps, matrix );
        od;
    od;
    
    for i in Vertices( QuiverOfAlgebra( A1 ) ) do
        for a in Arrows( QuiverOfAlgebra( A3 ) ) do
            alpha := PathInProductQuiver( QB3, [ i, a ] );
            source := Source( alpha );
            target := Target( alpha );
            if dimension[ VertexIndex( source ) ] = 0 or dimension[ VertexIndex( target ) ] = 0 then
                continue;
            fi;
            Add( arrowsB3, alpha );
            basis_i_j := [ ];
            images_i_jprime := [ ];
            for l in Vertices( QuiverOfAlgebra( A2 ) ) do
                beta := PathInProductQuiver( QB2, [ OppositePath( l ), a ] );
                j := Source( a );
                jprime := Target( a );
                jB2 := PathInProductQuiver( QB2, [ OppositePath( l ), j ] );
                jprimeB2 := PathInProductQuiver( QB2, [ OppositePath( l ), jprime ] );
                basisR1_i_l := BasisVectorsByVertex( Basis( R1 ) )[ VertexIndex( PathInProductQuiver( QB1, [ i, l ] ) ) ];
                basisR2_l_j := BasisVectorsByVertex( Basis( R2 ) )[ VertexIndex( jB2 ) ];
                for b in basisR1_i_l do
                    for bprime in basisR2_l_j do
                        Add( basis_i_j, partialtensor( b, bprime, PathInProductQuiver( QB3, [ i, j ] ) ) );  
                        Add( images_i_jprime, partialtensor( b, PathAction( bprime, beta ), PathInProductQuiver( QB3, [ i, jprime ] ) ) ); 
                    od;
                od;
            od;
            matrix := [ ];
            for b in BasisVectors( Basis( Range( projections[ VertexIndex( source ) ] ) ) ) do
                bprime := PreImagesRepresentative( projections[ VertexIndex( source ) ], b ); 
                Add( matrix, ImageElm( projections[ VertexIndex( target ) ], bprime * basis_i_j^( -1 ) * images_i_jprime ) );
            od;
            Add( maps, matrix );
        od;
    od;
    tensorproduct := QuiverRepresentation( B3, dimension, arrowsB3, maps );
    
    elementarytensor := function( r1, r2 )
        local   vectors,  v;
        
        vectors := [ ];
        for v in verticesB3 do
           if dimension[ VertexIndex( v ) ] > 0 then
               vectors[ VertexIndex( v ) ] := ImageElm( projections[ VertexIndex( v ) ], partialtensor( r1, r2, v ) );
           fi;
       od;
       
       return QuiverRepresentationElement( tensorproduct, vectors );
    end;
    
    SetElementaryTensorFunction( tensorproduct, elementarytensor ); 
    SetTensorProductFactors( tensorproduct, [ R1, R2 ] );
    
    return tensorproduct;
end
  );

InstallMethod( TensorProductOfHomomorphisms,
        "for two homomorphisms of representations",
        [ IsQuiverRepresentationHomomorphism, IsQuiverRepresentationHomomorphism, IsQuiverRepresentation, IsQuiverRepresentation ],
        function( f, g, T1, T2 )
    
  local pi, elementarytensor1, bastop, K, factors, M, N, dimM, dimN, 
        basM, basN, VM, VN, basVM, basVN, mat, v, w, m, n, gens, 
        generators, gen, temp, i, b, elementarytensor2, newgenerators, 
        images, temp1, temp2;
    
    if ( TensorProductFactors( T1 ) <> [ Source( f ), Source( g ) ] ) or 
       ( TensorProductFactors( T2 ) <> [ Range( f ), Range( g ) ] ) then
        Error( "The entered homomorphisms and tensor products are inconsistent," );
    fi;
    pi := TopProjection( T1 );
    elementarytensor1 := ElementaryTensorFunction( T1 );
    bastop := Basis( Range( pi ) );
    K := LeftActingDomain( T1 );
    factors := TensorProductFactors( T1 );
    M := factors[ 1 ];
    N := factors[ 2 ];
    dimM := Dimension( M );
    dimN := Dimension( N );
    basM := Basis( M );
    basN := Basis( N );
    VM := K^( dimM );
    VN := K^( dimN );
    basVM := Basis( VM );
    basVN := Basis( VN );
    mat := [];
    for v in basVM do
      for w in basVN do
        m := LinearCombination( basM, v );
        n := LinearCombination( basN, w );
        Add( mat, Coefficients( bastop, ImageElm( pi, elementarytensor1( m, n ) ) ) );
      od;
    od;
    gens := List( bastop, b -> SolutionMat( mat, Coefficients( bastop, b ) ) );
    generators := [];
    for gen in gens do
      temp := [];
      for i in [ 1..dimM * dimN ] do
        if i mod dimN <> 0 then 
          b := ( i mod dimN );
        else
          b := dimN;
        fi;
        if gen[ i ] <> Zero( gen[ i ] ) then 
          Add( temp, [ gen[ i ], basM[ Int( Ceil( 1.0 * i / dimN ) ) ], basN[ b ] ] );
        fi;
      od;
      Add( generators, temp );
    od;
    
    elementarytensor2 := ElementaryTensorFunction( T2 );
    newgenerators := [];
    images := [];
    for gen in generators do
      temp1 := Zero( T1 );
      temp2 := Zero( T2 );
      for m in gen do
        temp1 := temp1 + m[ 1 ] * elementarytensor1( m[ 2 ], m[ 3 ] );
        temp2 := temp2 + m[ 1 ] * elementarytensor2( ImageElm( f, m[ 2 ] ), ImageElm( g, m[ 3 ] ) );
      od;
      Add( newgenerators, temp1 );
      Add( images, temp2 );
    od;
        
    return QuiverRepresentationHomomorphismByImages( T1, T2, newgenerators, images );
end
  );


InstallMethod( TensorProductOfModules,
        "for two modules of (two) quivers",
        [ IsQuiverBimodule, IsQuiverBimodule ],
        function( M1, M2 )
    
    local   R1,  R2;

    R1 := UnderlyingRepresentation( M1 );
    R2 := UnderlyingRepresentation( M2 );
    
    return AsBimodule( TensorProductOfRepresentations( R1, R2 ) );
end
  );

InstallMethod( TensorProductOfModules,
        "for two modules of (two) quivers",
        [ IsRightQuiverModule, IsQuiverModule ],
        function( M1, M2 )
    
    return RestrictionToRight( TensorProductOfModules( RightModuleToBimodule( M1 ), M2 ) );
end
  );

InstallMethod( TensorProductOfModules,
        "for two modules of (two) quivers",
        [ IsQuiverModule, IsLeftQuiverModule ],
        function( M1, M2 )
    
    return RestrictionToLeft( TensorProductOfModules( M1, LeftModuleToBimodule( M2 ) ) );
end
  );

InstallMethod( TensorProductOfHomomorphisms,
        "for two homomorphisms of modules",
        [ IsQuiverBimoduleHomomorphism, IsQuiverBimoduleHomomorphism, IsQuiverBimodule, IsQuiverBimodule ],
        function( f, g, T1, T2 )
    
    local   rf,  rg,  RT1,  RT2;
    
    rf := UnderlyingRepresentationHomomorphism( f );
    rg := UnderlyingRepresentationHomomorphism( g );
    RT1 := UnderlyingRepresentation( T1 );
    RT2 := UnderlyingRepresentation( T2 );
    
    return AsBimoduleHomomorphism( TensorProductOfHomomorphisms( rf, rg, RT1, RT2 ) ); 
end
  );


InstallMethod( LeftTensorFunctor, 
        "for a quiver module and a quiver module category",
        [ IsQuiverBimodule, IsQuiverBimoduleCategory ], 
        function( M, AmodC )
    local   A,  B,  C,  BmodC,  tensor;
    
    A := RightActingAlgebra( M );
    B := LeftActingAlgebra( M );
    C := ActingAlgebras( AmodC )[ 2 ];
    if A <> ActingAlgebras( AmodC )[ 1 ] then
        Error( "Entered bimodule and category has incompatible structures,\n" );
    fi;
    BmodC := BimoduleCategory( B, C );
    tensor := CapFunctor( "LeftTensor", AmodC, BmodC );
    
    AddObjectFunction( tensor, X -> TensorProductOfModules( M, X ) ); 
    
    AddMorphismFunction( tensor, function( R1, f, R2 ) return TensorProductOfHomomorphisms( IdentityMorphism( M ), f, R1, R2 ); end );
    
    return tensor;
end
  );

InstallMethod( RightTensorFunctor, 
        "for a quiver module and a quiver module category",
        [ IsQuiverBimodule, IsQuiverBimoduleCategory ], 
        function( M, CmodA )
    local   A,  B,  C,  CmodB,  tensor;
    
    A := LeftActingAlgebra( M );
    B := RightActingAlgebra( M );
    C := ActingAlgebras( CmodA )[ 1 ];
    if A <> ActingAlgebras( CmodA )[ 2 ] then
        Error( "Entered bimodule and category has incompatible structures,\n" );
    fi;
    CmodB := BimoduleCategory( C, B );
    tensor := CapFunctor( "RightTensor", CmodA, CmodB );
    
    AddObjectFunction( tensor, X -> TensorProductOfModules( X, M ) ); 
    
    AddMorphismFunction( tensor, function( R1, f, R2 ) return TensorProductOfHomomorphisms( f, IdentityMorphism( M ), R1, R2 ); end );
    
    return tensor;
end
  );
