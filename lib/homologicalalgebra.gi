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

InstallTrueMethod( IsFiniteGlobalDimensionAlgebra, IsSemisimpleAlgebra );
InstallTrueMethod( IsFiniteGlobalDimensionAlgebra, IsPathAlgebra );

InstallTrueMethod( IsExceptionalRepresentation, IsIndecomposableRepresentation and IsRigidObject );
InstallTrueMethod( IsExceptionalModule, IsIndecomposableModule and IsRigidObject );

InstallMethod( ProjectiveCover, "for a quiver representation",
               [ IsQuiverRepresentation ],
function( R )
  local   mingen,  maps,  PR,  projections;

  if Sum( DimensionVector( R ) ) = 0 then
    return ZeroMorphism( ZeroObject( CapCategory( R ) ), R);
  else
    mingen := MinimalGeneratingSet( R );
    maps := List( mingen, x -> HomFromProjective( x, R ) );
    return UniversalMorphismFromDirectSum( maps );
  fi;
end
);

#######################################################################
##
#A  InjectiveEnvelope( <M> )
##
##  This function finds the injective envelope I(M) of the module  <M>  
##  in that it returns the map from M ---> I(M). 
##
InstallMethod ( InjectiveEnvelope, 
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ],
function( R );

    return DualOfRepresentationHomomorphism( ProjectiveCover( DualOfRepresentation( R ) ) );
end
  );

#######################################################################
##
#O  IsOmegaPeriodic( <R>, <n> )
##
##  This function tests if the object  <R>  is \Omega-periodic, that is,
##  if  R \simeq \Omega^i( R )  when  i  ranges over the set {1,2,...,n}.
##  Otherwise it returns false.
##
InstallMethod( IsOmegaPeriodic, 
"for a IsFieldCategoryObject and an integer",
[ IsFieldCategoryObject, IS_INT  ], 0,
function( R, n ) 

    local   cat,  res,  diffs,  i,  K;
    
    cat := CapCategory( R );
    res := AsChainComplex( ProjectiveResolution( R ) );
    diffs := Differentials( res ); 
    res := ChainComplex( cat, Replace( diffs, 0, [ ProjectiveCover( R ) ] ) );
    for i in [ 1..n ] do
        K := KernelObject( res^( i - 1 ) );
        if IsQuiverModule( R ) then 
            if IsomorphicModules( R, K ) then
                return i;
            fi;
        elif IsQuiverRepresentation( R ) then
            if IsomorphicRepresentations( R, K ) then
                return i;
            fi;
        else
            TryNextMethod( );
        fi;
    od;
    
    return false;
end
  );

#######################################################################
##
#P  IsRigidObject( <R> )
##
##  This function returns true if the entered object  <R>  is a rigid 
##  object, that is, no self extensions, otherwise false.
##
InstallMethod( IsRigidObject,
"for a IsFieldCategoryObject",
[ IsFieldCategoryObject ],
function( R ); 
    
  return Dimension( Ext( 1, R, R ) ) = 0;
end
  );

#######################################################################
##
#P  IsExceptionalModule( <M> )
##
##  This function returns true if the entered module  <M>  is an
##  exceptional module (ie. indecomposable and Ext^1(M,M)=(0), otherwise 
##  false, if the field, over which the algebra  <M>  is defined over, 
##  is finite.
##
InstallMethod( IsExceptionalModule,
"for a PathAlgebraMatModule",
[ IsQuiverModule ], 0,
function( M );
    
  return IsRigidObject( M ) and IsIndecomposableModule( M );
end
  );

#######################################################################
##
#P  IsExceptionalRepresentation( <R> )
##
##  This function returns true if the entered representation  <R>  is an
##  exceptional representation (ie. indecomposable and Ext^1(R,R)=(0), 
##  otherwise false, if the field, over which the algebra  <R>  is 
##  defined over, is finite.
##
InstallMethod( IsExceptionalRepresentation,
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ], 0,
    function( R );
    
    return IsRigidObject( R ) and IsIndecomposableRepresentation( R );
end
  );

#######################################################################
##
#O  NthSyzygy( <R>, <n> )
##
##  This functions computes the  <n>-th syzygy of the object  <R>  by 
##  first computing a projective resolution of  <R>, then it finds the
##  <n-1>-th differential and takes the kernel.
##
InstallMethod( NthSyzygy,
"for a IsFieldCategoryObject and a positive integer",
[ IsFieldCategoryObject, IS_INT ], 0,
function( R, n ) 

    local   cat,  res,  diffs;
   
    if n = 0 then 
        return R;
    fi;
    cat := CapCategory( R );
    res := AsChainComplex( ProjectiveResolution( R ) );
    diffs := Differentials( res ); 
    res := ChainComplex( cat, Replace( diffs, 0, [ ProjectiveCover( R ) ] ) );
    
    return KernelObject( res^( n - 1 ) );
end
  );

#######################################################################
##
#O  RightApproximationByAddR( <R>, <C> )
##
##  This function computes a right add<R>-approximation of the  
##  representation <C>, and the approximation is not necessarily minimal.
##
InstallMethod ( RightApproximationByAddR, 
"for two IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( R, C )

    local   K,  HomRC,  EndR,  radEndR,  radHomRC,  BHomRC,  i,  j,  
            g,  f,  B,  gens;

   if AlgebraOfRepresentation( R ) <> AlgebraOfRepresentation( C ) then
       Error(" the two representations entered into RightApproximationByAddR are not representations over the same algebra.\n");
   fi;
   if Dimension( C ) = 0 then
       return ZeroMorphism( ZeroObject( R ), C );
   fi;
   K := FieldOfRepresentation( R );
   HomRC := Hom( R, C );
   if Dimension( HomRC ) = 0 then 
       return ZeroMorphism( ZeroObject( R ), C );
   fi;
   
   EndR := EndomorphismAlgebra( R );
   radEndR := RadicalOfAlgebra( EndR );
   radEndR := BasisVectors( Basis( radEndR ) );
   radEndR := List( radEndR, x -> FromEndRToHomRR( R, x ) );
   radHomRC := [ ];
   BHomRC := BasisVectors( Basis( HomRC ) );
   for i in [ 1..Length( BHomRC ) ] do
       for j in [ 1..Length( radEndR ) ] do
           Add( radHomRC, PreCompose( radEndR[ j ], BHomRC[ i ] ) );
       od;
   od;
   g := SubspaceInclusion( HomRC, radHomRC );
   f := CokernelProjection( g );
   B := BasisVectors( Basis( Range( f ) ) );
   gens := List( B, x -> PreImagesRepresentative( f, x ) ); 
   
   return UniversalMorphismFromDirectSum( gens );
end
);

#######################################################################
##
#O   LeftApproximationByAddR( <C>, <R> )
##
##  This function computes a left add<M>-approximation of the module
##  <C>.
##
InstallMethod ( LeftApproximationByAddR,
"for two IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( C, R )

    local   K,  HomCR,  EndR,  radEndR,  radHomCR,  BHomCR,  i,  j,  
            g,  f,  B,  gens;
    
    if AlgebraOfRepresentation( R ) <> AlgebraOfRepresentation( C ) then
        Error(" the two representations entered into RightApproximationByAddR are not representations over the same algebra.\n");
    fi;
    if Dimension( C ) = 0 then
        return ZeroMorphism( C, ZeroObject( R ) );
    fi;
    K := FieldOfRepresentation( R );
    HomCR := Hom( C, R );
    if Dimension( HomCR ) = 0 then 
        return ZeroMorphism( C, ZeroObject( R ) );
    fi;
    
    EndR := EndomorphismAlgebra( R );
    radEndR := RadicalOfAlgebra( EndR );
    radEndR := BasisVectors( Basis( radEndR ) );
    radEndR := List( radEndR, x -> FromEndRToHomRR( R, x ) );
    radHomCR := [ ];
    BHomCR := BasisVectors( Basis( HomCR ) );
    for i in [ 1..Length( BHomCR ) ] do
        for j in [ 1..Length( radEndR ) ] do
            Add( radHomCR, PreCompose( BHomCR[ i ], radEndR[ j ] ) );
        od;
    od;
    g := SubspaceInclusion( HomCR, radHomCR );
    f := CokernelProjection( g );
    B := BasisVectors( Basis( Range( f ) ) );    
    gens := List( B, x -> PreImagesRepresentative( f, x ) ); 
    
    return UniversalMorphismIntoDirectSum( gens );
end
  );

#######################################################################
##
#O  MinimalRightApproximationByAddR( <R>, <C> )
##
##  This function computes the minimal right add<R>-approximation of the
##  representation  <C>.  
##
InstallMethod ( MinimalRightApproximationByAddR, 
"for two IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( R, C )

    local   f;
   
    f := RightApproximationByAddR( R, C );
   
   return RightMinimalVersion( f )[ 1 ];
end
);

#######################################################################
##
#O   MinimalLeftApproximationByAddR( <C>, <R> )
##
##  This function computes the minimal left add<R>-approximation of the
##  representation  <C>.  
##
InstallMethod ( MinimalLeftApproximationByAddR, 
"for two IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( C, R )

    local   f;
   
    f := LeftApproximationByAddR( C, R );
   
    return LeftMinimalVersion( f )[ 1 ];
end
  );
