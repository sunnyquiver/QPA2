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
#O  ProjDimensionOfObject( <M>, <n> )
##
##  Returns the projective dimension of the module  <M>  if it is less
##  or equal to  <n>, otherwise it returns false.
##  
InstallMethod ( ProjDimensionOfObject, 
"for a IsFieldCategoryObject",
[ IsFieldCategoryObject, IS_INT ], 
function( M, n )

    local   projres,  i;
    
    if HasProjDimension( M ) then
        if ProjDimension( M ) > n then
            return false;
        else
            return ProjDimension( M );
        fi;
    fi;
    projres := AsChainComplex( ProjectiveResolution( M ) );
    i := 1;
    while i < n + 2 do
        if Dimension( projres[ i ] ) = 0 then # the projective dimension is  i - 1.
            SetProjDimension( M, i - 1 );
            return i-1;
        fi;
        i := i + 1;
    od; 
    
    return false;
end 
  );

#######################################################################
##
#O  InjDimensionOfObject( <M>, <n> )
##
##  Returns the injective dimension of the module  <M>  if it is less
##  or equal to  <n>, otherwise it returns false.
##  
InstallMethod ( InjDimensionOfObject, 
"for a IsFieldCategoryObject",
[ IsFieldCategoryObject, IS_INT ], 
function( M, n )

    local   injres,  i;
    
    if HasInjDimension( M ) then
        if InjDimension( M ) > n then
            return false;
        else
            return InjDimension( M );
        fi;
    fi;
    injres := AsChainComplex( InjectiveResolution( M ) );
    i := 1;
    while i < n + 2 do
        if Dimension( injres[ -i ] ) = 0 then # the injective dimension is  i - 1.
            SetInjDimension( M, i - 1 );
            return i-1;
        fi;
        i := i + 1;
    od; 
    
    return false;
end 
  );

InstallTrueMethod( IsExceptionalRepresentation, IsIndecomposableRepresentation and IsRigidObject );
InstallTrueMethod( IsExceptionalModule, IsIndecomposableModule and IsRigidObject );

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
#O  GlobalDimensionOfAlgebra( <A>, <n> )
##
##  Returns the global dimension of the algebra  <A>  if it is less or
##  equal to  <n>, otherwise it returns false.
##  
InstallMethod ( GlobalDimensionOfAlgebra, 
"for a finite dimensional quotient of a path algebra",
[ IsQuiverAlgebra, IS_INT ], 
function( A, n )

    local   simples,  dimension,  S,  pd;
    
    if not IsFiniteDimensional(A) then
        TryNextMethod();
    fi;
    if HasGlobalDimension( A ) then
        return GlobalDimension( A );
    fi;
    if not IsAdmissibleQuiverAlgebra( A ) then
        TryNextMethod( );
    fi;
    if Trace( AdjacencyMatrixOfQuiver( QuiverOfAlgebra( A ) ) ) > 0 then 
        SetGlobalDimension( A, infinity );
        return infinity;
    fi;
    simples := SimpleRepresentations( A );
    dimension := 0;
    for S in simples do
        pd := ProjDimensionOfObject( S, n ); 
        if pd = false then
            return false;
        fi;
        dimension := Maximum( dimension, pd );
    od;
    
    SetGlobalDimension( A, dimension );
    return dimension;
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

#######################################################################
##
#O  DominantDimensionOfModule( <R>, <n> )
##
##  Returns the dominant dimension of the representation  <R>  if it 
##  is less or equal to  <n>. If the representation  <R>  is injectiv 
##  and projective, then it returns infinity. Otherwise it returns false.
##  
InstallMethod ( DominantDimensionOfRepresentation, 
"for a IsQuiverRepresentation and an integer",
[ IsQuiverRepresentation, IS_INT ], 
function( R, n )

    local   A,  Rop,  resop,  dimension,  j;
    
    A := AlgebraOfRepresentation( R );
    #
    # If the algebra  A  is not a finite dimensional, try another method.
    #
    if not IsFiniteDimensional( A ) then
        TryNextMethod();
    fi;
    # 
    # If the representation  <R>  is injective and projective, then the 
    # dominant dimension of <R>  is infinite.
    if IsInjectiveRepresentation( R ) and IsProjectiveRepresentation( R ) then 
        return infinity;
    fi;
    Rop := DualOfRepresentation( R );
    #
    # Compute the minimal projective resolution of the module  Rop.
    #
    resop := AsChainComplex( ProjectiveResolution( Rop ) );
    dimension := 0;
    # Check how far out the modules in the minimal projective resolution of Rop
    # is injective.
    if IsInjectiveRepresentation( resop[ 0 ] ) then
        if Dimension( resop[ 1 ] ) = 0 then
            return infinity;
        else
            j := 1;
            while ( j < n + 1 ) and ( IsInjectiveRepresentation( resop[ j ] ) ) do 
                    j := j + 1;
            od; 
            if ( j < n + 1 ) then # if this happens, then j-th projective is not injective and domdim equal to j for <R>.
                dimension := Maximum( dimension, j );
            else                    # if this happens, then j = n + 1. 
                if not IsInjectiveRepresentation( resop[ n + 1 ] ) then # then domdim is  n  of  <R>.
                    dimension := Maximum( dimension, n );
                else                                                      # then domdim is > n  for  <R>.
                    return false;
                fi;
            fi;
        fi;
    fi;
    SetDominantDimension( R, dimension );
    
    return dimension;
end 
);

#######################################################################
##
#O  DominantDimensionOfAlgebra( <A>, <n> )
##
##  Returns the dominant dimension of the algebra  <A>  if it is less or
##  equal to  <n>. If the algebra  <A>  is selfinjectiv, then it returns
##  infinity. Otherwise it returns false.
##  
InstallMethod ( DominantDimensionOfAlgebra, 
    "for a finite dimensional quotient of a path algebra",
    true,
    [ IsQuiverAlgebra, IS_INT ], 
    0,
    function( A, n )

    local   P,  domdimlist,  M,  test,  pos,  finite_ones;
    
    #
    # If the algebra  <A>  is not a finite dimensional, try another method.
    #
    if not IsFiniteDimensional( A ) then
        TryNextMethod();
    fi;
    # 
    # If the algebra  <A>  is selfinjective, then the dominant dimension of 
    # <A>  is infinite.
    if IsSelfinjectiveAlgebra( A ) then 
        return infinity;
    fi;
    P := IndecProjRepresentations( A );
    domdimlist := [];
    for M in P do
        test := DominantDimensionOfRepresentation( M, n );   # Checking the dominant dimension of each indec. projective representation. 
        if test = false then
            return false;
        fi;
        Add( domdimlist, test ); 
    od; 
    pos := Positions( domdimlist, infinity ); # positions of the indec. projective modules with infinite domdim.
    finite_ones := [ 1..Length( P ) ]; 
    SubtractSet( finite_ones, pos ); # positions of the indec. projective modules with finite domdim.
    
    return Minimum( domdimlist{ finite_ones } );
end
  );

#######################################################################
##
#O  GorensteinDimensionOfAlgebra( <A>, <n> )
##
##  Returns the Gorenstein dimension of the algebra  <A>  if it is less
##  or equal to  <n>, otherwise it returns false.
##  
InstallMethod ( GorensteinDimensionOfAlgebra,
"for a quiver algebra",
[ IsQuiverAlgebra, IS_INT ], 
function( A, n )

    local Ilist, dimension_right, I, projdim, Ilistop, dimension_left;
    
    #
    #  If  <A>  has finite global dimension, then the Gorenstein 
    #  dimension of  <A>  is equal to the global dimension of  <A>. 
    #
    if HasGlobalDimension( A ) then
        if GlobalDimension( A ) <> infinity then 
            SetGorensteinDimension( A, GlobalDimension( A ) );
            return GlobalDimension( A );
        fi;
    fi;
    if HasGorensteinDimension( A ) then
        return GorensteinDimension( A );
    fi;
    #
    #  First checking the injective dimension of the indecomposable
    #  projective left  <A>-modules.
    #
    Ilist := IndecInjRepresentations( A );
    dimension_right := 0;
    for I in Ilist do
        projdim := ProjDimensionOfObject( I, n ); 
        if projdim = false then   # projective dimension of  I  is bigger than  n.
            return false;
        else                      # projective dimension of  I  is less or equal to  n.
            dimension_right := Maximum( dimension_right, projdim );
        fi;
    od;
    #
    #  Secondly checking the injective dimension of the indecomposable
    #  projective right  <A>-modules.
    #  
    Ilistop := IndecInjRepresentations( OppositeAlgebra( A ) );    
    dimension_left := 0;
    for I in Ilistop do
        projdim := ProjDimensionOfObject( I, n ); 
        if projdim = false then   # projective dimension of  I  is bigger than  n.
            return false;
        else                      # projective dimension of  I  is less or equal to  n.
            dimension_left := Maximum( dimension_left, projdim );
        fi;
    od;    
    if dimension_left <> dimension_right then
        Print( "You have a counterexample to the Gorenstein conjecture!\n\n" );
    fi;
    
    SetGorensteinDimension( A, Maximum( dimension_left, dimension_right ) );
    return Maximum( dimension_left, dimension_right );    
end 
  );

#######################################################################
##
#A  FaithfulDimension( <R> )
##
##  This function computes the faithful dimension of a representation  
##  <R>.
##
InstallMethod( FaithfulDimension,
"for a IsQuiverRepresentation and an integer",
[ IsQuiverRepresentation ],
    
    function( R )
    local P, lengths, p, tempdim, U, g, f;
    
    P := IndecProjRepresentations( AlgebraOfRepresentation( R ) );
    # 
    # For all indecomposable projective  A-representations computing successive 
    # minimal left add<R>-approximation to produce find the faithful
    # dimension  <R>  has with respect to that indecomposable 
    # projective. 
    #
    lengths := [ ];
    for p in P do
        tempdim := 0; 
        U := p;
        f := MinimalLeftApproximationByAddR( U, R );
	if not IsMonomorphism( f ) then
	   return 0;
        fi;
        while IsMonomorphism( f ) do 
            tempdim := tempdim + 1;
            g := CokernelProjection( f );
            if Dimension( Range( g ) ) = 0 then
                Add( lengths, infinity );
                break;
            else
                f := MinimalLeftApproximationByAddR( Range( g ), R ); 
            fi;
        od;
        if Dimension( Range( g ) ) <> 0 then 
            Add( lengths, tempdim );
        fi;
    od; 

    return Minimum( lengths );
end
  );

#######################################################################
##
#O  IsN_RigidObject( <M>, <n> )
##
##  Returns true if the object  <M>  is n-rigid, otherwise false.
##  
InstallMethod ( IsN_RigidObject, 
"for a IsFieldCategoryObject",
[ IsFieldCategoryObject, IS_INT ], 
function( M, n )

  local   i;
  
  i := 1;
  while i < n + 1 do
      if Dimension( Ext( i, M, M ) ) > 0 then
          return false;
      else
          i := i + 1;
      fi;
  od;
  
  return true;
end 
  );

#######################################################################
##
#P  IsProjectiveRepresentation( <R> )
##
##  Checks whether <R> is projective.
##
InstallMethod( IsProjectiveRepresentation, 
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ],
function( R ) 

   local top, dimension, i, P; 

   top := TopOfRepresentation( R ); 
   dimension := 0; 
   P := IndecProjRepresentations( AlgebraOfRepresentation( R ) ); 
   for i in [ 1..Length( DimensionVector( R ) ) ] do 
      if DimensionVector( top )[ i ] <> 0 then 
         dimension := dimension + Dimension( P[ i ] ) * DimensionVector( top )[ i ];
      fi;
   od; 

   if dimension = Dimension( R ) then 
      return true;
   else 
      return false;
   fi;
end
  );

#######################################################################
##
#P  IsProjectiveModule( <M> )
##
##  Checks whether <M> is projective.
##
InstallMethod( IsProjectiveModule, 
"for a IsQuiverModule",
[ IsQuiverModule ],
function( M ) 

  local   R;

  R := UnderlyingRepresentation( M );
   
  return IsProjectiveRepresentation( R );
end
  );

#######################################################################
##
#P  IsInjectiveRepresentation( <R> )
##
##  Checks whether <R> is injective.
##
InstallMethod( IsInjectiveRepresentation, 
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ],
function( R ); 

   return IsProjectiveRepresentation( DualOfRepresentation( R) );
end
  );

InstallMethod( IsNthSyzygy, 
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation, IS_INT ], 
function( R, n )

    local N;
    
    N := NthSyzygy( DualOfModule( NthSyzygy( DualOfRepresentation( R ), n ) ), n );
    N := DirectSum( [ N, Source( ProjectiveCover( R ) ) ] );
    
    return IsDirectSummand( R, N ); 
end
  );

#######################################################################
##
#O  TiltingRepresentation( <R>, <n> )
##
##  This function checks if the representation  <R>  is a tilting 
##  representation of projective dimension at most  <n>.
##
InstallMethod( TiltingRepresentation,
"for IsQuiverRepresentation and a positive integer",
[ IsQuiverRepresentation, IS_INT ],
function( R, n )
    
    local m, N, i, P, t, coresolution, temp;
    #
    # Checking if the representation has projective dimension at most  <n>.
    #
    if HasProjDimension( R ) then 
        if ProjDimension( R ) > n then
            return false;
        fi;
    else
        if not IS_INT( ProjDimensionOfObject( R, n ) ) then
            return false;
        fi;
    fi;
    #
    # Now we know that the projective dimension of  <R>  is at most  <n>.
    # Next we check if the module  <R>  is selforthogonal. 
    # 
    m := ProjDimension( R ); 
    N := R;
    i := 1;
    repeat 
        if Dimension( Ext( i, N, R ) ) <> 0 then
            return false;
        fi;
        i := i + 1;
    until i = m + 1;
    #
    # Now we know that the projective dimension of  <R>  is at most  <n>, 
    # and that the representation  <R>  is selforthogonal.  Next we check if all the 
    # indecomposable projectives can be coresolved in  add<R>.
    #
    P := IndecProjRepresentations( AlgebraOfRepresentation( R ) );
    t := Length( P );
    coresolution := [];
    for i in [1..t] do
        temp := FiniteCoresolutionInAddR( P[ i ], R, m); 
        if temp = false then
            return false;
        fi;
        Add( coresolution, temp );
    od;
    #
    # Now we know that the representation  <R>  is a tilting representation of projective 
    # dimension  m. 
    # 
    SetIsTiltingObject( R, true );
    return [ m, coresolution ];
end
  );

#######################################################################
##
#O  CotiltingRepresentation( <R>, <n> )
##
##  This function checks if the representation  <R>  is a cotilting 
##  representation of injective dimension at most  <n>.
##
InstallMethod( CotiltingRepresentation,
"for a IsQuiverRepresentation and a positive integer",
[ IsQuiverRepresentation, IS_INT ],
    
    function( R, n )
    local m, N, i, I, t, resolution, temp;
    #
    # Checking if the module has injective dimension at most  <n>.
    #
    if HasInjDimension( R ) then 
        if InjDimension( R ) > n then
            return false;
        fi;
    else
        if not IS_INT( InjDimensionOfObject( R, n ) ) then
            return false;
        fi;
    fi;
    #
    # Now we know that the injective dimension of  <M>  is at most  <n>.
    # Next we check if the module  <M>  is selforthogonal. 
    # 
    m := InjDimension( R ); 
    N := R;
    i := 1;
    repeat 
        if Dimension( Ext(i, N, R ) ) <> 0 then
	    SetIsCotiltingObject( R, false );
            return false;
        fi;
        i := i + 1;
    until i = m + 1;
    #
    # Now we know that the injective dimension of  <R>  is at most  <n>, 
    # and that the module  <R>  is selforthogonal.  Next we check if all the 
    # indecomposable injectives can be resolved in  add<R>.
    #
    I := IndecInjRepresentations( AlgebraOfRepresentation( R ) );
    t := Length( I );
    resolution := [ ];
    for i in [ 1..t ] do
        temp := FiniteResolutionInAddR( I[ i ], R, m ); 
        if temp = false then
            return false;
        fi;
        Add( resolution, temp );
    od;
    #
    # Now we know that the representation  <R>  is a cotilting representation of injective 
    # dimension  <R>. 
    # 
    SetIsCotiltingObject( R, true );    
    return [ m, resolution ];
end
  );

#######################################################################
##
#O  LeftApproximationByFacR( <C>, <M> )
##
##  This function computes a left, not necessarily left minimal, 
##  Fac<R>-approximation of the representation  <C>  by doing the following:
##                  p
##           P(C) -------> C  (projective cover)
##             |           |
##           f |           | g - the returned homomorphism
##             V           V
##          M^{P(C)} ----> E
##
InstallMethod( LeftApproximationByFacR,
"for IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( C, R )

    local p, f; 
    #
    # Checking if  <C>  and  <R>  are representations over the same algebra.
    #
    if AlgebraOfRepresentation( C ) <> AlgebraOfRepresentation( R ) then
        Error("the entered representations are not over the same algebra,\n");
    fi;    
    p := ProjectiveCover( C );
    f := LeftApproximationByAddR( Source( p ), R );
    
    return Pushout( [ p, f ] )[2][ 1 ];
end
  );

#######################################################################
##
#O  MinimalLeftApproximationByFacR( <C>, <R> )
##
##  This function computes a minimal left Fac<M>-approximation of the 
##  representation  <C>. 
##
InstallMethod( MinimalLeftApproximationByFacR,
"for IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( C, R );

    return LeftMinimalVersion( LeftApproximationByFacR( C, R ) )[ 1 ];
end
  );

#######################################################################
##
#O  RightApproximationBySubR( <R>, <C> )
##
##  This function computes a right, not necessarily a right minimal,
##  Sub<M>-approximation of the module  <C>  by doing the following:
##
##             E -----> M^{I(C)}  (minimal right Add<M>-approximation)
##             |           |
##           g |           | f     g - the returned homomorphism
##             V    i      V
##             C -------> I(C)  (injective envelope)
##
InstallMethod( RightApproximationBySubR,
"for IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( R, C )

    local iop, i, f; 
    #
    # Checking if  <R>  and  <C>  are representations over the same algebra.
    #
    if AlgebraOfRepresentation( C ) <> AlgebraOfRepresentation( R ) then
        Error("the entered representations are not over the same algebra,\n");
    fi;     
    iop := ProjectiveCover( DualOfRepresentation( C ) );
    i := DualOfRepresentationHomomorphism( iop );
    f := RightApproximationByAddR( R, Range( i ) );
    
    return ProjectionInFactorOfFiberProduct( [ i, f ], 1 );
end
  );


#######################################################################
##
#O  MinimalRightApproximationSubR( <R>, <C> )
##
##  This function computes a minimal right Sub<R>-approximation of the
##  representation <C>. 
##
InstallMethod( MinimalRightApproximationBySubR,
"for IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( R, C );

    return RightMinimalVersion( RightApproximationBySubR( R, C ) )[ 1 ];
end
  );


#######################################################################
##
#O  LeftApproximationBySubR( <C>, <R> )
##
##  This function computes a left Sub<R>-approximation of the 
##  representation  <C>  by doing the following: 
##            f
##       C -------> R(C) = a left Add<R>-approxiamtion
##
##   Returns the natural projection  C --------> Im( f ). 
##
InstallMethod( LeftApproximationBySubR,
"for IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( C, R )

    local f; 
    #
    # Checking if <C>  and  <R>  are representations over the same algebra.
    #
    if AlgebraOfRepresentation( C ) <> AlgebraOfRepresentation( R ) then
        Error("the entered representations are not over the same algebra,\n");
    fi;       
    f := LeftApproximationByAddR( C, R );
    
    return CoastrictionToImage( f );
end
  );

#######################################################################
##
#O  MinimalLeftApproximationSubR( <R>, <C> )
##
##  This function computes a minimal left Sub<R>-approximation of the
##  representation <C>. 
##
InstallMethod( MinimalLeftApproximationBySubR,
"for IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( R, C );

    return LeftMinimalVersion( LeftApproximationBySubR( R, C ) )[ 1 ];
end
  );

#######################################################################
##
#O  FiniteCoresolutionInAddR( <N>, <R>, <n> )
##
##  This function checks if the module  <N>  has a finite coresolution
##  in  add<R>  of length at most  <n>.  If it does, then this 
##  coresoultion is returned, otherwise false is returned. 
##
InstallMethod( FiniteCoresolutionInAddR,
"for two IsQuiverRepresentation and a positive integer",
[ IsQuiverRepresentation, IsQuiverRepresentation, IS_INT ],
function( N, R, n )
    
    local   U,  differentials,  g,  f,  i,  cat,  coresolution;
    #
    # Checking if  <N>  and  <R>  are representations over the same algebra.
    #
    if AlgebraOfRepresentation( R ) <> AlgebraOfRepresentation( N ) then
        Error("the entered representations are not over the same algebra,\n");
    fi;
    #
    # If n = 0, then this is the same as N being in add R.
    # 
    # Computing successive minimal left add<R>-approximation to produce
    # a coresolution of  <N>  in  add<R>.
    #
    U := N;
    differentials := [ ];
    g := IdentityMorphism( U );
    f := MinimalLeftApproximationByAddR( U, R );
    if n = 0 then 
      if IsIsomorphism( f ) then 
        return true;
      else
        return false;
      fi;
    fi;
    for i in [ 0..n ] do
        if not IsMonomorphism( f ) then 
            return false;
        fi;
        Add( differentials, PreCompose( g, f ) ); 
        g := CokernelProjection( f );
        if Dimension( Range( g ) ) = 0 then
            break;
        fi;
        f := MinimalLeftApproximationByAddR( Range( g ), R ); 
    od;
    differentials := Reversed( differentials );
    cat := CapCategory( R );
#    coresolution := CochainComplex( cat, differentials );
    
    return differentials;
end
  );

#######################################################################
##
#O  FiniteResolutionInAddR( <N>, <M>, <n> )
##
##  This function checks if the representation  <N>  has a finite resolution
##  in  add<R>  of length at most  <n>.  If it does, then this 
##  resoultion is returned, otherwise false is returned. 
##
InstallMethod( FiniteResolutionInAddR,
"for two IsQuiverRepresentation and a positive integer",
[ IsQuiverRepresentation, IsQuiverRepresentation, IS_INT ],
    
    function( N, R, n )
    local U, differentials, g, f, i, cat, resolution;
    #
    # Checking if  <N>  and  <R>  are modules over the same algebra.
    #
    if AlgebraOfRepresentation( R ) <> AlgebraOfRepresentation( N ) then
        Error("the entered representations are not over the same algebra,\n");
    fi;
    cat := CapCategory( R );
    if IsZero( N ) then
        differentials := [ ZeroMorphism( N, N ) ];
        
        return CochainComplex( cat, differentials );
    fi;
    if Dimension( Hom( R, N ) ) = 0  then 
        return false;
    fi;
    #
    # If n = 0, then this is the same as N being in add R.
    # 
    # Computing successive minimal right add<M>-approximation to produce
    # a resolution of  <N>  in  add<R>.
    #
    U := N;    
    differentials := [ ];
    g := IdentityMorphism( U );
    f := MinimalRightApproximationByAddR( R, U );
    if n = 0 then 
      if IsIsomorphism( f ) then 
        return true;
      else
        return false;
      fi;
    fi;
    for i in [ 0..n ] do
        if not IsEpimorphism( f ) then
            return false;
        fi;
        Add( differentials, PreCompose( f, g ) );
        g := KernelEmbedding( f );
        if Dimension( Source( g ) ) = 0 then
            break;
        fi;
        f := MinimalRightApproximationByAddR( R, Source( g ) );
    od;
    differentials := Reversed( differentials );
#    resolution := CochainComplex( cat, differentials );
    
    return differentials;
end
  );

#######################################################################
##
#O  RightApproximationByPerpT( <T>, <R> )
##
##  Returns the minimal rightt $\widehat{\add T}$-approximation of the 
##  representation  <R>.  It checks if  <T>  is a cotilting module, and if not
##  it returns an error message. 
## 
InstallMethod ( RightApproximationByPerpT, 
"for a cotilting IsQuiverRepresentation and a IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( T, R )

    local   n,  projres,  exactsequences,  currentsequences,  f,  i,  g,  
            h,  alpha,  t, fprime;
    
    if not IsCotiltingObject( T ) then
        Error("the first argument is not a cotilting object,\n");
    fi;
    if IsZero( R ) then
      return ZeroMorphism( ZeroModule( AlgebraOfRepresentation( R ) ), R );
    fi;
    n := InjDimension( T );
    projres := AsChainComplex( ProjectiveResolution( R ) );    
    exactsequences := List( [ 0..n - 1 ], i -> [ KernelEmbedding( projres^i ), 
                              CoastrictionToImage( projres^i ) ] ); 
    f := MinimalLeftApproximationByAddR( Source( exactsequences[ n ][ 1 ] ), T );    
    for i in [ 1..n ] do
      g := Pushout( exactsequences[ n + 1 - i ][ 1 ], f )[ 1 ];
      h := CokernelProjection( g );
      alpha := IsomorphismOfRepresentations( Range( h ), Range( exactsequences[ n + 1 - i ][ 2 ] ) );
      h := h * alpha;
      if i < n then
        fprime := MinimalLeftApproximationByAddR( Source( h ), T );
        f := Pushout( fprime, h )[ 1 ]; 
      fi;
    od;
    
    return RightMinimalVersion( h )[ 1 ];
end
  );

#######################################################################
##
#O  LeftApproximationByAddTHat( <T>, <R> )
##
##  Returns the minimal left $\widehat{\add T}$-approximation of the 
##  representation  <R>.  It checks if  <T>  is a cotilting module, and if not
##  it returns an error message. 
## 
InstallMethod ( LeftApproximationByAddTHat, 
"for a cotilting IsQuiverRepresentation and a IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( T, R )

    local   n,  projres,  exactsequences,  currentsequences,  f,  i,  g,  
            h,  alpha,  t, fprime;
    
    if not IsCotiltingObject( T ) then
        Error( "the first argument is not a cotilting representation,\n" );
    fi;
    if IsZero( R ) then
      return ZeroMorphism( R, R );
    fi;
    n := InjDimension( T );
    projres := AsChainComplex( ProjectiveResolution( R ) );
    exactsequences := List( [ 0..n - 1 ], i -> [ KernelEmbedding( projres^i ), 
                              CoastrictionToImage( projres^i ) ] ); 
    f := MinimalLeftApproximationByAddR( Source( exactsequences[ n ][ 1 ] ), T );    
    for i in [ 1..n ] do
      g := Pushout( exactsequences[ n + 1 - i ][ 1 ], f )[ 1 ];
      h := CokernelProjection( g );
      alpha := IsomorphismOfRepresentations( Range( h ), Range( exactsequences[ n + 1 - i ][ 2 ] ) );
      h := PreCompose( h, alpha );
      fprime := MinimalLeftApproximationByAddR( Source( h ), T );
      f := Pushout( fprime, h )[ 1 ]; 
    od;
        
    return LeftMinimalVersion( f )[ 1 ];
end
  );

#######################################################################
##
#O  LeftMutationOfTiltingRepresentationComplement( <R>, <N> )
##
##  This function computes the left mutation of a complement  <N>  of
##  an almost complete tilting representation  <R>, assuming that  <R>
##  is an almost complete tilting module.  If it doesn't exist, then the
##  function returns false.
##
InstallMethod( LeftMutationOfTiltingRepresentationComplement,
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( R, N )
    
    local f;
    
    f := MinimalLeftApproximationByAddR( N, R );
    if IsMonomorphism( f ) then
        return CokernelObject( f );
    else
        return false;
    fi;
end
  );

DeclareSynonym( "LeftMutationOfCotiltingRepresentationComplement", LeftMutationOfTiltingRepresentationComplement );

#######################################################################
##
#O  RightMutationOfTiltingRepresentationComplement( <R>, <N> )
##
##  This function computes the right mutation of a complement  <N>  of
##  an almost complete tilting representation  <R>, assuming that  <R>  
##  is an almost complete tilting representation.  If it doesn't exist, 
##  then the function returns false.
##
InstallMethod( RightMutationOfTiltingRepresentationComplement,
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( R, N )
    
    local f;
    
    f := MinimalRightApproximationByAddR( R, N );
    if IsEpimorphism( f ) then
        return Kernel( f );
    else
        return false;
    fi;
end
  );

DeclareSynonym( "RightMutationOfCotiltingRepresentationComplement", RightMutationOfTiltingRepresentationComplement );

#######################################################################
##
#O  AllComplementsOfAlmostCompleteTiltingRepresentation( <R>, <X> )
##
##  This function constructs all complements of an almost complete 
##  tilting representation  <R>  given a complement  <X>  of  <R>.  The 
##  complements are returned as a long exact sequence (whenever possible)
##  of minimal left and minimal right  add<R>-approximations.
##
InstallMethod( AllComplementsOfAlmostCompleteTiltingRepresentation, 
"for two IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( R, X)
    
    local   U,  leftdifferentials,  g,  f,  cat,  resolution,  
            rightdifferentials,  coresolution;
    #
    # Checking if  <R>  and  <X>  are modules over the same algebra.
    #
    if AlgebraOfRepresentation( R ) <> AlgebraOfRepresentation( X ) then
        Error( "the entered representations are not over the same algebra,\n" );
    fi;
    # 
    # Computing successive minimal right add<R>-approximation to produce
    # a resolution of  <X>  in  add<R>.
    #
    U := X;
    leftdifferentials := [];
    g := IdentityMorphism( U );
    f := MinimalRightApproximationByAddR( R, U );
    while IsEpimorphism( f ) do
        Add( leftdifferentials, PreCompose( f, g ) );
        g := KernelEmbedding( f );
        if Dimension( Source( g ) ) = 0 then
            break;
        fi;
        f := MinimalRightApproximationByAddR( R, Source( g ) );
    od;
    cat := CapCategory( R );
    if Length( leftdifferentials ) = 0 then
        resolution := [];
    else
        Add( leftdifferentials, KernelEmbedding( leftdifferentials[ Length( leftdifferentials ) ] ) );         
#        resolution := FiniteComplex( cat, 1, leftdifferentials );
        resolution := leftdifferentials;
    fi;
    # 
    # Computing successive minimal left add<R>-approximation to produce
    # a coresolution of  <X>  in  add<R>.
    #
    U := X;
    rightdifferentials := [ ];
    g := IdentityMorphism(U);
    f := MinimalLeftApproximationByAddR( U, R );    
    while IsMonomorphism( f ) do
        Add( rightdifferentials, PreCompose( g, f ) ); 
        g := CokernelProjection( f );
        f := MinimalLeftApproximationByAddR( Range( g ), R ); 
    od;

    if Length( rightdifferentials ) = 0 then
        coresolution := [ ];
    else
        Add( rightdifferentials, CokernelProjection( rightdifferentials[ Length( rightdifferentials ) ] ) ); 
        rightdifferentials := Reversed( rightdifferentials );
#        coresolution := FiniteComplex(cat, -Length( rightdifferentials ) + 1, rightdifferentials );    
        coresolution := rightdifferentials;        
    fi;
    
    return [ resolution,coresolution ];
end
  );

DeclareSynonym( "AllComplementsOfAlmostCompleteCotiltingRepresentation", AllComplementsOfAlmostCompleteTiltingRepresentation );

#######################################################################
##
#O  NumberOfComplementsOfAlmostCompleteTiltingRepresentation( <R> )
##
##  This function computes the number complements of an almost 
##  complete tilting/cotilting representation  <R>, assuming that  <R>
##  is an almost complete tilting representation.
##
InstallMethod( NumberOfComplementsOfAlmostCompleteTiltingRepresentation,
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ],
function( R );
    
    return FaithfulDimension( R ) + 1; 
end
  );

DeclareSynonym( "NumberOfComplementsOfAlmostCompleteCotiltingRepresentation", NumberOfComplementsOfAlmostCompleteTiltingRepresentation );

#######################################################################
##
#O  PartialIyamaGenerator( <R> )
##
##  Given a representation  <R>  this function returns the 
##  subrepresentation of  <R> given by the radical of the endomorphism 
##  ring of  <R>  times <R>.  If  <R>  is zero, then  <R>  is returned.
##
InstallMethod( PartialIyamaGenerator,
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ],
function( R ) 

    local B, EndR, radEndR, Brad, subgens, b, r; 

    if Dimension( R ) = 0 then 
        return R;
    fi;
    B := BasisVectors( Basis( R ) ); 
    EndR := EndomorphismAlgebra( R );
    radEndR := RadicalOfAlgebra( EndR );  
    Brad := BasisVectors( Basis( radEndR ) );
    Brad := List( Brad, x -> FromEndRToHomRR( R, x ) );
    subgens := [ ];
    for b in B do
        for r in Brad do
            Add( subgens, ImageElm( r, b ) );
        od;
    od;

    return Source( SubrepresentationInclusion( R, subgens ) );
end
);

#######################################################################
##
#O  IyamaGenerator( <R> )
##
##  Given a representation  <R> this function returns a representation 
##  S  such that  <R>  is a direct summand of  S  and such that the 
##  global dimension of the endomorphism ring of  S  is finite. 
##
InstallMethod( IyamaGenerator,
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ],
function( R ) 

    local   iyamagen,  N,  S,  L;

    iyamagen := [];
    N := R;
    repeat 
        Add( iyamagen, N );
        N := PartialIyamaGenerator( N );
    until
        Dimension( N ) = 0;

    S := DirectSum( iyamagen );
    if IsFinite( FieldOfRepresentation( R ) ) then 
        L := DecomposeRepresentationWithMultiplicities( S );
        S := DirectSum( L[ 1 ] );
    fi;

    return S;
end
  );

#######################################################################
##
#O  RejectOfRepresentation( <N>, <R> )
##
##  This function computes the reject of the module  <R>  in  <N> by doing 
##  the following: computes a basis for Hom(N,R) and then computes 
##  the intersections of all the kernels of the elements in this basis. 
##
InstallMethod( RejectOfRepresentation,
"for two IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( N, R )

    local  homNR;
    #
    # Checking if the representations  <R>  and  <N>  are over the same algebra.
    #
    if AlgebraOfRepresentation(R) <> AlgebraOfRepresentation(N) then
        Error("the entered representations are not over the same algebra,\n");
    fi;
    #
    # If  Hom( N, R )  is zero, then the reject is the identity homomorphism N ---> N.
    #
    homNR := Hom( N, R );
    if Dimension( homNR ) = 0 then 
        return IdentityMorphism( N );
    fi;
    #
    # Using the basis we found for  Hom( N, R )  above, and taking the intersection of 
    # all the kernels of these homomorphisms in the basis.  This is the reject of  
    # <R>  in  <N>.
    #
    homNR := BasisVectors( Basis( homNR ) );
    if Length( homNR ) = 1 then
       return KernelEmbedding( homNR[ 1 ] );
    fi;
    if Length( homNR ) > 1 then
	return IntersectionOfRepresentations( List( homNR, f -> KernelEmbedding( f ) ) );
    fi;
end
  );

#######################################################################
##
#O  TraceOfRepresentation( <R>, <N> )
##
##  This function computes trace of the module  <R>  in  <N> by doing 
##  the following: computes a basis for Hom_A(R,N) and then computes 
##  the sum of all the images of the elements in this basis. This is 
##  also a minimal right Fac(R)-approximation. 
##
InstallMethod( TraceOfRepresentation,
"for two IsQuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation],
function( R, N )
    
    local homRN, B, trace;
    #
    # Checking if the representations  <R>  and  <N>  are over the same algebra.
    #
    if AlgebraOfRepresentation( R ) <> AlgebraOfRepresentation( N ) then
        Error( "the entered modules are not modules over the same algebra,\n" );
    fi;
    #
    # If  <R>  or  <N>  are zero, then the trace is the zero module.
    #
    if R = ZeroRepresentation( AlgebraOfRepresentation( R ) ) or 
       N = ZeroRepresentation( AlgebraOfRepresentation( N ) ) then
        return ZeroMorphism( ZeroRepresentation( AlgebraOfRepresentation( N ) ), N );
    fi;
    #
    # Finding a basis for  Hom(R,N), and taking the sum of all the images of these
    # homomorphisms from  <R>  to  <N>.  This is the trace of  <R>  in  <N>. 
    #
    homRN := BasisVectors( Basis( Hom( R,N ) ) );
    if Length( homRN ) = 0 then
        return ZeroMorphism( ZeroRepresentation( AlgebraOfRepresentation( N ) ), N );
    fi;
    B := BasisVectors( Basis( R ) );
    trace := Flat(List(B, b -> List( homRN, f -> ImageElm( f, b ) ) ) ) ;
    
    return SubrepresentationInclusion( N, trace );
end
  );