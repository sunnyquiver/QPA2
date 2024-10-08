#######################################################################
##
#O  CommonDirectSummand( <R1>, <R2> )
##
##  This function is using the algorithm for finding a common direct 
##  summand presented in the paper "Gauss-Elimination und der groesste
##  gemeinsame direkte Summand von zwei endlichdimensionalen Moduln"
##  by K. Bongartz, Arch Math., vol. 53, 256-258, with the modification
##  done by Andrzej Mroz found in "On the computational complexity of Bongartz's
##  algorithm" (improving the complexity of the algorithm).
##
InstallMethod( CommonDirectSummand, 
"for two QuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( R1, R2 ) 

  local HomR1R2, HomR2R1, r1r2, r2r1, m, n, l, zero, j, i, temp, r, f, 
        fr2r1, r2r1f;

  if AlgebraOfRepresentation( R1 ) <> AlgebraOfRepresentation( R2 ) then 
    Error( "The two representations are not representations over the same algebra.\n" );
  fi;
  
  HomR1R2 := BasisVectors( Basis( Hom( R1, R2 ) ) );
  HomR2R1 := BasisVectors( Basis( Hom( R2, R1 ) ) );
  r1r2 := Length( HomR1R2 );
  r2r1 := Length( HomR2R1 );
  
  if r1r2 = 0 or r2r1 = 0 then 
    return false;
  fi;
  
  m := Maximum( DimensionVector( R1 ) );
  n := Maximum( DimensionVector( R2 ) );
  if n = m then
    l := n;
  else
    l := Minimum( [ n, m ] ) + 1;
  fi;
  
  n := Int( Ceil( Log2( 1.0 * ( l ) ) ) );
  
  zero := ZeroMorphism( R1, R1 );
  
  for j in [ 1..r2r1 ] do
    for i in [ 1..r1r2 ] do
      if l > 1 then  # because hom^0 * hom => error! 
        temp := PreCompose( HomR1R2[ i ], HomR2R1[ j ] );
        for r in [ 1..n ] do
          temp := PreCompose( temp, temp );
        od;
        f := PreCompose( temp, HomR1R2[ i ] );
      else 
        f := HomR1R2[ i ];
      fi;
      
      fr2r1 := PreCompose( f, HomR2R1[ j ] );
      
      if fr2r1 <> zero then
        r2r1f := PreCompose( HomR2R1[ j ], f ); 
        return [ ImageObject( fr2r1 ), KernelObject( fr2r1 ), ImageObject( r2r1f ), KernelObject( r2r1f ) ];
      fi;
    od;
  od;
  
  return false;
end
  );

#######################################################################
##
#O  CommonDirectSummand( <M1>, <M2> )
##
##  This function is using the algorithm for finding a common direct 
##  summand presented in the paper "Gauss-Elimination und der groesste
##  gemeinsame direkte Summand von zwei endlichdimensionalen Moduln"
##  by K. Bongartz, Arch Math., vol. 53, 256-258, with the modification
##  done by Andrzej Mroz found in "On the computational complexity of Bongartz's
##  algorithm" (improving the complexity of the algorithm).
##
InstallMethod( CommonDirectSummand, 
"for two QuiverModule",
[ IsQuiverModule, IsQuiverModule ],
function( M1, M2 ) 

  local side, R1, R2, commondir;

  side := Side( M1 );
  if side <> Side( M2 ) then
    Error( "The entered modules are not modules on the same side.\n" );
  fi;
  R1 := UnderlyingRepresentation( M1 );
  R2 := UnderlyingRepresentation( M2 );
  commondir := CommonDirectSummand( R1, R2 );
 
  if commondir = false then 
    return false;
  else
    return List( commondir, m -> AsModule( side, m ) );
  fi;
end
  );

#######################################################################
##
#O  MaximalCommonDirectSummand( <R1>, <R2> )
##
##  This function is using the algorithm for finding a maximal common 
##  direct summand based on the algorithm presented in the paper 
##  "Gauss-Elimination und der groesste gemeinsame direkte Summand von 
##  zwei endlichdimensionalen Moduln" by K. Bongartz, Arch Math., 
##  vol. 53, 256-258, with the modification done by Andrzej Mroz found 
##  in "On the computational complexity of Bongartz's algorithm" 
##  (improving the complexity of the algorithm).
##
InstallMethod( MaximalCommonDirectSummand, 
"for two QuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( R1, R2 ) 

  local U, V, maxcommon, L;
  
  U := R1;
  V := R2;
  maxcommon := [];
  repeat
    L := CommonDirectSummand( U, V );
    if L <> false and L <> fail then 
      Add( maxcommon, L[ 1 ] );
      U := L[ 2 ];
      V := L[ 4 ];
      if Dimension( L[ 2 ] ) = 0 or Dimension( L[ 4 ] ) = 0 then
        break;
      fi;
    fi;
  until  L = false or L = fail;
  
  if Length( maxcommon ) = 0 then 
    return false;
  else 
    return [ maxcommon, U, V ];
  fi;     
end
  );

#######################################################################
##
#O  MaximalCommonDirectSummand( <M1>, <M2> )
##
##  This function is using the algorithm for finding a maximal common 
##  direct summand based on the algorithm presented in the paper 
##  "Gauss-Elimination und der groesste gemeinsame direkte Summand von 
##  zwei endlichdimensionalen Moduln" by K. Bongartz, Arch Math., 
##  vol. 53, 256-258, with the modification done by Andrzej Mroz found 
##  in "On the computational complexity of Bongartz's algorithm" 
##  (improving the complexity of the algorithm).
##
InstallMethod( MaximalCommonDirectSummand, 
"for two QuiverRepresentation",
[ IsQuiverModule, IsQuiverModule ],
function( M1, M2 ) 

  local side, R1, R2, maxcommon;

  side := Side( M1 );
  if side <> Side( M2 ) then
    Error( "The entered modules are not modules on the same side.\n" );
  fi;
  R1 := UnderlyingRepresentation( M1 );
  R2 := UnderlyingRepresentation( M2 );
  maxcommon := MaximalCommonDirectSummand( R1, R2 );
  if maxcommon = false then
    return maxcommon;
  fi;
  maxcommon[ 1 ] := List( maxcommon[ 1 ], m -> AsModule( side, m ) );
  maxcommon{ [ 2..3 ] } := List(  maxcommon{ [ 2..3 ] }, m -> AsModule( side, m ) );
  
  return maxcommon;
end
  );

#######################################################################
##
#O  DecomposeRepresentation( <R> )
##
##  Given a representation  <R>  this function computes a list of 
##  representations  L  such that  <R>  is isomorphic to the direct 
##  sum of the representations on the list  L. 
##
InstallMethod( DecomposeRepresentation, 
"for a quiver representation", true,
[ IsQuiverRepresentation ], 0, 
function( R )

  local endo, idemmaps;

  endo := EndomorphismAlgebra( R );
  idemmaps := CompleteSetOfPrimitiveIdempotents( endo );
  idemmaps := List( idemmaps, x -> FromEndRToHomRR( R, x ) );
    
  return List( idemmaps, x -> ImageObject( x ) );
end
  );

#######################################################################
##
#O  DecomposeRepresentationWithMultiplicities( <M> )
##
##  Given a QuvierRepresentation this function decomposes the represen- 
##  tation  R  into indecomposable representations with multiplicities. 
##  First decomposing the representation  R = R_1 + R_2 + ... + R_t, 
##  then checking if  R_i  is isomorphic to R_1 for  i in [2..t], 
##  removing those indices from [ 2..t ] which are isomorphic to R_1, 
##  call this set rest, take the minimum from this, call it current, 
##  remove it from rest, and continue as above until rest is empty. 
##
InstallMethod ( DecomposeRepresentationWithMultiplicities, 
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ],
function( R )

  local L, current, non_iso_summands, rest, basic_summands, 
        multiplicities, temprest, i;
  
  if Dimension( R ) = 0 then
    return [ ]; 
  fi;
#
#   Decompose the representation  R.
#
  L := DecomposeRepresentation( R );
#
#   Do the initial setup.
#
  current := 1;
  non_iso_summands := 1;
  rest := [ 2..Length( L ) ];
  basic_summands := [ ];
  multiplicities := [ ];
  Add( basic_summands, L[ 1 ] );
  Add( multiplicities, 1 );
# 
#   Go through the algorithm above to find the multiplicities.
#   
  while Length( rest ) > 0 do
    temprest := ShallowCopy( rest );
    for i in temprest do
      if DimensionVector( L[ current ] ) = DimensionVector( L[ i ] ) then
        if CommonDirectSummand( L[ current ], L[ i ] ) <> false then
          multiplicities[ non_iso_summands ] := 
            multiplicities[ non_iso_summands ] + 1;
          RemoveSet( rest, i );
        fi;
      fi;
    od;
    if Length( rest ) > 0 then 
      current := Minimum( rest );
      non_iso_summands := non_iso_summands + 1;
      Add( basic_summands, L[ current ] );
      RemoveSet( rest, current );
      Add( multiplicities, 1 );
    fi;
  od;
  
  return [ basic_summands, multiplicities ];
end
  );

InstallMethod ( DecomposeModule, 
"for a IsQuiverModule",
[ IsQuiverModule ],
function( M )

  local side, R, decomp;
  
  side := Side( M );
  R := UnderlyingRepresentation( M );
  decomp := DecomposeRepresentation( R );
  decomp := List( decomp, r -> AsModule( side, r ) );
  
  return decomp;
end
  );

#######################################################################
##
#O  DecomposeModuleWithMultiplicities( <M> )
##
##  Given a QuvierModule this function decomposes the module 
##  M  into indecomposable modules with multiplicities. First 
##  decomposing the module  M = M_1 + M_2 + ... + M_t, then checking
##  if  M_i  is isomorphic to M_1 for  i in [2..t], removing those
##  indices from [ 2..t ] which are isomorphic to M_1, call this set 
##  rest, take the minimum from this, call it current, remove it from
##  rest, and continue as above until rest is empty. 
##
InstallMethod ( DecomposeModuleWithMultiplicities, 
"for a IsQuiverModule",
[ IsQuiverModule ],
function( M )

  local side, R, decomp, temp;
  
  side := Side( M );
  R := UnderlyingRepresentation( M );
  decomp := DecomposeRepresentationWithMultiplicities( R );
  temp := List( decomp[ 1 ], r -> AsModule( side, r ) );
  
  return [ temp, decomp[ 2 ] ];
end
  );

#######################################################################
##
#O  BlockSplittingIdempotents( <R> )
##
##  Given a QuiverRepresentation  <R>  this function returns a set 
##  \{e_1,..., e_t\} of idempotents in the endomorphism of  <R>  such 
##  that  R \simeq  Im e_1\oplus \cdots \oplus Im e_t,
##  where each  Im e_i  is isomorphic to  X_i^{n_i}  for some 
##  decomposable representation  X_i  and positive integer  n_i  for all i.
##  
InstallMethod( BlockSplittingIdempotents, 
"for a representation of a quiver",
[ IsQuiverRepresentation ],
function( R )

  local EndR, J, gens, I, map, top, orthogonalidempotents, i, etemp, 
        temp;
    
  if Dimension( R ) = 0 then
    return [];
  fi;
  EndR := EndomorphismAlgebra( R );
  J := RadicalOfAlgebra( EndR );
  gens := GeneratorsOfAlgebra( J );
  I := Ideal( EndR, gens ); 
  map := NaturalHomomorphismByIdeal( EndR, I ); 
  top := CentralIdempotentsOfAlgebra( Range( map ) );
  orthogonalidempotents := [ ];
  Add( orthogonalidempotents, LiftIdempotent( map, top[ 1 ] ) );
  for i in [ 1..Length( top ) - 1 ] do
        etemp := Sum( orthogonalidempotents{ [ 1..i ] } );
        temp := LiftTwoOrthogonalIdempotents( map, etemp, top[ i + 1 ] );
        Add( orthogonalidempotents, temp[ 2 ] );
    od;
    orthogonalidempotents := List( orthogonalidempotents, x -> FromEndRToHomRR( R, x ) );
    
    return orthogonalidempotents;
end
);

#######################################################################
##
#O  BlockSplittingIdempotents( <R> )
##
##  Given a QuiverModule  <M>  this function returns a set 
##  \{e_1,..., e_t\} of idempotents in the endomorphism of  <M>  such 
##  that  M \simeq  Im e_1\oplus \cdots \oplus Im e_t,
##  where each  Im e_i  is isomorphic to  X_i^{n_i}  for some 
##  decomposable representation  X_i  and positive integer  n_i  for all i.
##  
InstallMethod( BlockSplittingIdempotents, 
"for a representation of a quiver",
[ IsQuiverModule ],
function( M )

  local   side,  R,  idempotents;
  
  side := Side( M );
  R := UnderlyingRepresentation( M );
  idempotents := BlockSplittingIdempotents( R );
  idempotents := List( idempotents, e -> AsModuleHomomorphism( side, e ) );
                       
  return idempotents;
end
  );

#######################################################################
##
#O  BlockDecompositionOfRepresentation( <R> )
##
##  Given a QuiverRepresentation  <R>  this function returns a set of 
##  representations \{R_1,..., R_t\} such that  
##        R \simeq  R_1\oplus \cdots \oplus R_t,
##  where each  R_i  is isomorphic to  X_i^{n_i}  for some 
##  decomposable representation  X_i  and positive integer  n_i  for all i.
##  
InstallMethod( BlockDecompositionOfRepresentation, 
"for a QuiverRepresentation",
[ IsQuiverRepresentation ],
function( R );
    
    return List( BlockSplittingIdempotents( R ), e -> ImageObject( e ) );
end
);

#######################################################################
##
#O  BlockDecompositionOfModule( <M> )
##
##  Given a QuiverModule  <M>  this function returns a set of 
##  modules \{M_1,..., M_t\} such that  
##        M \simeq  M_1\oplus \cdots \oplus M_t,
##  where each  M_i  is isomorphic to  X_i^{n_i}  for some 
##  decomposable module  X_i  and positive integer  n_i  for all i.
##  
InstallMethod( BlockDecompositionOfModule, 
"for a QuiverModule",
[ IsQuiverModule ],
function( M )
    
  local side, R, blockdecomp;
  
  side := Side( M );
  R := UnderlyingRepresentation( M );
  blockdecomp := BlockDecompositionOfRepresentation( R );
  
  return List( blockdecomp, r -> AsModule( side, r ) );
end
);

#######################################################################
##
#P  IsIndecomposableRepresentation( <R> )
##
##  If the entered representation  <R>  is an indecomposable 
##  representation over an algebra over a finite field, then the function
##  returns true.  If the field is not finite, the function makes some 
##  easy checks ensuring that the representation is indecomposable.  If 
##  they fail, it searches for other methods to apply. 
##
InstallMethod( IsIndecomposableRepresentation,
"for a QuiverRepresentation",
[ IsQuiverRepresentation ],
function( R )
    
  local K, cat, T, S;
    
  K := LeftActingDomain( R );
  if IsFinite( K ) then 
    return Length( DecomposeRepresentation( R ) ) = 1;
  fi;
  cat := CapCategory( R ); 
  T := TopFunctor( cat );
#  S := SocleFunctor( cat );
  
  if IsSimpleRepresentation( ApplyFunctor( T, R ) ) then
    return true;
  fi;
#  if IsSimpleRepresentation( ApplyFunctor( S, R ) ) then
#    return true;
#  fi;
  TryNextMethod( );
end
  );

#######################################################################
##
#P  IsIndecomposableModule( <M> )
##
##  If the entered module  <M>  is an indecomposable module over an 
##  algebra over a finite field, then the function returns true.  If the 
##  field is not finite, the function makes some easy checks ensuring 
##  that the module is indecomposable.  If they fail, it searches for 
##  other methods to apply. 
##
InstallMethod( IsIndecomposableModule,
"for a QuiverModule",
[ IsQuiverModule ],
function( M )
    
  local R;
    
  R := UnderlyingRepresentation( M );

  return IsIndecomposableRepresentation( R );
end
  );

#######################################################################
##
#O  IsDirectSummand( <M>, <N> )
##
##  This function returns true if the representation  <R1>  is 
##  isomorphic to a direct of the representation  <R2>, an error 
##  message is issued if  <R1>  and  <R2>  are not representations over 
##  over the same algebra.
##  
InstallMethod( IsDirectSummand, 
"for two QuiverRepresentation",
[ IsQuiverRepresentation, IsQuiverRepresentation ],
function( R1, R2 ) 

  local L;

  if AlgebraOfRepresentation( R1 ) <> AlgebraOfRepresentation( R2 ) then
    Error( "The entered representations are not over the same algebra.\n" );
  fi;
  if not DimensionVectorPartialOrder( R1, R2 ) then 
    return false;
  fi;
  L := MaximalCommonDirectSummand( R1, R2 );
  if L = false then 
    return false;
  else 
    if Dimension( L[ 2 ] ) = 0 then 
      return true;
    else
      return false;
    fi;
  fi;
end
  );

#######################################################################
##
#O  IsDirectSummand( <M>, <N> )
##
##  This function returns true if the module  <M1>  is 
##  isomorphic to a direct of the module  <M2>, an error 
##  message is issued if  <M1>  and  <M2>  are not modules over 
##  over the same algebra.
##  
InstallMethod( IsDirectSummand, 
"for two QuiverModule",
[ IsQuiverModule, IsQuiverModule ],
function( M1, M2 ) 

  local R1, R2;

  R1 := UnderlyingRepresentation( M1 );
  R2 := UnderlyingRepresentation( M2 );  
  
  return IsDirectSummand( R1, R2 );
end
  );

#######################################################################
##
#O  BasicVersionOfRepresentation( <R> )
##
##  This function returns a basic version of the entered representation
##  <R>, that is, if <R> \simeq R_1^{n_1} \oplus \cdots \oplus R_t^{n_t}
##  where  R_i  is indecomposable, then  R_1\oplus \cdots \oplus R_t
##  is returned. At present, this function only work at for finiite 
##  dimensional (quotients of a) path algebra over a finite field. If
##  <R>  is zero, then  <R> is returned.
##  
InstallMethod( BasicVersionOfRepresentation, 
"for a IsQuiverRepresentaiton",
[ IsQuiverRepresentation ],
function( R ) 

    local L;
    
    if Dimension( R ) = 0 then
        return R;
    else
        L := DecomposeRepresentationWithMultiplicities( R );
        return DirectSum( L[ 1 ] );
    fi;
end
  );

#######################################################################
##
#O  NumberOfNonIsoDirectSummands( <R> )
##
##  This function computes the number of non-isomorphic indecomposable 
##  direct summands of the representation  <R>, and in addition returns
##  the dimensions of the simple blocks of the semisimple ring  
##  End(R)/rad End(R). 
##
InstallMethod( NumberOfNonIsoDirectSummands,
"for a IsQuiverRepresentation",
[ IsQuiverRepresentation ],
function( R )
 
    local   EndR,  K,  J,  gens,  I,  A,  top;

    EndR := EndomorphismAlgebra( R );
    K := LeftActingDomain( R );
    J := RadicalOfAlgebra( EndR );
    gens := GeneratorsOfAlgebra( J );
    I := Ideal( EndR, gens ); 
    A := EndR/I;
    top := CentralIdempotentsOfAlgebra( A );
    
    return [ Length( top ), List( DirectSumDecomposition( EndR/I ), Dimension ) ];
end
  );

#######################################################################
##
#O  DecomposeModuleViaCharPoly( <M> )
##
##  Given a module  M  over a finite dimensional quotient of a path 
##  algebra over a finite field, this function tries to decompose the 
##  entered module  M  by choosing random elements in the endomorphism
##  ring of  M computing a factorization of their characteristic 
##  polynomials. 
##  
InstallMethod( DecomposeModuleViaCharPoly, 
   "for a path algebra matmodule",
   [ IsQuiverModule ],
   function( M ) 
    
  local K, side, dimM, num_vert, homMM, bashomMM, t, V, num_repeats, 
        max_runs, p, h, matrices, charpolys, nonzero_vert, m, 
        factorlist, numfactors, occurringprimes, multiplicities, 
        maxmultiplicities, polys, mats, f, mat, i, homs;
      
    K := LeftActingDomain( M );
    side := Side( M );
    if not IsFinite( K ) then 
        Error( "The entered module is not over a finite field.\n" );
    fi;
    dimM := DimensionVector( M );
    num_vert := Length( DimensionVector( M ) );
    if Sum( dimM ) < num_vert + 1 then 
      return DecomposeModule( M );
    fi;
    homMM := Hom( M, M );
    bashomMM := BasisVectors( Basis( homMM ) );
    t := Length( bashomMM );
    V := FullRowSpace( K, t );
    num_vert := Length( DimensionVector( M ) );
    num_repeats := 0;
    max_runs := Int( 40 / Sum( dimM ) ) + 1 + 9;
    repeat
      num_repeats := num_repeats + 1;
      p := Random( V );
      h := LinearCombination( bashomMM, p );
      matrices := MatricesOfModuleHomomorphism( h );
      charpolys := [ ];
      nonzero_vert := 0;
      for m in matrices do
        if not IsZero( m ) then 
          nonzero_vert := nonzero_vert + 1;
          Add(charpolys, CharacteristicPolynomial( RowsOfMatrix( m ) ) );
        fi;
      od;
      factorlist := List( charpolys, Factors );
      numfactors := List( factorlist, Length );
      occurringprimes := Unique( Flat( factorlist ) );
      if Length( occurringprimes ) > 1 then 
        multiplicities := List( [ 1..Length( occurringprimes ) ], 
                                j -> List( [1..nonzero_vert], i -> Length( Positions( factorlist[i], occurringprimes[ j ] ) ) ) );
        maxmultiplicities := List( multiplicities, m -> Maximum( m ) );
        polys := List( [ 1..Length( occurringprimes ) ], i -> occurringprimes[ i ]^maxmultiplicities[ i ] );
        
        mats := [];
        for f in polys do
          mat := [ ];
          for i in [ 1..num_vert ] do
            if dimM[ i ] = 0 then
              Add( mat, MakeZeroMatrix( K, 0, 0 ) );
            else
              Add( mat, MatrixByRows( K, Value( f, RowsOfMatrix( matrices[ i ] ) ) ) );
            fi;
          od;
          Add( mats, mat );
        od;
        homs := [ ];
        for m in mats do
          Add( homs, QuiverModuleHomomorphism( M, M, m ) );
        od;

        return Flat( List( List( homs, h -> KernelObject( h ) ), m -> DecomposeModuleViaCharPoly( m ) ) );
      fi;
    until 
      num_repeats = max_runs;

    return [ M ];
end
  );
