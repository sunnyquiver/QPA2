InstallHandlingByNiceBasis( "IsSubspaceOfQuiverAlgebra",
  rec( detect :=
       function( F, gens, V, z )
         if z <> false then
           return IsQuiverAlgebraElement( z );
         else
           return IsQuiverAlgebraElement( gens[ 1 ] );
         fi;
       end,
       NiceFreeLeftModuleInfo := function( V )
         local gens, algebra, alg_as_vector_space, B, gens_vec, 
               nice_space, basis;
         gens := GeneratorsOfLeftModule( V );
         algebra := AlgebraOfElement( Zero( V ) );
         alg_as_vector_space := LeftActingDomain( algebra ) ^ Dimension( algebra );
         B := CanonicalBasis( algebra );
         gens_vec := List( gens, g -> Coefficients( B, g ) );
         nice_space := Subspace( alg_as_vector_space, gens_vec );
         return rec( algebra := algebra,
                     basis := B,
                     nice_space := nice_space );
       end,
       NiceVector := function( V, v )
         local info, B, c;
         info := NiceFreeLeftModuleInfo( V );
         B := info.basis;
         c := Coefficients( B, v );
         return c;
       end,
       UglyVector := function( V, r )
         local info, B;
         info := NiceFreeLeftModuleInfo( V );
         B := info.basis;
         return LinearCombination( B, r );
       end ) );


InstallMethod( NiceFreeLeftModule, [ IsSubspaceOfQuiverAlgebra ],
function( V )
  local info;
  info := NiceFreeLeftModuleInfo( V );
  return info.nice_space;
end );

