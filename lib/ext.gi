BindGlobal( "FamilyOfExtensions", NewFamily( "extensions" ) );
BindGlobal( "FamilyExtSpaces", NewFamily( "Ext spaces" ) );
BindGlobal( "FamilyOfExtSpaces", CollectionsFamily( FamilyOfExtensions ) );
DeclareRepresentation( "IsExtSpaceRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [] );

#######################################################################
##
#O  Ext( <n>, <R1>, <R2> )
##
##  Given a minimal projective resolution 
##  ..... --> P_2 -- d_2 --> P_1 -- d_1 --> P_0 -- d_0 --> R1 --> 0
##  of the representation  R1, this function returns a list of three 
##  elements: (1) the differential d_n: P_n --> P_{n-1}, (2) a basis of 
##  Ext^n( R1, R2 )  inside  Hom( P_n, R2 )  and (3) a function 
##  that takes as an argument a homomorphism in  Hom( P_n, R2 )
##  and returns the coefficients of this element when written in 
##  terms of the basis of  Ext^n( R1, R2 ), if the group is non-zero
##  
##  Otherwise it returns an empty list. 
##
InstallMethod( Ext, 
"for two FieldCategoryObjects",
[ IsInt, IsFieldCategoryObject, IsFieldCategoryObject ],
function( n, R1, R2 )

  local res, P, dn, dnplus1, dnstar, dnplus1star, k, l, m, t, tprime, 
        inc, U, cat, type, ext;

  if not IsIdenticalObj( CapCategory( R1 ), CapCategory( R2 ) ) then
    Error( "The two entered objects are not in the same category.\n" );
  fi;
  if n < 0 then 
    return fail;
  fi;
  if n = 0 then
    return Hom( R1, R2 );
  fi;
  res := AsChainComplex( ProjectiveResolution( R1 ) );
  P := res[ n + 1 ];
  dn := res^n;
  dnplus1 := res^( n + 1 );
  dnstar := Hom( dn, R2 );
  dnplus1star := Hom( dnplus1, R2 );
  k := KernelEmbedding( dnplus1star ); 
  l := ImageEmbedding( dnstar ); 
  m := LiftAlongMonomorphism( k, l );
  t := CokernelProjection( m ); 
  tprime := RightInverse( t ); 
  inc := PreCompose( tprime, k ); 
  U := Range( t );
  
  cat := CapCategory( R1 );  
  type := NewType( FamilyOfExtSpaces, IsExtSpace and IsExtSpaceRep );
  ext := rec( space := U );
  ObjectifyWithAttributes( ext, type, 
                           CategoryOfExtSpace, cat,
                           ExtDegree, n,
                           StartTerm, R2,
                           EndTerm, R1,
                           AsCycleMap, inc );  
  Add( CategoryOfVectorSpaces( UnderlyingField( R1 ) ), ext );

  return ext; 
end
  );

InstallMethod( String, [ IsExtSpace ],
function( ext )
  return Concatenation( "Ext^", String( ExtDegree( ext ) ),"(", String( EndTerm( ext ) ),
                        ", ", String( StartTerm( ext ) ), ")" );
end );

InstallMethod( PrintObj, [ IsExtSpace ],
function( ext )
  Print( String( ext ) );
end );

InstallMethod( ViewObj, [ IsExtSpace ],
function( ext )
  Print( String( ext ) );
end );

InstallMethod( ExtensionFromCycle, 
"for an ExtSpace and FieldCategoryMorphism",
[ IsExtSpace, IsFieldCategoryMorphism ],
function( ext, f )

  local inc, rep;

  inc := AsCycleMap( ext );
  rep := PreImagesRepresentative( inc, f );
  if rep = fail then
    return fail;
  fi;
  
  return Vector( ext, AsList( rep ) );
end
  );

InstallMethod( Vector, 
"for an ExtSpace and a Denselist",
[ IsExtSpace, IsDenseList ],
function( ext, list )

  local type, extension;
  
  type := NewType( FamilyOfExtensions, 
                   IsExtension and IsComponentObjectRep and IsAttributeStoringRep ); 
  extension := rec( );
  ObjectifyWithAttributes( extension, type, 
                           SpaceContainingVector, ext, 
                           ExtDegree, ExtDegree( ext ), 
                           StartTerm, StartTerm( ext ),
                           EndTerm, EndTerm( ext ),
                           AsList, list );
  
  return extension;
end
  );

InstallMethod( AsCycle,
"for an Extension",
[ IsExtension ],
function( extension )
  
  local ext, vector, inc, B, b;
  
  ext := SpaceContainingVector( extension );
  vector := AsList( extension );
  inc := AsCycleMap( ext );
  b := LinearCombination( Basis( Source( inc ) ), vector ); 
  
  return ImageElm( inc, b );
end
    );

InstallMethod( String, 
"for an Extension",
[ IsExtension ],
function( extension )
  return "<extension>";
end
  );
  
InstallMethod( CanonicalBasis, [ IsExtSpace ],
function( ext )
  local dim, basis_vectors, basis;
  
  dim := Dimension( Source( AsCycleMap( ext ) ) );
  if dim = 0 then
    basis_vectors := [];
  else
    basis_vectors := List( IdentityMat( dim, UnderlyingField( ext ) ),
                           v -> Vector( ext, v ) );
  fi;
  basis := rec();
  ObjectifyWithAttributes( basis,
                           NewType( FamilyOfVectorSpaceBases,
                                    IsBasis and IsVectorSpaceBasisRep ),
                           BasisVectors, basis_vectors,
                           UnderlyingLeftModule, ext,
                           IsCanonicalBasis, true );
  return basis;
end );

#######################################################################
##
#O  AsExactSequence( <extension> )
##
##  Suppose the  <extension>  starts in R1 and ends in R2. Let 
##   .... --> P_2 --d_2--> P_1 --d_1--> P_0 --d_0--> R1 --> 0 
##  be the minimal projective resolution of R1.  The extension of
##  is degree  n  represented by a homomorphism 
##  <extension>: P_n --> R1.  We take the pushout of the following
##  diagram 
##             P_n ------ d_n ------> P_{n-1} 
##              |                      |
##              |                      |
##     h1 = <extension>             h2 |
##              |                      |
##              |                      |
##              V                      V
##              R1 ------ f1 --------> E
##  We want to clue/splice the exact sequences 
##        0 --> R1 -- f1 --> E -- coker_f1 --> Cokernel( f1 ) --> 0 
##  with 
##  0 --> \Omega^{n-1}( R1 ) --> P_{n-2} --> ..... --> P_1 --> P_0 --> R1 --> 0,
##  since we know Cokernel( f1 ) \simeq \Omega^{n-1}( R1 ). Then we have
##                    |--------------------------- dn-1 -----------------------|
##                    |                                                        V
##    P_n -- d_n --> P_{n-1} -- coker_dn --> Cokernel( dn ) ----- inc ------> P_{n-2} 
##     |              |                          |  A                          ||
##     |              |                          |  |                          ||
## h1 = <extension>   | h2                   phi |  | phi_inv                  ||
##     |              |                          |  |                          ||
##     |              |                          |  |                          ||
##     V              V                          V  |                          ||
##    R1 --- f1 ----> E ---- coker_f1 ------> Cokernel( f1 ) - phi_inv*inc -> P_{n-2}
##
##
InstallMethod( AsExactSequence, [ IsExtension ], 
function( extension )
  local n, R2, R1, cat, res, diffs, dn, h1, h2, f1, coker_f1, phi, 
        phi_inv, inc, f2;

  n := ExtDegree( extension );
  if n = 0 then
    Error( "The entered extension has degree 0 and doesn't correspond to an exact sequence.\n" );
  fi;
  R2 := StartTerm( extension );
  R1 := EndTerm( extension );
  cat := CapCategory( R1 );
  res := AsChainComplex( ProjectiveResolution( R1 ) );
  diffs := Differentials( res ); 
  res := ChainComplex( cat, Replace( diffs, 0, [ ProjectiveCover( R1 ) ] ) );
  dn := res^n;
  h1 := AsCycle( extension );
  h2 := InjectionOfCofactorOfPushout( [ dn, h1 ], 1 );
  f1 := InjectionOfCofactorOfPushout( [ dn, h1 ], 2 );
  coker_f1 := CokernelProjection( f1 );
  phi := CokernelObjectFunctorial( [ dn, [ h1, h2 ], f1 ] );
  phi_inv := Inverse( phi );
  inc := CokernelColift( dn, res^( n - 1 ) );
  f2 := PreCompose( [ coker_f1, phi_inv, inc ] );

  return Concatenation( [ f1, f2], List( [ 2..n - 1 ], i -> res^( n - i ) ) );
end
  );
