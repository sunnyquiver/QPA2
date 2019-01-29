BindGlobal( "FamilyOfQuiverModuleHomomorphisms",
            NewFamily( "quiver module homomorphisms" ) );

# We need this because the builtin global function Image uses
# the FamilySource attribute to check whether a given map can
# be applied to a given element.
SetFamilySource( FamilyOfQuiverModuleHomomorphisms,
                 FamilyOfQuiverModuleElements );

DeclareRepresentation( "IsQuiverModuleHomomorphismRep",
                       IsComponentObjectRep and IsAttributeStoringRep,
                       [] );

DeclareSideOperations( IsQuiverModuleHomomorphism,
                       IsLeftQuiverModuleHomomorphism, IsRightQuiverModuleHomomorphism, IsQuiverBimoduleHomomorphism );
DeclareSideOperations( AsModuleHomomorphism,
                       AsLeftModuleHomomorphism, AsRightModuleHomomorphism, AsBimoduleHomomorphism );

InstallMethodWithSides( AsModuleHomomorphism,
                        [ IsQuiverRepresentationHomomorphism ],
side -> function( rf )
  local R1, R2, rep_algebra, cat, hom_type, M1, M2, matrices, f;
  R1 := Source( rf );
  R2 := Range( rf );
  M1 := AsModule( side, R1 );
  M2 := AsModule( side, R2 );
  rep_algebra := AlgebraOfRepresentation( R1 );
  cat := AsModuleCategory( side, CapCategory( R1 ) );
  hom_type := IsQuiverModuleHomomorphism^side;
  matrices := MatricesOfRepresentationHomomorphism( rf );
  f := rec();
  ObjectifyWithAttributes( f, NewType( FamilyOfQuiverModuleHomomorphisms,
                                       hom_type and IsQuiverModuleHomomorphismRep ),
                           UnderlyingRepresentationHomomorphism, rf,
                           Side, side,
                           MatricesOfModuleHomomorphism, matrices,
                           Source, M1,
                           Range, M2 );
  Add( cat, f );
  return f;
end );

InstallMethod( QuiverModuleHomomorphism,
               [ IsQuiverModule, IsQuiverModule, IsList ],
function( M1, M2, matrices )
  local R1, R2, rf;
  # TODO check that M1 and M2 are in same category
  R1 := UnderlyingRepresentation( M1 );
  R2 := UnderlyingRepresentation( M2 );
  rf := QuiverRepresentationHomomorphism( R1, R2, matrices );
  return AsModuleHomomorphism( Side( M1 ), rf );
end );

InstallMethod( String,
               "for quiver module homomorphism",
               [ IsQuiverModuleHomomorphism ],
function( f )
  return Concatenation( "(", String( Source( f ) ), ")",
                        "->",
                        "(", String( Range( f ) ), ")" );
end );

InstallMethod( ViewObj,
               "for quiver module homomorphism",
               [ IsQuiverModuleHomomorphism ],
function( f )
  Print( "<", String( f ), ">" );
end );

InstallMethod( IsZero, [ IsQuiverModuleHomomorphism ],
               IsZeroForMorphisms );

InstallMethod( \=, "for quiver module homomorphisms",
               [ IsQuiverModuleHomomorphism, IsQuiverModuleHomomorphism ],
function( f1, f2 )
  return IsIdenticalObj( CapCategory( f1 ), CapCategory( f2 ) )
         and UnderlyingRepresentationHomomorphism( f1 ) = UnderlyingRepresentationHomomorphism( f2 );
end );

InstallMethod( \+,
               [ IsQuiverModuleHomomorphism, IsQuiverModuleHomomorphism ],
function( f, g )
  if not IsIdenticalObj( CapCategory( f ), CapCategory( g ) ) then
    Error( "homomorphisms from different categories" );
  fi;
  return AsModuleHomomorphism( Side( f ),
                               UnderlyingRepresentationHomomorphism( f ) +
                               UnderlyingRepresentationHomomorphism( g ) );
end );

InstallMethod( \*, [ IsMultiplicativeElement, IsQuiverModuleHomomorphism ],
function( c, m )
  return AsModuleHomomorphism( Side( m ),
                               c * UnderlyingRepresentationHomomorphism( m ) );
end );

InstallMethod( InverseOp, "for quiver module homomorphism",
               [ IsQuiverModuleHomomorphism ],
function( m )
  if not IsIsomorphism( m ) then
    Error( "not an isomorphism" );
  fi;
  return AsModuleHomomorphism( Side( m ),
                               Inverse( UnderlyingRepresentationHomomorphism( m ) ) );
end );

InstallMethod( ImageElm,
               [ IsQuiverModuleHomomorphism, IsQuiverModuleElement ],
function( f, m )
  local M, r, F;
  M := ModuleOfElement( m );
  r := UnderlyingRepresentationElement( m );
  F := UnderlyingRepresentationHomomorphism( f );
  return AsModuleElement( ImageElm( F, r ), Range( f ) );
end );

InstallMethod( PreImagesRepresentative, [ IsQuiverModuleHomomorphism, IsQuiverModuleElement ],
function( h, m )
  local preim_r;
  preim_r := PreImagesRepresentative( UnderlyingRepresentationHomomorphism( h ),
                                      UnderlyingRepresentationElement( m ) );
  if preim_r = fail then
    return fail;
  else
    return AsModuleElement( preim_r, Source( h ) );
  fi;
end );

InstallMethod( SubmoduleInclusion, [ IsQuiverModule, IsHomogeneousList ],
function( M, gens )
  local R, r_gens, r_inc;
  R := UnderlyingRepresentation( M );
  r_gens := List( gens, UnderlyingRepresentationElement );
  r_inc := SubrepresentationInclusion( R, r_gens );
  return AsModuleHomomorphism( Side( M ), r_inc );
end );

#######################################################################
##
#A  RightInverseOfHomomorphism( <f> )
##
##  This function returns false if the homomorphism  <f>  is not a  
##  splittable monomorphism, otherwise it returns a splitting of the
##  split monomorphism  <f>. 
##
InstallMethod ( RightInverseOfHomomorphism, 
"for a IsQuiverRepresentationHomomorphism",
[ IsQuiverRepresentationHomomorphism ],
function( f )

    local   B,  C,  CB,  mat,  id_B,  split_f;

    B := Source( f );
    C := Range( f );
    if IsMonomorphism( f ) then 
        CB := BasisVectors( Basis( Hom( C, B ) ) );
        if Length( CB ) = 0 then 
            return false;
        fi;
        
        mat := [ ];
        mat := List( CB, x -> PreCompose( f, x ) );
        mat := List( mat, m -> Flat( FromHomRRToEndR( m ) ) ); 
        id_B := Flat( FromHomRRToEndR( IdentityMorphism( B ) ) ); 
        split_f := SolutionMat( mat, id_B );
        if split_f <> fail then 
            split_f := LinearCombination( CB, split_f );
            SetIsSplitEpimorphism( split_f, true );
            SetIsSplitMonomorphism( f, true );
            SetLeftInverseOfHomomorphism( split_f, f ); 
            return split_f;
        else
            SetIsSplitMonomorphism( f, false );
            return false;
        fi;
    fi;
    
    return false;
end
  );
