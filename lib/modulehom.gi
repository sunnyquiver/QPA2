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

InstallMethod( AsModuleHomomorphism, "for quiver representation homomorphism and quiver modules",
               [ IsQuiverRepresentationHomomorphism, IsQuiverModule, IsQuiverModule ],
function( rf, M, N )
  local cat, side, R1, R2, rep_algebra, hom_type, matrices, f;

  cat := CapCategory( M );
  if cat <> CapCategory( N ) then
    Error( "modules from different categories" );
  fi;
  side := Side( cat );
  R1 := Source( rf );
  R2 := Range( rf );
  if R1 <> UnderlyingRepresentation( M ) then
    Error( "representation homomorphism has wrong source" );
  fi;
  if R2 <> UnderlyingRepresentation( N ) then
    Error( "representation homomorphism has wrong range" );
  fi;
  rep_algebra := AlgebraOfRepresentation( R1 );
  hom_type := IsQuiverModuleHomomorphism^side;
  matrices := MatricesOfRepresentationHomomorphism( rf );
  f := rec();
  ObjectifyWithAttributes( f, NewType( FamilyOfQuiverModuleHomomorphisms,
                                       hom_type and IsQuiverModuleHomomorphismRep ),
                           UnderlyingRepresentationHomomorphism, rf,
                           Side, side,
                           HomSide, side,
                           MatricesOfModuleHomomorphism, matrices,
                           Source, M,
                           Range, N );
  Add( cat, f );
  return f;
end );

DeclareSideOperations( AsModuleHomomorphism,
                       AsLeftModuleHomomorphism, AsRightModuleHomomorphism, AsBimoduleHomomorphism );

InstallMethodWithSides( AsModuleHomomorphism,
                        [ IsQuiverRepresentationHomomorphism ],
side -> function( rf )
  local R1, R2;
  R1 := Source( rf );
  R2 := Range( rf );
  return AsModuleHomomorphism( rf, AsModule( side, R1 ), AsModule( side, R2 ) );
end );

InstallMethod( QuiverModuleHomomorphism,
               [ IsQuiverModule, IsQuiverModule, IsList ],
function( M1, M2, matrices )
  local R1, R2, rf;
  # TODO check that M1 and M2 are in same category
  R1 := UnderlyingRepresentation( M1 );
  R2 := UnderlyingRepresentation( M2 );
  rf := QuiverRepresentationHomomorphism( R1, R2, matrices );
  return AsModuleHomomorphism( rf, M1, M2 );
end );

InstallMethod( QuiverModuleHomomorphism,
               [ IsQuiverModule, IsQuiverModule, IsFunction ],
function( M, N, f )
  local R1, R2, rf, rm;
  # TODO check that M and N are in same category
  R1 := UnderlyingRepresentation( M );
  R2 := UnderlyingRepresentation( N );
  rf := function( e )
    local m;
    m := AsModuleElement( e, M );
    return UnderlyingRepresentationElement( f( m ) );
  end;
  rm := QuiverRepresentationHomomorphism( R1, R2, rf );
  return AsModuleHomomorphism( rm, M, N );
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

InstallOtherMethod( IsZero, [ IsQuiverModuleHomomorphism ],
               IsZeroForMorphisms );

InstallMethod( \=, "for quiver module homomorphisms",
               [ IsQuiverModuleHomomorphism, IsQuiverModuleHomomorphism ],
function( f1, f2 )
  return IsIdenticalObj( CapCategory( f1 ), CapCategory( f2 ) )
         and UnderlyingRepresentationHomomorphism( f1 ) = UnderlyingRepresentationHomomorphism( f2 );
end );

InstallOtherMethod( \+,
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
  if not c in UnderlyingField( m ) then
    TryNextMethod();
  fi;
  return AsModuleHomomorphism( Side( m ),
                               c * UnderlyingRepresentationHomomorphism( m ) );
end );

InstallOtherMethod( InverseOp, "for quiver module homomorphism",
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

InstallMethod( AsLinearTransformation, "for quiver module homomorphism",
               [ IsQuiverModuleHomomorphism ],
function( f )
  return AsLinearTransformation( UnderlyingRepresentationHomomorphism( f ) );
end );

InstallMethod( MorphismByLinearTransformation, "for quiver modules and linear transformation",
               [ IsQuiverModule, IsQuiverModule, IsLinearTransformation ],
function( M1, M2, T )
  local rep_morphism;
  if not IsIdenticalObj( CapCategory( M1 ),
                         CapCategory( M2 ) ) then
    #Error( "modules from different categories" );
    TryNextMethod();
  fi;
  rep_morphism :=  MorphismByLinearTransformation( UnderlyingRepresentation( M1 ),
                                                   UnderlyingRepresentation( M2 ),
                                                   T );
  return AsModuleHomomorphism( Side( M1 ), rep_morphism );
end );
