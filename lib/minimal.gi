#######################################################################
##
#P  IsRightMinimal( <f> )
##
##  This function returns true is the homomorphism  <f>  is right 
##  minimal. 
##
InstallMethod ( IsRightMinimal, 
"for a IsQuiverRepresentationHomomorphism",
[  IsQuiverRepresentationHomomorphism ],
function( f )

    local   B,  C,  BB,  mat,  rows,  g,  matrices,  row,  m,  dims,  
            Ann_f,  radEndB;

    B := Source( f );
    C := Range( f );
    BB := BasisVectors( Basis( Hom( B, B ) ) );
    mat := List( BB, x -> PreCompose( x, f ) );
    rows := [ ];
    for g in mat do
        matrices := MatricesOfRepresentationHomomorphism( g );
        row := [ ];
        for m in matrices do
            if IsZero( m ) then
                dims := DimensionsMat( m );
                Add( row, Flat( NullMat( dims[ 1 ], dims[ 2 ], Rationals ) ) );
            else
                Add( row, Flat( m!.rows ) );
            fi;
        od;
        Add( rows, Flat( row ) );
    od;
    Ann_f := NullspaceMat( rows );
    Ann_f := List( Ann_f, x -> LinearCombination( BB, x ) );
    radEndB := RadicalOfAlgebra( EndomorphismAlgebra( B ) ); 
    Ann_f := List( Ann_f, x -> FromHomRRToEndR( x ) );

    if ForAll( Ann_f, x -> x in radEndB ) then 
        return true;
    else
        return false;
    fi;
end
);

#######################################################################
##
#P  IsLeftMinimal( <f> )
##
##  This function returns true is the homomorphism  <f>  is left 
##  minimal. 
##
InstallMethod ( IsLeftMinimal, 
    "for a IsQuiverRepresentationHomomorphism",
    true,
    [ IsQuiverRepresentationHomomorphism ],
    0,
    function( f )

    local   A,  B,  BB,  mat,  rows,  g,  matrices,  row,  m,  dims,  
            Ann_f,  radEndB;

    A := Source( f );
    B := Range( f );
    BB := BasisVectors( Basis( Hom( B, B ) ) );
    mat := List( BB, x -> PreCompose( f, x ) );
    rows := [ ];
    for g in mat do
        matrices := MatricesOfRepresentationHomomorphism( g );
        row := [ ];
        for m in matrices do
            if IsZero( m ) then
                dims := DimensionsMat( m );
                Add( row, Flat( NullMat( dims[ 1 ], dims[ 2 ], Rationals ) ) );
            else
                Add( row, Flat( m!.rows ) );
            fi;
        od;
        Add( rows, Flat( row ) );
    od;
    Ann_f := NullspaceMat( rows );    
    Ann_f := List( Ann_f, x -> LinearCombination( BB, x ) );
    radEndB := RadicalOfAlgebra( EndomorphismAlgebra( B ) ); 
    Ann_f := List( Ann_f, x -> FromHomRRToEndR( x ) );

    if ForAll(Ann_f, x -> x in radEndB) then 
        return true;
    else
        return false;
    fi;
end
);

InstallMethod ( MoreRightMinimalVersion, 
"for a IsQuiverRepresentationHomomorphism",
[ IsQuiverRepresentationHomomorphism ],
function( f ) 


    local   B,  g,  A,  BHomBA,  BHomAA,  n,  ba,  aa,  hh,  nmaps,  
            i,  gg;

    B := Source( f );
    g := KernelEmbedding( f );
    A := Source( g );
    BHomBA := BasisVectors( Basis( Hom( B, A ) ) );
    if Length( BHomBA ) = 0 then 
        return f;
    fi;

    BHomAA  := BasisVectors( Basis( Hom( A, A ) ) );
    n := Maximum( Concatenation( DimensionVector( A ), DimensionVector( B ) ) );
    for ba in BHomBA do 
        for aa in BHomAA do
#            gg := PreCompose( [ g, ba , aa ] )^n;
	    gg := PreCompose( [ g, ba , aa ] );
	    nmaps := [ ];
	    for i in [ 1..n ] do
	    	Add( nmaps, gg );
	    od;
	    gg := PreCompose( nmaps ); 
            if gg <> ZeroMorphism( A, A ) then
                hh := PreCompose( [ ba, aa , g ] );
                nmaps := [ ];
                for i in [ 1..n ] do
	    	    Add( nmaps, hh );
                od; 
                hh := PreCompose( nmaps );
                
                return [ PreCompose( KernelEmbedding( hh ), f ), PreCompose( ImageEmbedding( hh ), f ) ];
            fi;
        od;
    od;
    
    return f;
end
  );

InstallMethod ( MoreLeftMinimalVersion, 
"for a IsQuiverRepresentationHomomorphism",
[ IsQuiverRepresentationHomomorphism ],
function( f ) 

    local   B,  g,  C,  BHomCB,  BHomCC,  n,  cc,  cb,  gg,  nmaps,  
            i,  hh,  t;

    B := Range( f );
    g := CokernelProjection( f );
    C := Range( g );
    BHomCB := BasisVectors( Basis( Hom( C, B ) ) );
    if Length( BHomCB ) = 0 then 
        return f;
    fi;
    
    BHomCC := BasisVectors( Basis( Hom( C, C ) ) );
    n := Maximum( Concatenation( DimensionVector( B ), DimensionVector( C ) ) );
    for cc in BHomCC do 
        for cb in BHomCB do
#            gg := PreCompose( [ cc, cb, g ] )^n;
            gg := PreCompose( [ cc, cb, g ] );
            nmaps := [ ];
	    for i in [ 1..n ] do
                Add( nmaps, gg );
            od;
	    gg := PreCompose( nmaps );             
            if gg <> ZeroMorphism( C, C ) then
#                hh := PreCompose( [ g, cc, cb ] )^n;
                hh := PreCompose( [ g, cc, cb ] );
                nmaps := [ ];
                for i in [ 1..n ] do
	    	    Add( nmaps, hh );
                od; 
                hh := PreCompose( nmaps );
                t := RightInverseOfHomomorphism( KernelEmbedding( hh ) );
                
                return [ PreCompose( f, t ), PreCompose( f, CoastrictionToImage( hh ) ) ];
            fi;
        od;
    od;
    
    return f;
end
);

#######################################################################
##
#A  RightMinimalVersion( <f> )
##
##  This function returns a right minimal version  f'  of the 
##  homomorphism  <f>  in addition to a list of representations  B such that 
##  Source(f') direct sum the representations on the list  B  is isomorphic to
##  Source(f).
##
InstallMethod ( RightMinimalVersion, 
"for a IsQuiverRepresentationHomomorphism",
[ IsQuiverRepresentationHomomorphism ],
function( f )

    local   Bprime,  g,  L;

    Bprime := [ ];
    g := f;
    repeat
        L := MoreRightMinimalVersion( g );
        if L <> g then 
            g := L[ 1 ];
            Add( Bprime, Source( L[ 2 ] ) );
        fi;
    until 
        L = g;
    
    SetIsRightMinimal( g, true );

    return [ g, Bprime ];
end
);

#######################################################################
##
#A  LeftMinimalVersion( <f> )
##
##  This function returns a left minimal version  f'  of the 
##  homomorphism  <f>  in addition to a list of representations  B such that 
##  Range(f') direct sum the representations on the list  B  is isomorphic to
##  Range(f).
##
InstallMethod ( LeftMinimalVersion, 
"for a IsQuiverRepresentationHomomorphism",
[ IsQuiverRepresentationHomomorphism ],
function( f )

    local   Bprime,  g,  L;

    Bprime := [ ];
    g := f;
    repeat
        L := MoreLeftMinimalVersion( g );
        if L <> g then 
            g := L[ 1 ];
            Add( Bprime, Range( L[ 2 ] ) );
        fi;
    until 
        L = g;

    SetIsLeftMinimal( g, true );

    return [ g, Bprime ];
end
  );
