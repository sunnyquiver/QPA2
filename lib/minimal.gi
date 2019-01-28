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
