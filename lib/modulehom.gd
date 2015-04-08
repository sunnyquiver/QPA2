DeclareCategory( "IsQuiverRepresentationHomomorphism",
                 IsVectorSpaceHomomorphism );

DeclareOperation( "QuiverRepresentationHomomorphism",
                  [ IsQuiverRepresentation, IsQuiverRepresentation,
                    IsDenseList ] );

DeclareOperation( "QuiverRepresentationHomomorphismNC",
                  [ IsQuiverRepresentation, IsQuiverRepresentation,
                    IsDenseList ] );

DeclareOperation( "MatricesOfRepresentationHomomorphism",
                  [ IsQuiverRepresentationHomomorphism ] );
