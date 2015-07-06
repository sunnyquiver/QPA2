DeclareCategory( "IsQuiverRepresentationHomomorphism",
                 IsMapping and IsSPGeneralMapping and IsVectorSpaceHomomorphism );

DeclareOperation( "QuiverRepresentationHomomorphism",
                  [ IsQuiverRepresentation, IsQuiverRepresentation,
                    IsDenseList ] );

DeclareOperation( "QuiverRepresentationHomomorphismNC",
                  [ IsQuiverRepresentation, IsQuiverRepresentation,
                    IsDenseList ] );

DeclareOperation( "MatricesOfRepresentationHomomorphism",
                  [ IsQuiverRepresentationHomomorphism ] );

# DeclareOperation( "ImageElm",
#                   [ IsQuiverRepresentationHomomorphism,
#                     IsQuiverRepresentationElement ] );

