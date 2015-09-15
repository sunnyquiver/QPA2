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

DeclareCategory( "IsQuiverModuleHomomorphism",
                 IsMapping and IsSPGeneralMapping and IsVectorSpaceHomomorphism );

DeclareCategory( "IsLeftQuiverModuleHomomorphism",
                 IsQuiverModuleHomomorphism );
DeclareCategory( "IsRightQuiverModuleHomomorphism",
                 IsQuiverModuleHomomorphism );

DeclareOperation( "QuiverModuleHomomorphism",
                  [ IsQuiverModule, IsQuiverModule, IsDenseList ] );

DeclareOperation( "LeftQuiverModuleHomomorphism",
                  [ IsLeftQuiverModule, IsLeftQuiverModule, IsDenseList ] );

DeclareOperation( "RightQuiverModuleHomomorphism",
                  [ IsRightQuiverModule, IsRightQuiverModule, IsDenseList ] );

DeclareOperation( "AsModuleHomomorphism",
                  [ IsQuiverRepresentationHomomorphism, IsQuiverAlgebra ] );

DeclareOperation( "AsLeftModuleHomomorphism",
                  [ IsQuiverRepresentationHomomorphism ] );

DeclareOperation( "AsRightModuleHomomorphism",
                  [ IsQuiverRepresentationHomomorphism ] );

DeclareOperation( "QuiverModuleHomomorphismNC",
                  [ IsQuiverModule, IsQuiverModule,
                    IsDenseList ] );

DeclareAttribute( "UnderlyingRepresentationHomomorphism",
                  IsQuiverModuleHomomorphism );

DeclareAttribute( "MatricesOfModuleHomomorphism",
                  IsQuiverModuleHomomorphism );
