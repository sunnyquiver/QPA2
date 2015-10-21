DeclareCategory( "IsQuiverModuleHomomorphism",
                 IsMapping and IsSPGeneralMapping and IsVectorSpaceHomomorphism and
                 IsCapCategoryMorphism );

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
