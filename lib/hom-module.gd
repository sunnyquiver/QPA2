#! @Chapter Hom spaces

#! @Section Hom modules

#! @Description
#!  GAP category for hom modules.
DeclareCategory( "IsHomModule", IsQuiverModule and IsHomObject );

DeclareAttribute( "HomSide", IsHomModule );

DeclareAttribute( "UnderlyingHomFunctor", IsHomModule );

#!
DeclareCategory( "IsEnrichedQuiverModuleHomomorphism",
                 IsMapping and IsSPGeneralMapping and IsVectorSpaceHomomorphism
                 and IsQuiverModuleElement );

# TODO: what about morphism from vector space to module?
# should this be an enriched quiver module homomorphism,
# or an enriched linear transformation?

DeclareAttribute( "HomSide", IsEnrichedQuiverModuleHomomorphism );

#!
DeclareOperation( "EnrichedQuiverModuleHomomorphism",
                  [ IsSide, IsQuiverModule, IsQuiverModule, IsList ] );

DeclareAttribute( "IsomorphismToLeftModule", IsQuiverModule );

DeclareAttribute( "IsomorphismFromLeftModule", IsQuiverModule );

DeclareAttribute( "IsomorphismToRightModule", IsQuiverModule );

DeclareAttribute( "IsomorphismFromRightModule", IsQuiverModule );

DeclareOperation( "IsomorphismToSide", [ IsSide, IsQuiverModule ] );

DeclareOperation( "IsomorphismFromSide", [ IsSide, IsQuiverModule ] );

DeclareOperation( "PreCompose", [ IsEnrichedQuiverModuleHomomorphism,
                                  IsEnrichedQuiverModuleHomomorphism ] );

DeclareOperation( "PreCompose", [ IsQuiverModuleHomomorphism,
                                  IsEnrichedQuiverModuleHomomorphism ] );

DeclareOperation( "PreCompose", [ IsEnrichedQuiverModuleHomomorphism,
                                  IsQuiverModuleHomomorphism ] );

DeclareOperation( "PreCompose", [ IsQuiverModuleHomomorphism,
                                  IsEnrichedQuiverModuleHomomorphism,
                                  IsQuiverModuleHomomorphism ] );



#!
DeclareOperation( "Hom", [ IsSide, IsQuiverModule, IsQuiverModule ] );
# Hom( LEFT, M, N )       (each is bimodule or left module; both must have same left structure)
# Hom( RIGHT, M, N )      (each is bimodule or right module; both must have same right structure)
# Hom( LEFT_RIGHT, M, N ) (each is bimodule; both must have same structure) (same as just Hom( M, N ); result is vector space)

#!
DeclareOperation( "Hom", [ IsSide, IsQuiverModuleHomomorphism, IsQuiverModule ] );

#!
DeclareOperation( "Hom", [ IsSide, IsQuiverModule, IsQuiverModuleHomomorphism ] );

#!
DeclareOperation( "Hom", [ IsSide, IsQuiverModuleHomomorphism, IsQuiverModuleHomomorphism ] );

#!
DeclareOperation( "HomFunctor", [ IsSide, IsQuiverModuleCategory, IsQuiverModuleCategory ] );

#!
DeclareOperation( "HomFunctor", [ IsSide, IsQuiverModule, IsQuiverModuleCategory ] );

#!
DeclareOperation( "HomFunctor", [ IsSide, IsQuiverModuleCategory, IsQuiverModule ] );

#
