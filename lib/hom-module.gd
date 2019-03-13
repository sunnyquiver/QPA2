#! @Chapter Hom spaces

#! @Section Hom modules

#! @Description
#!  GAP category for hom modules.
DeclareCategory( "IsHomModule", IsQuiverModule and IsHomObject );

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
DeclareOperation( "HomFunctor", [ IsSide, IsQuiverModuleCategory, IsQuiverModuleCategory ] );

#!
DeclareOperation( "HomFunctor", [ IsQuiverModuleCategory ] );

