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

#!
DeclareOperation( "EnrichedQuiverModuleHomomorphism",
                  [ IsSide, IsQuiverModule, IsQuiverModule, IsLinearTransformation ] );

#!
DeclareOperation( "EnrichedQuiverModuleHomomorphism",
                  [ IsSide, IsQuiverModule, IsQuiverModule, IsQuiverModuleHomomorphism ] );

DeclareAttribute( "AsLinearTransformation", IsEnrichedQuiverModuleHomomorphism );

DeclareAttribute( "AsHomomorphism", IsEnrichedQuiverModuleHomomorphism );

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



#! @Description
#!  Compute <M>Hom(M,N)</M>, where <A>M</A> and <A>N</A> are quiver modules,
#!  not necessarily in the same category.
#!
#!  The arguments <A>M</A> and <A>N</A> are quiver modules
#!  (each is either a left module, right module or bimodule),
#!  and <A>side</A> is a side value
#!  (one of the constants LEFT, RIGHT or LEFT_RIGHT).
#!  The <A>side</A> argument determines which module structure
#!  to hom over,
#!  and the two modules must have the same acting algebra on this side.
#!
#!  The result is either a hom module or a hom space,
#!  depending on the arguments.
#!  If both modules only have module structure on the side given by
#!  the argument <A>side</A>, then the result is a hom space,
#!  and it is exactly the same as that obtained by calling
#!  <C>Hom( M, N )</C>.
#!  If one or both modules has some additional module structure,
#!  then the result is a hom module.
#!
#!  The following table shows the different possible ways to call
#!  this operation, and the type of the result in each case.
#!  <Table Align="lccl">
#!  <Row><Item>function call</Item><Item>M's acting algebras</Item><Item>N's acting algebras</Item><Item>result</Item></Row>
#!  <Row><Item><C>Hom( LEFT, M, N )</C></Item><Item>(A,-)</Item><Item>(A,-)</Item><Item>vector space</Item></Row>
#!  <Row><Item><C>Hom( LEFT, M, N )</C></Item><Item>(A,B)</Item><Item>(A,-)</Item><Item>left B-module</Item></Row>
#!  <Row><Item><C>Hom( LEFT, M, N )</C></Item><Item>(A,-)</Item><Item>(A,C)</Item><Item>right C-module</Item></Row>
#!  <Row><Item><C>Hom( LEFT, M, N )</C></Item><Item>(A,B)</Item><Item>(A,C)</Item><Item>B-C-bimodule</Item></Row>
#!  <HorLine/>
#!  <Row><Item><C>Hom( RIGHT, M, N )</C></Item><Item>(-,A)</Item><Item>(-,A)</Item><Item>vector space</Item></Row>
#!  <Row><Item><C>Hom( RIGHT, M, N )</C></Item><Item>(-,A)</Item><Item>(B,A)</Item><Item>left B-module</Item></Row>
#!  <Row><Item><C>Hom( RIGHT, M, N )</C></Item><Item>(C,A)</Item><Item>(-,A)</Item><Item>right C-module</Item></Row>
#!  <Row><Item><C>Hom( RIGHT, M, N )</C></Item><Item>(C,A)</Item><Item>(B,A)</Item><Item>B-C-bimodule</Item></Row>
#!  <HorLine/>
#!  <Row><Item><C>Hom( LEFT_RIGHT, M, N )</C></Item><Item>(A,B)</Item><Item>(A,C)</Item><Item>vector space</Item></Row>
#!  </Table>
#!
#! @Arguments side, M, N
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
