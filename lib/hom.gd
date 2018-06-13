#! @Chapter Functors

#! @Section Hom-functors

DeclareCategory( "IsHomSpace", IsQPAVectorSpace );
DeclareCategory( "IsVectorSpaceHomSpace", IsHomSpace );
DeclareCategory( "IsQuiverRepresentationHomSpace", IsHomSpace );
DeclareCategory( "IsQuiverModuleHomSpace", IsHomSpace and IsObjectWithSide );
DeclareCategory( "IsLeftQuiverModuleHomSpace", IsQuiverModuleHomSpace );
DeclareCategory( "IsRightQuiverModuleHomSpace", IsQuiverModuleHomSpace );
DeclareCategory( "IsQuiverBimoduleHomSpace", IsQuiverModuleHomSpace );

DeclareOperation( "Hom", [ IsQPAVectorSpace, IsQPAVectorSpace ] );
DeclareOperation( "Hom", [ IsLinearTransformation, IsQPAVectorSpace ] );
DeclareOperation( "Hom", [ IsQPAVectorSpace, IsLinearTransformation ] );

DeclareOperation( "Hom", [ IsQuiverRepresentation, IsQuiverRepresentation ] );
DeclareOperation( "Hom", [ IsQuiverModule, IsQuiverModule ] );

DeclareAttribute( "Source", IsHomSpace );
DeclareAttribute( "Range", IsHomSpace );

#DeclareAttribute( "Side", IsQuiverModuleHomSpace );
DeclareAttribute( "UnderlyingRepresentationHomSpace", IsQuiverModuleHomSpace );

DeclareAttribute( "VertexHomSpaces", IsQuiverRepresentationHomSpace );
DeclareAttribute( "ArrowHomSpaces", IsQuiverRepresentationHomSpace );
DeclareAttribute( "SumOfVertexHomSpaces", IsQuiverRepresentationHomSpace );
DeclareAttribute( "SumOfArrowHomSpaces", IsQuiverRepresentationHomSpace );
DeclareAttribute( "HomSpaceVertexToArrowMap", IsQuiverRepresentationHomSpace );


#! @Description
#!  Given two representations  <A>R1</A> and <A>R2</A> this constructs
#!  a vector space basis for the Hom-space between <A>R1</A> and <A>R2</A>.
#!  <P/>
#! @Returns List of <Ref Filt="IsQuiverRepresentationHomomorphism"/>
#! @Arguments R1, R2
DeclareOperation( "BasisOfHom", [ IsQuiverRepresentation, IsQuiverRepresentation ]);

#
