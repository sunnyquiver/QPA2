DeclareCategory( "IsQuiverRepresentationElement", IsVector );
DeclareCategory( "IsQuiverRepresentation", IsVectorSpace and CategoryCollections( IsQuiverRepresentationElement ) );

DeclareOperation( "QuiverRepresentationElement", [ IsQuiverRepresentation, IsDenseList ] );
DeclareOperation( "QuiverRepresentationElementNC", [ IsQuiverRepresentation, IsDenseList ] );
DeclareOperation( "QuiverRepresentationElementByVertices", [ IsQuiverRepresentation, IsDenseList, IsDenseList ] );
DeclareAttribute( "RepresentationOfElement", IsQuiverRepresentationElement );
DeclareAttribute( "ElementVectors", IsQuiverRepresentationElement );
DeclareOperation( "ElementVector", [ IsQuiverRepresentationElement, IsPosInt ] );
DeclareOperation( "ElementVector", [ IsQuiverRepresentationElement, IsVertex ] );
DeclareOperation( "PathAction", [ IsQuiverRepresentationElement, IsPath ] );
DeclareOperation( "QuiverAlgebraAction", [ IsQuiverRepresentationElement, IsQuiverAlgebraElement ] );

DeclareOperation( "QuiverRepresentation", [ IsQuiverAlgebra, IsDenseList, IsDenseList ] );
DeclareOperation( "QuiverRepresentationNC", [ IsQuiverAlgebra, IsDenseList, IsDenseList ] );
DeclareOperation( "QuiverRepresentationByArrows",
                  [ IsQuiverAlgebra, IsDenseList, IsDenseList, IsDenseList ] );
DeclareOperation( "AsRepresentationOfQuotientAlgebra",
                  [ IsQuiverRepresentation, IsQuotientOfPathAlgebra ] );
DeclareAttribute( "ZeroRepresentation", IsQuiverAlgebra );
DeclareAttribute( "AlgebraOfRepresentation", IsQuiverRepresentation );
DeclareAttribute( "QuiverOfRepresentation", IsQuiverRepresentation );
DeclareAttribute( "FieldOfRepresentation", IsQuiverRepresentation );
DeclareAttribute( "VertexDimensions", IsQuiverRepresentation );
DeclareOperation( "VertexDimension", [ IsQuiverRepresentation, IsPosInt ] );
DeclareOperation( "VertexDimension", [ IsQuiverRepresentation, IsVertex ] );
DeclareAttribute( "MatricesOfRepresentation", IsQuiverRepresentation );
DeclareOperation( "MatrixForArrow", [ IsQuiverRepresentation, IsPosInt ] );
DeclareOperation( "MatrixForArrow", [ IsQuiverRepresentation, IsArrow ] );
DeclareOperation( "MatrixForPath", [ IsQuiverRepresentation, IsPath ] );
DeclareOperation( "MatrixForAlgebraElement", [ IsQuiverRepresentation, IsQuiverAlgebraElement ] );


DeclareCategory( "IsQuiverModuleElement", IsAlgebraModuleElement );
DeclareCategory( "IsQuiverModule", IsVectorSpace and IsAlgebraModule and CategoryCollections( IsQuiverModuleElement ) );
DeclareCategory( "IsLeftQuiverModuleElement", IsQuiverModuleElement and IsLeftAlgebraModuleElement );
DeclareCategory( "IsLeftQuiverModule", IsQuiverModule and IsLeftAlgebraModule and CategoryCollections( IsLeftQuiverModuleElement ) );
DeclareCategory( "IsRightQuiverModuleElement", IsQuiverModuleElement and IsRightAlgebraModuleElement );
DeclareCategory( "IsRightQuiverModule", IsQuiverModule and IsLeftAlgebraModule and CategoryCollections( IsRightQuiverModuleElement ) );
DeclareCategory( "IsQuiverBimoduleElement", IsLeftQuiverModuleElement and IsRightQuiverModuleElement );
DeclareCategory( "IsQuiverBimodule", IsLeftQuiverModule and IsRightQuiverModule and CategoryCollections( IsQuiverBimoduleElement ) );

DeclareOperation( "AsModule", [ IsQuiverRepresentation, IsQuiverAlgebra ] );
DeclareOperation( "AsLeftModule", [ IsQuiverRepresentation, IsQuiverAlgebra ] );
DeclareOperation( "AsRightModule", [ IsQuiverRepresentation, IsQuiverAlgebra ] );
DeclareOperation( "AsBimodule", [ IsQuiverRepresentation, IsQuiverAlgebra, IsQuiverAlgebra ] );
DeclareAttribute( "UnderlyingRepresentation", IsQuiverModule );

DeclareOperation( "LeftQuiverModule", [ IsQuiverAlgebra, IsDenseList, IsDenseList ] );
DeclareOperation( "LeftQuiverModuleByArrows", [ IsQuiverAlgebra, IsDenseList, IsDenseList, IsDenseList ] );
DeclareAttribute( "LeftZeroModule", IsQuiverAlgebra );

DeclareOperation( "RightQuiverModule", [ IsQuiverAlgebra, IsDenseList, IsDenseList ] );
DeclareOperation( "RightQuiverModuleByArrows", [ IsQuiverAlgebra, IsDenseList, IsDenseList, IsDenseList ] );
DeclareAttribute( "RightZeroModule", IsQuiverAlgebra );

DeclareAttribute( "QuiverOfModule", IsQuiverModule );
DeclareAttribute( "FieldOfModule", IsQuiverModule );
DeclareAttribute( "VertexDimensions", IsQuiverModule );
DeclareOperation( "VertexDimension", [ IsQuiverModule, IsPosInt ] );
DeclareOperation( "VertexDimension", [ IsQuiverModule, IsVertex ] );

DeclareOperation( "QuiverModuleElement", [ IsQuiverModule, IsDenseList ] );
DeclareOperation( "QuiverModuleElementByVertices", [ IsQuiverModule, IsDenseList, IsDenseList ] );
DeclareOperation( "AsModuleElement", [ IsQuiverRepresentationElement, IsQuiverModule ] );
DeclareAttribute( "UnderlyingRepresentationElement", IsQuiverModuleElement );
DeclareAttribute( "ModuleOfElement", IsQuiverModuleElement );
DeclareAttribute( "ElementVectors", IsQuiverModuleElement );
DeclareOperation( "ElementVector", [ IsQuiverModuleElement, IsPosInt ] );
DeclareOperation( "ElementVector", [ IsQuiverModuleElement, IsVertex ] );

DeclareOperation( "\^", [ IsQuiverAlgebraElement, IsLeftQuiverModuleElement ] );
DeclareOperation( "\^", [ IsRightQuiverModuleElement, IsQuiverAlgebraElement ] );
