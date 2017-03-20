DeclareCategory( "IsDirection", IsObject );
DeclareCategory( "IsSide", IsObject );

DeclareGlobalVariable( "LEFT" );
DeclareGlobalVariable( "RIGHT" );
DeclareGlobalVariable( "LEFT_RIGHT" );

DeclareGlobalVariable( "OPERATIONS_WITH_SIDE_VERSIONS" );

DeclareOperation( "\^", [ IsOperation, IsSide ] );

DeclareOperation( "DeclareDirectionOperations", [ IsOperation, IsOperation, IsOperation ] );
DeclareOperation( "DeclareSideOperations", [ IsOperation, IsOperation, IsOperation, IsOperation ] );

DeclareOperation( "InstallMethodWithDirections", [ IsOperation, IsDenseList, IsFunction ] );
DeclareOperation( "InstallMethodWithSides", [ IsOperation, IsDenseList, IsFunction ] );

DeclareOperation( "Replace", [ IsList, IsObject, IsObject ] );

DeclareCategory( "IsObjectWithDirection", IsObject );
DeclareCategory( "IsObjectWithSide", IsObject );
DeclareOperation( "\^", [ IsObjectWithDirection, IsDirection ] );
DeclareOperation( "\^", [ IsObjectWithSide, IsSide ] );
DeclareAttribute( "Direction", IsObjectWithDirection );
DeclareAttribute( "Side", IsObjectWithSide );
