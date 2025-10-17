DeclareCategory( "IsDirection", IsObject );
DeclareCategory( "IsSide", IsObject );

DeclareAttribute( "Opposite", IsDirection );

DeclareOperation( "\^", [ IsOperation, IsSide ] );

DeclareOperation( "DeclareDirectionOperations", [ IsOperation, IsOperation, IsOperation ] );
DeclareOperation( "DeclareSideOperations", [ IsOperation, IsOperation, IsOperation, IsOperation ] );

DeclareOperation( "InstallMethodWithDirections", [ IsOperation, IsDenseList, IsFunction ] );
DeclareOperation( "InstallMethodWithSides", [ IsOperation, IsDenseList, IsFunction ] );

DeclareCategory( "IsObjectWithDirection", IsObject );
DeclareCategory( "IsObjectWithSide", IsObject );
DeclareOperation( "\^", [ IsObjectWithDirection, IsDirection ] );
DeclareOperation( "\^", [ IsObjectWithSide, IsSide ] );
DeclareAttribute( "Direction", IsObjectWithDirection );
DeclareAttribute( "Side", IsObjectWithSide );
