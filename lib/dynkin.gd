DeclareOperation( "DynkinGraphAn", [ IsPosInt ] );
DeclareOperation( "DynkinGraphDn", [ IsPosInt ] );
DeclareOperation( "DynkinGraphEn", [ IsPosInt ] );
DeclareOperation( "DynkinGraph", [ IsChar, IsPosInt ] );

DeclareOperation( "EuclideanGraphAn", [ IsPosInt ] );
DeclareOperation( "EuclideanGraphDn", [ IsPosInt ] );
DeclareOperation( "EuclideanGraphEn", [ IsPosInt ] );
DeclareOperation( "EuclideanGraph", [ IsChar, IsPosInt ] );

DeclareOperation( "DynkinOrEuclideanGraph", [ IsChar, IsBool, IsPosInt ] );
DeclareOperation( "DynkinOrEuclideanGraph", [ IsChar, IsBool, IsPosInt, IsString ] );

DeclareOperation( "DynkinQuiver", [ IsDirection, IsString ] );
DeclareOperation( "DynkinQuiver", [ IsDirection, IsChar, IsPosInt ] );
DeclareOperation( "LeftDynkinQuiver", [ IsString ] );
DeclareOperation( "LeftDynkinQuiver", [ IsChar, IsPosInt ] );
DeclareOperation( "RightDynkinQuiver", [ IsString ] );
DeclareOperation( "RightDynkinQuiver", [ IsChar, IsPosInt ] );

DeclareOperation( "EuclideanQuiver", [ IsDirection, IsString ] );
DeclareOperation( "EuclideanQuiver", [ IsDirection, IsChar, IsPosInt ] );
DeclareOperation( "LeftEuclideanQuiver", [ IsString ] );
DeclareOperation( "LeftEuclideanQuiver", [ IsChar, IsPosInt ] );
DeclareOperation( "RightEuclideanQuiver", [ IsString ] );
DeclareOperation( "RightEuclideanQuiver", [ IsChar, IsPosInt ] );

DeclareOperation( "DynkinOrEuclideanQuiver", [ IsDirection, IsString ] );
DeclareOperation( "DynkinOrEuclideanQuiver", [ IsDirection, IsChar, IsBool, IsPosInt ] );
DeclareOperation( "DynkinOrEuclideanQuiver", [ IsDirection, IsChar, IsBool, IsPosInt, IsString ] );
DeclareOperation( "DynkinOrEuclideanQuiver", [ IsDirection, IsChar, IsBool, IsPosInt, IsString, IsList ] );

DeclareOperation( "LeftDynkinOrEuclideanQuiver", [ IsString ] );
DeclareOperation( "LeftDynkinOrEuclideanQuiver", [ IsChar, IsBool, IsPosInt ] );
DeclareOperation( "LeftDynkinOrEuclideanQuiver", [ IsChar, IsBool, IsPosInt, IsString ] );
DeclareOperation( "LeftDynkinOrEuclideanQuiver", [ IsChar, IsBool, IsPosInt, IsString, IsList ] );

DeclareOperation( "RightDynkinOrEuclideanQuiver", [ IsString ] );
DeclareOperation( "RightDynkinOrEuclideanQuiver", [ IsChar, IsBool, IsPosInt ] );
DeclareOperation( "RightDynkinOrEuclideanQuiver", [ IsChar, IsBool, IsPosInt, IsString ] );
DeclareOperation( "RightDynkinOrEuclideanQuiver", [ IsChar, IsBool, IsPosInt, IsString, IsList ] );


DeclareOperation( "ParseDynkinOrEuclideanDescriptionString", [ IsString ] );
