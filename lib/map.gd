DeclareOperation( "MapRepresentation", [ IsFunction, IsFunction, IsQuiverRepresentation,
                                         IsQuiverRepresentationCategory ] );
DeclareOperation( "MapRepresentation", [ IsFunction, IsQuiverRepresentation,
                                         IsQuiverRepresentationCategory ] );

#! @Arguments mfun, afun_source, afun_range, m, cat
DeclareOperation( "MapRepresentation", [ IsFunction, IsFunction, IsFunction,
                                         IsQuiverRepresentationHomomorphism,
                                         IsQuiverRepresentationCategory ] );
DeclareOperation( "MapRepresentation", [ IsFunction, IsFunction, IsQuiverRepresentationHomomorphism,
                                         IsQuiverRepresentationCategory ] );
DeclareOperation( "MapRepresentation", [ IsFunction, IsQuiverRepresentationHomomorphism,
                                         IsQuiverRepresentationCategory ] );

DeclareOperation( "MapRepresentation", [ IsFunction, IsFunction, IsDenseList,
                                         IsQuiverRepresentationCategory ] );

DeclareOperation( "MapRepresentation", [ IsFunction, IsDenseList,
                                         IsQuiverRepresentationCategory ] );

DeclareOperation( "MapRepresentation", [ IsFunction, IsFunction, IsFunction, IsDenseList,
                                         IsQuiverRepresentationCategory ] );

DeclareOperation( "MapRepresentation", [ IsCapFunctor, IsQuiverAlgebra ] );
DeclareOperation( "MapRepresentation", [ IsCapFunctor, IsDenseList ] );
