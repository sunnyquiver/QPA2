#! @Chapter Hom spaces

#! @Section Hom spaces

#! @Description
#!  GAP category for hom spaces.
#! @Label
DeclareCategory( "IsHomSpace", IsQPAVectorSpace );

#! @Description
#!  Gives the hom space <M>\operatorname{Hom}(X,Y)</M> of two objects in a field category.
#! @Returns <Ref Filt="IsHomSpace"/>
#! @Arguments X, Y
#! @Label for two objects
DeclareOperation( "Hom", [ IsFieldCategoryObject, IsFieldCategoryObject ] );

#! @Description
#!  Returns the source of all morphisms in a given hom space.
#!  That is, if the argument <A>hom</A> is <M>\operatorname{Hom}(X,Y)</M>,
#!  then the result is the object <M>X</M>.
#! @Returns IsFieldCategoryObject
#! @Arguments hom
DeclareAttribute( "Source", IsHomSpace );

#! @Description
#!  Returns the source of all morphisms in a given hom space.
#!  That is, if the argument <A>hom</A> is <M>\operatorname{Hom}(X,Y)</M>,
#!  then the result is the object <M>Y</M>.
#! @Returns IsFieldCategoryObject
#! @Arguments hom
DeclareAttribute( "Range", IsHomSpace );

#! @Description
#!  For a given hom space <M>\operatorname{Hom}_{\mathscr{C}}(X, Y)</M>,
#!  returns the category <M>\mathscr{C}</M>.
#! @Returns IsFieldCategory
#! @Arguments hom
DeclareAttribute( "CategoryOfHomSpace", IsHomSpace );


#! @Section Hom spaces of vector spaces

#! @Description
#!  GAP category for hom spaces in vector space categories.
DeclareCategory( "IsVectorSpaceHomSpace", IsHomSpace );

#! @BeginExampleSession
#! gap> V := StandardVectorSpace( Rationals, 3 );
#! Rationals^3
#! gap> W := StandardVectorSpace( Rationals, 2 );
#! Rationals^2
#! gap> hom := Hom( V, W );
#! Hom( Rationals^3, Rationals^2 );
#! gap> IsVectorSpaceHomSpace( hom );
#! true
#! gap> Source( hom );
#! Rationals^3
#! gap> Range( hom );
#! Rationals^2
#! gap> Dimension( hom );
#! 6
#! gap> T := Basis( hom )[ 1 ] + 2 * Basis( hom )[ 3 ];
#! linear transformation 3->2
#! @EndExampleSession


#! @Section Hom spaces of representations

#! @Description
#!  GAP category for hom spaces in quiver representation categories.
DeclareCategory( "IsQuiverRepresentationHomSpace", IsHomSpace );

#! @BeginExampleSession
#! gap> kQ := PathAlgebra( Rationals, LeftQuiver( "Q(3)[a:1->2,b:2->3]" ) );;
#! gap> R1 := QuiverRepresentation( kQ, [ 1, 2, 1 ], [ [ 1, 0 ], [ 0, 1 ] ] );
#! <1,2,1>
#! gap> R2 := QuiverRepresentation( kQ, [ 0, 2, 2 ], [ , [ 0, 1, 1, 0 ] ] );
#! <0,2,2>
#! gap> hom := Hom( R1, R2 );
#! Hom( <1,2,1>, <0,2,2> )
#! gap> IsQuiverRepresentationHomSpace( hom );
#! true
#! @EndExampleSession

#!
DeclareAttribute( "VertexHomSpaces", IsQuiverRepresentationHomSpace );

#!
DeclareAttribute( "ArrowHomSpaces", IsQuiverRepresentationHomSpace );

#!
DeclareAttribute( "SumOfVertexHomSpaces", IsQuiverRepresentationHomSpace );

#!
DeclareAttribute( "SumOfArrowHomSpaces", IsQuiverRepresentationHomSpace );

#!
DeclareAttribute( "HomSpaceVertexToArrowMap", IsQuiverRepresentationHomSpace );


#! @Section Hom spaces of modules

#!
DeclareCategory( "IsQuiverModuleHomSpace", IsHomSpace and IsObjectWithSide );

#!
DeclareCategory( "IsLeftQuiverModuleHomSpace", IsQuiverModuleHomSpace );

#!
DeclareCategory( "IsRightQuiverModuleHomSpace", IsQuiverModuleHomSpace );

#!
DeclareCategory( "IsQuiverBimoduleHomSpace", IsQuiverModuleHomSpace );

#!
DeclareAttribute( "UnderlyingRepresentationHomSpace", IsQuiverModuleHomSpace );
