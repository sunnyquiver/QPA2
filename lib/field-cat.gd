#! @Chapter Hom spaces

#! @Section Field categories

#! If <M>k</M> is a field, then a <M>k</M>-category is a category
#! where every hom set is a <M>k</M>-vector space.

#! @Description
#!  GAP category for <M>k</M>-categories.
DeclareCategory( "IsFieldCategory", IsCapCategory );

#! @Description
#!  GAP category for objects in a <M>k</M>-category.
DeclareCategory( "IsFieldCategoryObject", IsCapCategoryObject );

#! @Description
#!  GAP category for morphisms in a  <M>k</M>-category.
DeclareCategory( "IsFieldCategoryMorphism", IsCapCategoryMorphism and IsQPAVector );

#! @BeginGroup UnderlyingField
#! @Description
#!  Given a <M>k</M>-category, or an object or morphism in a <M>k</M>-category,
#!  returns the field <M>k</M>.
#! @Arguments cat
DeclareAttribute( "UnderlyingField", IsFieldCategory );
#! @Arguments X
DeclareAttribute( "UnderlyingField", IsFieldCategoryObject );
#! @Arguments m
if false then
DeclareAttribute( "UnderlyingField", IsFieldCategoryMorphism );
fi;
#! @EndGroup

#! @Description
#!  Given an object <A>X</A> in a field category,
#!  returns the underlying vector space.
#! @Arguments X
DeclareAttribute( "AsQPAVectorSpace", IsFieldCategoryObject );

#! @Description
#!  Given a morphism <A>f</A> in a field category,
#!  returns the corresponding linear transformation on the underlying vector spaces.
#! @Returns <Ref Filt="IsLinearTransformation"/>
#! @Arguments f
DeclareAttribute( "AsLinearTransformation", IsFieldCategoryMorphism );

#
