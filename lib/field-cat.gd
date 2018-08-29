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
DeclareCategory( "IsFieldCategoryMorphism", IsCapCategoryMorphism );

#! @BeginGroup UnderlyingField
#! @Description
#!  Given a <M>k</M>-category, or an object or morphism in a <M>k</M>-category,
#!  returns the field <M>k</M>.
#! @Arguments cat
DeclareAttribute( "UnderlyingField", IsFieldCategory );
#! @Arguments X
DeclareAttribute( "UnderlyingField", IsFieldCategoryObject );
#! @Arguments m
DeclareAttribute( "UnderlyingField", IsFieldCategoryMorphism );
#! @EndGroup

