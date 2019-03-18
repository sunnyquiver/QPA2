#! @Chapter Combinatorial invariants

#! @Section Unit forms

#! @Description
#!  Category for unit forms.
DeclareCategory( "IsUnitForm", IsMatrix ); 
DeclareCategoryFamily( "IsUnitForm" );
DeclareCategoryCollections( "IsUnitForm" );
DeclareRepresentation( "IsUnitFormRep", IsComponentObjectRep and IsAttributeStoringRep, [ "type" ] );

#! @Description
#!  A unitform is identitied with a symmetric square matrix, where the 
#!  entries along the diagonal are all 2. The bilinear form associated
#!  to the unitform given by a matrix  <A>B</A>  is defined for two vectors  
#!  <M>x</M>  and  <M>y</M>  as:
#!                    <M>x*B*y^T</M>
#!  The quadratic form associated to the unitform given by a matrix  <A>B</A>
#!  is defined for a vector  <M>x</M>  as:
#!                 <M>(1/2) x B x^T</M>
#!  The bilinear and the quadratic forms associated to a unitform  <A>B</A>  are
#!  attributes of the unitform and they are given by 
#!         <C>BilinearFormOfUnitForm(B)</C>
#!  and
#!         <C>QuadraticFormOfUnitForm(B).</C>
#!  The matrix of a unitform is also given as an attribute by
#!         <C>SymmetricMatrixOfUnitForm(B).</C>
#! @Returns <Ref Filt="IsUnitForm"/>
#! @Arguments B
DeclareOperation( "UnitForm", [ IsMatrix ] );

#! @Description
#!  Returns the symmetric matrix defining a unit form.
#! @Returns <C>IsMatrix</C>
#! @Arguments B
DeclareAttribute( "SymmetricMatrixOfUnitForm", IsUnitForm );

#! @Description
#!  Returns the bilinear form defined by a unit form.
#! @Returns function
#! @Arguments B
DeclareAttribute( "BilinearFormOfUnitForm", IsUnitForm );

#! @Description
#!  Returns the quadratic form defined by a unit form.
#! @Returns function
#! @Arguments B
DeclareAttribute( "QuadraticFormOfUnitForm", IsUnitForm );

#! @Description
#!  When a unit form is weakly positive definite, then it has
#!  only a finite number of positive roots. When <Ref Filt="IsWeaklyPositiveUnitForm"/>
#!  has been applied to a unit form with positive result, then this attribute
#!  is set.
#! @Returns <C>IsList</C>
#! @Arguments B
DeclareAttribute( "PositiveRootsOfUnitForm", IsUnitForm );

#! @Description
#!  This function returns the Tits unitform associated to a finite 
#!  dimensional quotient of a path algebra given that the underlying
#!  quiver has no loops or minimal relations that starts and ends in 
#!  the same vertex. That is, then it returns a symmetric matrix  B  
#!  such that for  <M>x = (x_1,\ldots,x_n)</M>
#!  <M>(1/2) (x_1,\dots,x_n)B(x_1,\ldots,x_n)^T =</M>
#!  <M>\sum_{i=1}^n x_i^2 - \sum_{i,j} \dim_k \operatorname{Ext}^1(S_i,S_j)x_ix_j</M>
#!  <M>              + \sum_{i,j} \dim_k \operatorname{Ext}^2(S_i,S_j)x_ix_j,</M> 
#!  where  <M>n</M>  is the number of vertices in  <M>Q</M>.
#! @Returns <Ref Filt="IsUnitForm"/>
#! @Arguments A
DeclareOperation( "TitsUnitFormOfAlgebra", [ IsQuiverAlgebra ] );

#! @Description
#!  This function returns the Euler (non-symmetric) bilinear form 
#!  associated to a finite dimensional (basic) quotient of a path algebra <A>A</A>.
#!  That is,  it returns a bilinear form (function) defined by
#!  <M>\langle x, y\rangle = x \operatorname{CartanMatrix}^(-1)y.</M>
#!  It makes sense only in case <A>A</A> is of finite global dimension.
#! @Returns <Ref Filt="IsUnitForm"/>
#! @Arguments A
DeclareOperation( "EulerBilinearFormOfAlgebra", [ IsQuiverAlgebra ] );

#! @Description
#!  The function computes the reflection of a vector  <A>z</A>  with respect
#!  to a bilinear form given by a matrix  <A>B</A>  in the coordinate  <A>i</A>.
#! @Returns <C>IsVector</C>
#! @Arguments B, i, z
DeclareOperation( "ReflectionByBilinearForm", [ IsUnitForm, IS_INT, IsVector ] );

#! @Description
#!  This function returns true if the unit form is weakly non-negative and false
#!  otherwise.
#! @Returns <C>IsBool</C>
#! @Arguments B
DeclareProperty( "IsWeaklyNonnegativeUnitForm",  IsUnitForm  );

#! @Description
#!  This function returns true if the unit form is weakly positive and false
#!  otherwise. In addition the function computes the roots of the unit form
#!  when true is returned.
#! @Returns <C>IsBool</C>
#! @Arguments B
DeclareProperty( "IsWeaklyPositiveUnitForm",  IsUnitForm  );

#