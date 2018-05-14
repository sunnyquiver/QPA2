#! @Chapter Functors

#! @Section Tensor product

#! @Arguments R1, R2
#! @Returns <Ref Filt="IsQuiverRepresentation"/>
#! @Description
#!  Returns the tensor product of the two representations <A>R1</A> and <A>R2</A>. 
DeclareOperation( "TensorProductOfRepresentations", [ IsQuiverRepresentation, IsQuiverRepresentation ] );

#! @Arguments R
#! @Returns function
#! @Description
#! For the tensor product <A>R</A> of two representations <M>R1</M> and <M>R2</M>, this attribute 
#! returns the function that takes two elements/arguments <M>r_1</M> and <M>r_2</M> in <M>R1</M> 
#! and <M>R2</M>, respectively, and computes <M>r_1\otimes r_2</M> in the tensor product. 
DeclareAttribute( "ElementaryTensorFunction", IsQuiverRepresentation );

#! @Arguments T
#! @Returns list of representations
#! @Description
#! For the tensor product <A>T</A> of two representations <M>R1</M> and <M>R2</M>, this attribute 
#! returns a list of the representations  <M>R1</M> and <M>R2</M>.  
DeclareAttribute( "TensorProductFactors", IsQuiverRepresentation );

#! TODO
DeclareOperation( "TensorProductOfHomomorphisms",
        [ IsQuiverRepresentationHomomorphism, IsQuiverRepresentationHomomorphism, IsQuiverRepresentation, IsQuiverRepresentation ] );

#! @Arguments M1, M2
#! @Returns <Ref Filt="IsQuiverModule"/> or <Ref Filt="IsQPAVectorSpace"/>
#! @Description
#!  Returns the tensor product of the two modules <A>M1</A> and <A>M2</A>. 
DeclareOperation( "TensorProductOfModules", [ IsQuiverModule, IsQuiverModule ] );

#