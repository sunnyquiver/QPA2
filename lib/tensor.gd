#! @Chapter Functors

#! @Section Tensor product

#! @Arguments R1, R2, A1, A2, A3
#! @Returns <Ref Filt="IsQuiverRepresentation"/>
#! @Description
#!  Returns the tensor product of the two representations <A>R1</A> and <A>R2</A>. 
DeclareOperation( "TensorProductOfRepresentations", [ IsQuiverRepresentation, IsQuiverRepresentation, 
        IsQuiverAlgebra, IsQuiverAlgebra, IsQuiverAlgebra ] );
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

#