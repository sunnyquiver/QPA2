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

#! TODO
DeclareOperation( "TensorProductOfHomomorphisms",
        [ IsQuiverModuleHomomorphism, IsQuiverModuleHomomorphism, IsQuiverModule, IsQuiverModule ] );

#! @Arguments M1, M2
#! @Returns <Ref Filt="IsQuiverModule"/> or <Ref Filt="IsQPAVectorSpace"/>
#! @Description
#!  Returns the tensor product of the two modules <A>M1</A> and <A>M2</A>. 
DeclareOperation( "TensorProductOfModules", [ IsQuiverModule, IsQuiverModule ] );

#! @Arguments M, C
#! @Returns IsCapFunctor
#! @Description
#! Creates a tensor functor <M>T = M\otimes - </M> for a module <A>M</A>. The module <A>M</A> 
#! can be a right module <M>M_A</M> or a bimodule <M>_B M_A</M>, and the category <A>C</A> can
#! be <M>A-\mod</M> or <M>A-\mod-C</M>. This gives four possibilities 
#! <Display>
#!         M_A\otimes_A-\colon &amp; A-\mod \to \vec(k)\\
#!         M_A\otimes_A-\colon &amp; A-\mod-C\to \mod-C\\
#!       _BM_A\otimes_A-\colon &amp; A-\mod \to B-\mod\\
#!       _BM_A\otimes_A-\colon &amp; A-\mod-C\to B-\mod-C
#! </Display>
DeclareOperation( "LeftTensorFunctor", [ IsQuiverModule, IsQuiverModuleCategory ] );

#! @Arguments M, C
#! @Returns IsCapFunctor
#! @Description
#! Creates a tensor functor <M>T = -\otimes M</M> for a module <A>M</A>. The module <A>M</A> 
#! can be a left module <M>_AM</M> or a bimodule <M>_A M_B</M>, and the category <A>C</A> can
#! be <M>\mod-A</M> or <M>C-\mod-A</M>. This gives four possibilities 
#! <Display>
#!         -\otimes_A {_AM}-\colon &amp; \mod-A \to \vec(k)\\
#!         -\otimes_A-{_AM}\colon &amp; C-\mod-A\to C-\mod\\
#!         -\otimes_A-{_AM_B}\colon &amp; \mod-A \to \mod-B\\
#!         -\otimes_A-{_AM_B}\colon &amp; C-\mod-A\to C-\mod-B
#! </Display>
DeclareOperation( "RightTensorFunctor", [ IsQuiverModule, IsQuiverModuleCategory ] );

#