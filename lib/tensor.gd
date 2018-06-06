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

#! @Arguments M, cat
#! @Returns IsCapFunctor
#! @Description
#! Creates a tensor functor <M>T = M\otimes - </M> for a module <A>M</A>.
#! The argument <A>cat</A> is the source category of the functor.
#!
#! The module <A>M</A> is either a right module <M>M_A</M> or a bimodule
#! <Alt Not="Text"><M>_B M_A</M></Alt><Alt Only="Text">B_M_A</Alt>,
#! and the category <A>cat</A> is either
#! a left module category <M>A\text{-mod}</M> or a bimodule category <M>A\text{-mod-}C</M>.
#! This gives four possibilities for the resulting functor:
#! <Alt Not="Text">
#! <Display>
#! \begin{aligned}
#!         M_A \otimes_A - \colon &amp; A\text{-mod}   \to \operatorname{vec} k \\
#!         M_A \otimes_A - \colon &amp; A\text{-mod-}C \to \text{mod-}C         \\
#!       _BM_A \otimes_A - \colon &amp; A\text{-mod}   \to B\text{-mod}         \\
#!       _BM_A \otimes_A - \colon &amp; A\text{-mod-}C \to B\text{-mod-}C
#! \end{aligned}
#! </Display>
#! </Alt>
#! <Alt Only="Text">
#! <Verb>
#!   M_A tensor - : A-mod   -> vec k
#!   M_A tensor - : A-mod-C -> mod-C
#! B_M_A tensor - : A-mod   -> B-mod
#! B_M_A tensor - : A-mod-C -> B-mod-C
#! </Verb>
#! </Alt>
DeclareOperation( "LeftTensorFunctor", [ IsQuiverModule, IsQuiverModuleCategory ] );

#! @Arguments M, C
#! @Returns IsCapFunctor
#! @Description
#! Creates a tensor functor <M>T = - \otimes M</M> for a module <A>M</A>.
#! The argument <A>cat</A> is the source category of the functor.
#!
#! The module <A>M</A> is either a left module
#! <Alt Not="Text"><M>{}_A M</M></Alt><Alt Only="Text">A_M</Alt> or a bimodule
#! <Alt Not="Text"><M>{}_A M_B</M></Alt><Alt Only="Text">A_M_B</Alt>,
#! and the category <A>cat</A> is either
#! a right module category <M>\text{mod-}A</M> or a bimodule category <M>C\text{-mod-}A</M>.
#! This gives four possibilities for the resulting functor:
#! <Alt Not="Text">
#! <Display>
#! \begin{aligned}
#!   -\otimes_A {}_A M   \colon &amp;   \text{mod-}A \to \operatorname{vec} k \\
#!   -\otimes_A {}_A M   \colon &amp; C\text{-mod-}A \to C-\text{-mod}        \\
#!   -\otimes_A {}_A M_B \colon &amp;   \text{mod-}A \to    \text{mod-}B      \\
#!   -\otimes_A {}_A M_B \colon &amp; C\text{-mod-}A \to C-\text{-mod-}B
#! \end{aligned}
#! </Display>
#! </Alt>
#! <Alt Only="Text">
#! <Verb>
#! -tensor A_M   : mod-A   -> vec k
#! -tensor A_M   : C-mod-A -> C-mod
#! -tensor A_M_B : mod-A   -> mod-B
#! -tensor A_M_B : C-mod-A -> C-mod-B
#! </Verb>
#! </Alt>
DeclareOperation( "RightTensorFunctor", [ IsQuiverModule, IsQuiverModuleCategory ] );

#