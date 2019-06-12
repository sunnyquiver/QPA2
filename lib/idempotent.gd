#! @Chapter General algebra functions

#! @Section Idempotents

#! @Description
#! This function takes as an argument a finite dimensional simple 
#!  algebra  <A>A</A>  over a finite field and returns a list of two elements  
#!  <M>[a, b]</M>  in  <A>A</A>  such that  <M>a*b = 0</M>  in <A>A</A>, if  <A>A</A> is not 
#!  commutative. If  <A>A</A>  is commutative, it returns an empty list.
#! @Arguments A
#! @Returns a list of two algebra elements
DeclareOperation( "ZeroDivisor", [ IsSimpleAlgebra ] );

#! @Description
#! This function takes as an argument a finite dimensional algebra 
#! <A>A</A>  and returns a left identity for  <A>A</A>, if such a thing exists.   
#! Otherwise it returns fail.
#! @Arguments A
#! @Returns one element in an algebra
DeclareOperation( "LeftIdentity", [ IsAlgebra ] );

#! @Description
#! This function takes as an argument a finite dimensional simple 
#!  algebra  <A>A</A>  and returns a primitive idempotent for  <A>A</A>.
#! @Arguments A
#! @Returns one element in an algebra
DeclareOperation( "SingularIdempotent", [ IsSimpleAlgebra ] );

#! @Description
#! This function takes as an argument a finite dimensional  
#! algebra  <A>A</A>  over a finite field and returns a complete set of 
#! primitive idempotents  <M>\{ e_i \}</M>  such that  
#!          <M>\[A \simeq  Ae_1 + ... + Ae_n.\]</M>
#! @Arguments A
#! @Returns a list elements in an algebra
DeclareOperation( "CompleteSetOfPrimitiveIdempotents", [ IsAlgebraWithOne ] );

#! @Description
#! Given an onto homomorphism  <A>f</A>  of finite dimensional algebras and
#! an idempotent  <A>v</A>  in the range of  <A>f</A>, such that the kernel of  
#! <A>f</A>  is nilpotent, this function returns an idempotent  <M>e</M> in the 
#! source of  <A>f</A>, such that  <M>f(e) = v</M>.
#! @Arguments f, v
#! @Returns an element in an algebra
DeclareOperation( "LiftIdempotent", [ IsAlgebraGeneralMapping, IsRingElement ] );

#! @Description
#! Given an onto homomorphism  <A>f</A>  of finite dimensional algebras, an 
#! idempotent  <A>v</A>  in the source of  <A>f</A>  and an idempotent  <A>w</A>  in 
#! the range of  <A>f</A>, such that the kernel of  <A>f</A>  is nilpotent and 
#! <M>f(v)</M>  and  <A>w</A>  are two ortogonal idempotents in the range of  <A>f</A>, 
#! this function returns an idempotent  <M>e</M>  in the source of  <A>f</A>, such
#! that  <M>\{v, e\}</M>  is a pair of orthogonal idempotents in the source of 
#! <A>f</A>. 
#! @Arguments f, v, w
#! @Returns a list of two elements in an algebra
DeclareOperation( "LiftTwoOrthogonalIdempotents", [ IsAlgebraGeneralMapping, IsRingElement, IsRingElement ] );
#! @Description
#! Given a map  <A>map</A><M>\colon A \to B</M>  and a complete set  <A>idempotents</A>
#! of orthogonal idempotents in  <M>B</M>, which all are in the image
#! of  <A>map</A>,  this function computes (when possible) a complete set of
#! orthogonal idempotents of preimages of the idempotents in <M>B</M>.
#! @Arguments map, idempotents 
#! @Returns a list of two elements in an algebra
DeclareOperation( "LiftCompleteSetOfOrthogonalIdempotents", [ IsAlgebraGeneralMapping, IsHomogeneousList ] );

#