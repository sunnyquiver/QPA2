#! @Arguments R1, R2, A1, A2, A3
#! @Returns <Ref Filt="IsQuiverRepresentation"/>
#! @Description
#!  Returns the tensor product of the two representations <A>R1</A> and <A>R2</A>. 
DeclareOperation( "TensorProductOfRepresentations", [ IsQuiverRepresentation, IsQuiverRepresentation, 
        IsQuiverAlgebra, IsQuiverAlgebra, IsQuiverAlgebra ] );
