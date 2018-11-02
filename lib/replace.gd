#! @Description
#!  Replace the element at index <A>i</A> in the list <A>list</A>
#!  by <A>obj</A>.
#!
#!  The result is a new list.  The original list is not modified.
#! @Arguments list, i, obj
#! @Returns list
DeclareOperation( "Replace", [ IsDenseList, IsPosInt, IsObject ] );

#! @Description
#!  Replace the elements at indices <A>is</A> in the list <A>list</A>
#!  by the objects <A>objs</A>.
#!
#!  The result is a new list.  The original list is not modified.
#!  The two arguments <A>is</A> and <A>objs</A> should be lists of the same length.
#! @Arguments list, is, objs
#! @Returns list
DeclareOperation( "Replace", [ IsDenseList, IsDenseList, IsDenseList ] );

#! @Description
#!  Replace the object <A>value</A> by <A>replacement</A>
#!  wherever it occurs in <A>list</A>.
#!
#!  The result is a new list.  The original list is not modified.
#! @Arguments list, value, replacement
#! @Returns list
DeclareOperation( "ReplaceObj", [ IsList, IsObject, IsObject ] );
