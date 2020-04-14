/*

Some handy Magma utilities.

NB some of these may make questionable choices.

*/
/*

======= Hash functions for Lists and tuples =======

*/
intrinsic Hash(T::Tup) -> RngIntElt
  {
  Hash for a tuple.
  }
  try
    return &+[Hash(t) : t in T];
  catch e
    return 1;
  end try;
end intrinsic;

intrinsic Hash(T::List) -> RngIntElt
  {
  Hash for a list.
  }
  try
    return &+[Hash(t) : t in T];
  catch e
    return 1;
  end try;
end intrinsic;
/*

======= Functions for associative arrays =======

*/
intrinsic Hash(A::Assoc) -> RngIntElt
  {
  Hash for an associative array.
  }
  try
    return &+[ Hash(<k, A[k]>) : k in Keys(A)];
  catch e
    return 1;
  end try;
end intrinsic;

intrinsic 'eq'(A::Assoc, B::Assoc) -> BoolElt
  {
  Equality for associative arrays.
  }
  return (Universe(A) cmpeq Universe(B)) and (Keys(A) eq Keys(B)) and forall{ k : k in Keys(A) | A[k] cmpeq B[k] };
end intrinsic;
/*

======= GroupName for older versions of magma =======

*/
intrinsic DirectoryGroupName(G::GrpPerm) -> MonStgElt
  {
  Returns a group name suitable for using as a directory name.  If GroupName is defined in magma (roughly version 2.21 or above) it returns GroupName, otherwise it returns order_num, where <order, num> is given by IdentifyGroup. A hash is used in place of a colon.
  }
  try
    name := eval("GroupName(G)");
    // magma/linux/something messes up directory names with colons, so need to substitute these...
    name := Join(Split(name, ":"), "#");
  catch e
    ord, num := Explode(IdentifyGroup(G));
    name := Sprintf("%o_%o", ord, num);
  end try;
  return name;
end intrinsic;
/*

======= Functions on sets and sorting =======

*/
function lenlex_sort(x,y)
  if #x-#y ne 0 then
    return #x-#y;
  elif x eq y then
    return 0;
  else
    try
      assert exists(i){i : i in [1..#x] | x[i] ne y[i]};
      return x[i] lt y[i] select -1 else 1;
    catch e;
      return 0; // If we can't compare, then select 0;
    end try;
  end if;
end function;

intrinsic Subsets(S::SetIndx:empty := true) -> SetIndx
  {
  Returns the indexed subsets of S ordered by length and lexicographically wrt S. Optional parameter empty, controls whether the empty set is returned.
  }
  S_Sort := func<x,y | Position(S, x) - Position(S, y)>;
  function sub_Sort(x,y)
    if #x-#y ne 0 then
      return #x-#y;
    elif x eq y then
      return 0;
    else
      assert exists(i){i : i in [1..#x] | x[i] ne y[i]};
      return S_Sort(x[i], y[i]);
    end if;
  end function;
  
  subsets := Subsets(Set(S));
  if not empty then
    subsets diff:= {{}};
  end if;
  
  return Sort({@ Sort(IndexedSet(T), S_Sort) : T in subsets @}, sub_Sort);
end intrinsic;
