functor PQSortFn(structure Q : PQUEUE) :> SORT where type elem = Q.elem =
struct 
  type elem = Q.elem	
  val compare = Q.compare_elem
  fun sort xs =
  	let val pQ = foldr (fn (elem, queue) => Q.insert(elem, queue)) Q.empty xs
  		fun sorter (pq) =
  		    let val (min, rest) = Q.deletemin(pQ)
  			in if(Q.isEmpty(pq)) then []
  				else min::sorter(rest)
  			end 
  	in sorter pQ
  	end
end