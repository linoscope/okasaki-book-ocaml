module type S = sig
  type key
  type 'a t

  val empty : 'a t
  val bind : 'a t -> key:key -> value:'a -> 'a t
  val lookup : 'a t -> key:key -> 'a
end
