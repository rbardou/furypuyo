module T: sig
  type 'a t
  val x: unit -> 'a t
  val f: 'a t -> 'a -> unit
end = struct
  type 'a t = unit
  val x () = ()
  let f _ _ = ()
end
