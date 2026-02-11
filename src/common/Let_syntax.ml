(** See https://ocaml.org/manual/5.2/bindingops.html#ss%3Aletops-conventions
    This is an alternative to the [let%bind] and [let%map] syntax from ppx_let:
    https://blog.janestreet.com/let-syntax-and-why-you-should-use-it/ *)

module Result = struct
  let ( let* ) = Core.Result.( >>= )
  let ( let+ ) = Core.Result.( >>| )
end

module Option = struct
  let ( let* ) = Core.Option.( >>= )
  let ( let+ ) = Core.Option.( >>| )
end

module Context = struct
  (** Useful for callbacks, like when using [Core.With_return]

      Something like [With_return.with_return (fun {return} -> ...)] can be
      rewritten as [let@ {return} = With_return.with_return in ...]

      This is similar to the [use] construct in the Gleam language
      https://tour.gleam.run/advanced-features/use/ *)

  let ( let@ ) = ( @@ )
end
