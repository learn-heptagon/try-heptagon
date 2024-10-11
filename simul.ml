open Obc_interp

module type Interpreter = sig
  val reset : unit -> unit
  val step : value option list -> value option list
end

module type Simulator = sig
  val init : string -> (unit -> unit)
end
