module Pentive.Client.Counter

open Bolero



let flip f b a = f a b

let map get set f a =
  a |> get |> f |> flip set a


type Counter =
  { Value: int }

type CounterMsg =
  | Increment
  | Decrement
  | SetValue of int


module Counter =
  module Value =
    let get m = m.Value
    let set v m = { m with Value = v }
    let map = map get set
    
  let init =
    { Value = 0 }

  let update = function
    | Increment -> (+)  1 |> Value.map
    | Decrement -> (+) -1 |> Value.map
    | SetValue value -> value |> Value.set


type CounterTemplate = Template<"wwwroot/counter.html">

let view model dispatch =
    CounterTemplate()
        .Decrement(fun _ -> dispatch Decrement)
        .Increment(fun _ -> dispatch Increment)
        .Value(model.Value, dispatch << SetValue)
        .Elt()
