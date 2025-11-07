// F# utilities library
module Utils

let range start stop = [start..stop]
let sum lst = List.sum lst
let product lst = List.fold (*) 1 lst
let average lst = List.average lst |> float
