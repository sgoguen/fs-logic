module Tests

open System
open Xunit
open logic.core.Logic

module Utils = 
    let inline assertEqual (expected: 'a) (actual: 'a) =
        Assert.Equal<'a>(expected, actual)

open Utils

[<Fact>]
let ``Basic`` () =
    let example1 q = 
        logic {
            let! y = fresh
            do! y == Int 42
            do! q == y
        }

    let result = run 1 example1
    result |> assertEqual [ Int 42 ]


    let example2 q = 
        logic {
            let! x = fresh
            let! y = fresh
            do! x == Int 42
            do! y == Int 24
            do! q == x
            do! q == y
        }

    run 1 example2 |> assertEqual [ ]

    let example3 q = 
        logic {
            let! x = fresh
            let! y = fresh
            do! x == Int 42
            do! y == Int 24
            do! conde [q == x; q == y]
        }

    run 2 example3 |> assertEqual [ Int 42; Int 24 ]

    let rec fives x = 
        logic {
            do! conde [x == Int 5;
                    logic { return! fives x }]
        }

    run 9 fives |> assertEqual [ Int 5; Int 5; Int 5; Int 5; Int 5; Int 5; Int 5; Int 5; Int 5 ]

    let rec sixes x = 
        logic {
            do! conde [x == Int 6;
                    logic { return! sixes x }]
        }

    run 9 (fun q -> conde [fives q; sixes q])
    |> assertEqual [ Int 5; Int 6; Int 5; Int 6; Int 5; Int 6; Int 5; Int 6; Int 5 ]

    let rec peano n =
        logic {
            do! conde [ Str "z" == n;
                        logic {
                            let! n' = fresh
                            do! Pair (Str "s", n') == n
                            return! peano n'
                        } ]
        }

    run 3 (fun q -> peano q)
    |> assertEqual [Str "z"; Pair (Str "s", Str "z"); Pair (Str "s", Pair (Str "s", Str "z"))]


    let rec appendo l s out =
        logic {
            do! conde [ logic { do! l == Empty
                                do! s == out };
                        logic {
                            let! a, d, res = fresh3 
                            do! Pair (a, d) == l
                            do! Pair (a, res) == out
                            return! appendo d s res
                        }]
        }

    run 2 (fun x -> appendo x (Pair (Int 3, Empty)) (Pair (Int 1, Pair (Int 2, Pair (Int 3, Empty)))))
    |> assertEqual [Pair (Int 1, Pair (Int 2, Empty))]

