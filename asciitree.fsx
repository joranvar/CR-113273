#!/usr/bin/env fsharpi

    let iterations = System.Convert.ToInt32(System.Console.ReadLine())

    //63 rows
    //100 cols
    //16 length

    let splitRoots x = Array.collect (fun elem -> [|elem-1;elem+1|]) x

    let isEven i = if i % 2 = 0 then true else false

    let advanceBranch x = Array.mapi (fun i elem -> if isEven i then elem-1 else elem+1) x

    let treeLine oneLocs w =
        String.concat "" (Seq.map (fun x -> if Array.exists (fun elem -> elem = x) oneLocs then "1" else "_") w)

    //if trunk and counter > 0  -->  write more trunk
    //if trunk and counter = 0 --> split and transition to branch
    //if branch and counter > 0 --> write more branch
    //if branch and counter = 0 --> transition to trunk and deecrement max
    let rec tree l w h max original counter branch roots acc =
        match branch with
        | _ when max = 0 ->
            acc
        | false when counter > 0 ->
            tree l w h max original (counter-1) false roots (acc @ [ treeLine roots (seq { 1 .. w }) ])
        | false when counter = 0 ->
            tree l w h max original (original-1) true (splitRoots roots) acc
        | true when counter > 0 ->
            tree l w h max original (counter-1) true (advanceBranch roots) (acc @ [ treeLine roots (seq { 1 .. w })])
        | true when counter = 0 ->
            tree l w h (max-1) (original/2) (original/2) false roots (acc @ [ treeLine roots (seq { 1 .. w }) ])

    let emptyRow = String.concat "" (Seq.map (fun x -> "_") (seq{ 1..100}))

    let makeEmptyRowList n = [1..n] |> List.map (fun x -> emptyRow)

    let fillList notFilled = (fun x -> x @ (makeEmptyRowList (63-x.Length))) notFilled

    (tree 16 100 64 iterations 16 16 false [| 50 |] (List.empty))
        |> fillList
        |> List.rev
        |> List.iter System.Console.WriteLine
