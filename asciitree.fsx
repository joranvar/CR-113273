#!/usr/bin/env fsharpi
    let isEven i = i % 2 = 0

    //if trunk and counter > 0  -->  write more trunk
    //if trunk and counter = 0 --> split and transition to branch
    //if branch and counter > 0 --> write more branch
    //if branch and counter = 0 --> transition to trunk and deecrement max
    let tree l w h max =
        let advanceBranch x = Array.mapi (fun i elem -> if isEven i then elem-1 else elem+1) x
        let splitRoots x = Array.collect (fun elem -> [|elem-1;elem+1|]) x
        let treeLine oneLocs w =
            String.concat "" (Seq.map (fun x -> if Array.exists (fun elem -> elem = x) oneLocs then "1" else "_") w)

        let rec tree' max original counter branch roots acc =
            match branch with
            | _ when max = 0 ->
                acc
            | false when counter > 0 ->
                tree' max original (counter-1) false roots (acc @ [ treeLine roots (seq { 1 .. w }) ])
            | false when counter = 0 ->
                tree' max original (original-1) true (splitRoots roots) acc
            | true when counter > 0 ->
                tree' max original (counter-1) true (advanceBranch roots) (acc @ [ treeLine roots (seq { 1 .. w })])
            | true when counter = 0 ->
                tree' (max-1) (original/2) (original/2) false roots (acc @ [ treeLine roots (seq { 1 .. w }) ])
        tree' max l l false [| w/2 |] (List.empty)

    //63 rows
    //100 cols
    //16 length

    let fillList notFilled =
        let makeEmptyRowList n =
            let emptyRow = String.replicate 100 "_"
            List.replicate n emptyRow
        notFilled @ makeEmptyRowList (63 - notFilled.Length)

    let iterations = System.Convert.ToInt32(System.Console.ReadLine())
    (tree 16 100 64 iterations)
        |> fillList
        |> List.rev
        |> List.iter System.Console.WriteLine
