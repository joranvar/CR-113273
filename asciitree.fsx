#!/usr/bin/env fsharpi
    let isEven i = i % 2 = 0

    //if trunk and counter > 0  -->  write more trunk
    //if trunk and counter = 0 --> split and transition to branch
    //if branch and counter > 0 --> write more branch
    //if branch and counter = 0 --> transition to trunk and deecrement max
    let tree l w h max =
        let rec tree' max original branch roots acc =
            match branch with
            | _ when max = 0 ->
                acc
            | false ->
                let trunks = List.replicate original roots
                let splitRoots x = List.collect (fun elem -> [ elem-1;elem+1 ]) x
                tree' max original true (splitRoots roots) (trunks @ acc)
            | true ->
                let advanceBranch x = List.mapi (fun i elem -> if isEven i then elem-1 else elem+1) x
                let rec branch top acc = function
                    | i when i <= 0 -> acc
                    | i -> branch (advanceBranch top) (advanceBranch top::acc) (i-1)
                let branches = branch roots [roots] (original-1)
                tree' (max-1) (original/2) false (branches |> List.head) (branches @ acc)

        let displayTree w tree =
            let treeLine oneLocs =
                String.concat "" (Seq.map (fun x -> if List.exists (fun elem -> elem = x) oneLocs then "1" else "_") [1..w])
            List.map treeLine tree

        tree' max l false [ w/2 ] (List.empty)
        |> List.rev
        |> displayTree w

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
