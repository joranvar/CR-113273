#!/usr/bin/env fsharpi
    type TreeAction = Branch | GrowTrunk

    let isEven i = i % 2 = 0

    let tree l w h max =
        let rec tree' max branchLength action roots acc =
            match action with
            | _ when max = 0 || branchLength <= 0 ->
                acc
            | GrowTrunk ->
                let trunks = List.replicate branchLength roots
                let splitRoots x = List.collect (fun elem -> [ elem-1;elem+1 ]) x
                tree' max branchLength Branch (splitRoots roots) (trunks @ acc)
            | Branch ->
                let advanceBranch x = List.mapi (fun i elem -> if isEven i then elem-1 else elem+1) x
                let rec branch top acc = function
                    | i when i <= 0 -> acc
                    | i -> branch (advanceBranch top) (advanceBranch top::acc) (i-1)
                let branches = branch roots [roots] (branchLength-1)
                tree' (max-1) (branchLength/2) GrowTrunk (branches |> List.head) (branches @ acc)

        let displayTree w tree =
            let treeLine oneLocs =
                String.concat "" (Seq.map (fun x -> if List.exists (fun elem -> elem = x) oneLocs then "1" else "_") [1..w])
            List.map treeLine tree

        tree' max l GrowTrunk [ w/2 ] (List.empty)
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
