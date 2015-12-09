#!/usr/bin/env fsharpi
    [<AutoOpen>]
    module Utils =
        let indicesToMarkedString width indices zeroMark mark =
            List.scan (fun (prevIndex, _) index -> index, [String.replicate (index - prevIndex - 1) zeroMark; mark] ) (0, []) indices
            |> List.collect snd
            |> String.concat ""
            |> (fun s -> s + String.replicate (width - s.Length) zeroMark)

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

        let displayTree w (tree:int list list) =
            let treeLine oneLocs = indicesToMarkedString w oneLocs " " "."
            (List.replicate (h - tree.Length) [] @ tree) |> List.map treeLine

        tree' max l GrowTrunk [ w/2 ] [] |> displayTree w

    let rows = 63
    let cols = 100
    let length = 16

    let iterations = System.Convert.ToInt32(System.Console.ReadLine())
    tree length cols rows iterations |> List.iter System.Console.WriteLine
