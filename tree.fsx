type Node<'T> = {
    Item: 'T
    Branches: seq<Node<'T>>
    }

[<RequireQualifiedAccess>]
module Tree =

    let rec map (f: 'T -> 'U) (node: Node<'T>): Node<'U> =
        {
            Item = f node.Item
            Branches =
                node.Branches
                |> Seq.map (map f)
        }
    let rec tryFind (f: 'T -> bool) (node: Node<'T>): Option<Node<'T>> =

        if f node.Item
        then Some node
        else
            node.Branches
            |> Seq.tryFind (fun child ->
                tryFind f child
                |> Option.isSome
                    )

    let foo (node: Node<'T>): seq<Node<'T>> =
        // node.Branches
        // |> Seq.collect (fun b -> b.Branches)
        // seq {
        //     yield node
        //     yield! node.Branches
        //     }
        let rec f (x: Node<'T>) =

                x.Branches
                |> Seq.collect f

        f node

let testTree: Node<string> =
    {
        Item = "ROOT"
        Branches =
            [|
                {
                    Item = "A"
                    Branches = [|
                        { Item = "AA"; Branches = [||] }
                        { Item = "AB"; Branches = [||] }
                        { Item = "AC"; Branches = [||] }
                        |]
                }
                {
                    Item = "B"
                    Branches = [| |]
                }
                {
                    Item = "C"
                    Branches = [|
                        { Item = "CA"; Branches = [||] }
                        {
                            Item = "CB"
                            Branches = [|
                                { Item = "CBA"; Branches = [||] }
                                { Item = "CBB"; Branches = [||] }
                                |]
                        }
                        |]
                }
            |]
    }

testTree
|> Tree.tryFind (fun x -> x = "CA")

testTree
|> Tree.foo
|> Array.ofSeq
|> Array.iter (fun x -> printfn "%s" x.Item)