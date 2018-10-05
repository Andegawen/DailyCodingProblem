// Good morning! Here's your coding interview problem for today.
// This problem was asked by Uber.
// Given an array of integers, return a new array such that each element at index i of the new array is the product of all the numbers in the original array except the one at i.
// For example, if our input was [1, 2, 3, 4, 5], the expected output would be [120, 60, 40, 30, 24]. If our input was [3, 2, 1], the expected output would be [2, 3, 6].
// Follow-up: what if you can't use division?

let solution col =
    let value = col |> Array.reduce (*) 
    col |> Array.map (fun el-> value/el)

//foldi : http://www.fssnip.net/2Z/title/Extensions-to-the-Fold-function
let foldi fold first source  =
       source 
       |> Array.fold(fun (prev,i) c -> (fold i prev c,i + 1)) (first,0)
       |> fst
let solution2 (col:int array) =
    let acc = (Array.create (Array.length col) 1)
    foldi (fun index acc el-> 
            acc |> Array.mapi (fun it elAcc->if it=index then elAcc else elAcc*el) 
          ) acc col

solution [|1;2;3|] = [|6;3;2|]
solution [|1; 2; 3; 4; 5|] = [|120; 60; 40; 30; 24|]

solution2 [|1;2;3|] = [|6;3;2|]
solution2 [|1; 2; 3; 4; 5|] = [|120; 60; 40; 30; 24|]