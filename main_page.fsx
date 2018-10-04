//There's a staircase with N steps, and you can climb 1 or 2 steps at a time. Given N, write a function that returns the number of unique ways you can climb the staircase. The order of the steps matters.

//For example, if N is 4, then there are 5 unique ways:

// 1, 1, 1, 1
// 2, 1, 1
// 1, 2, 1
// 1, 1, 2
// 2, 2
// What if, instead of being able to climb 1 or 2 steps at a time, 
// you could climb any number from a set of positive integers X? 
// For example, if X = {1, 3, 5}, you could climb 1, 3, or 5 steps at a time.
// Generalize your function to take in X.

let solution (n:int) (x:Set<int>) = 
    let rec solution' n x = 
        let newN = 
            n 
            |> List.collect (fun el->Set.map (fun xi->el-xi) x |> Set.toList)
        let p1 = newN |> List.filter (fun x->x=0) |> List.length
        let newNfiltered = newN |> List.filter (fun el -> el>0)
        if not (List.isEmpty newNfiltered) then p1+(solution' newNfiltered x) else p1
    solution' [n] x


solution 1 (Set.empty.Add(1).Add(2)) = 1
solution 2 (Set.empty.Add(1).Add(2)) = 2
solution 3 (Set.empty.Add(1).Add(2)) = 3
solution 4 (Set.empty.Add(1).Add(2)) = 5