// Good morning! Here's your coding interview problem for today.
// This problem was recently asked by Google.
// Given a list of numbers and a number k, return whether any two numbers from the list add up to k.
// For example, given [10, 15, 3, 7] and k of 17, return true since 10 + 7 is 17.
// Bonus: Can you do this in one pass?

let col = [10;5;3;7]
let k = 17

let solution col k =
    let rec solution' col (set:Set<int>) k =
        match col with
        | [] -> false
        | [x] -> set.Contains x
        | x::rest -> if set.Contains x then true else solution' rest (set.Add (k-x)) k
    solution' col Set.empty k

solution col k