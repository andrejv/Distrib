open Distrib

[<EntryPoint>]
let main args =
    (* Throw the dice three times. d is the number of points on all dices. *)
    let d = distrib {
        let! d1 = Distrib.Dice 6
        let! d2 = Distrib.Dice 6
        let! d3 = Distrib.Dice 6
        let s = d1 + d2 + d3
        return s
    }
    (* Probability that we have more tha 15 points. *)
    let p1 = Distrib.prob (fun x -> x > 15) d
    (* Probability that we have exactly 0 points. *)
    let p2 = Distrib.prob (fun x -> x = 9) d
    printf "Probability 1: %f\n" p1
    printf "Probability 2: %f" p2
    0