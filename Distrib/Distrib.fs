namespace Distrib

type T<'A> = Distrib of ('A * float) list
type Coin = Head | Tail
type Disc = White | Black

module private DistribUtils =

    let rec flatten = function
    | [] -> []
    | hd::tl -> hd @ flatten tl

    let bin n k =

        let rec bin acc n = function
        | 0 -> 1
        | 1 -> n
        | k -> 
            bin (n * acc / k) (n-1) (k-1)

        if 2*k > n then 
            bin 1 n (n-k)
        else
            bin 1 n k

module Distrib = 

    (* Distribution functions *)

    let print_distrib (Distrib lst) =
        printf "("
        List.map (fun (x,p) -> printf "%6i" x) lst |> ignore
        printfn ")"
        printf "("
        List.map (fun (x,p) -> printf " %3f" p) lst |> ignore
        printfn ")"

    /// Map over distribution
    let map f (Distrib lst) =
        Distrib (List.map (fun (x,p) -> (f x, p)) lst)

    /// Functional probability computation
    let prob f (Distrib lst) =
        List.fold (fun acc (x,p) -> if (f x) then acc+p else acc) 0.0 lst

    /// Computes the expected value of the distribution
    let expected_value (Distrib lst) =
        List.sumBy (fun (x,p) -> p*(float x)) lst

    /// Sorts and removes duplicate values from distribution table
    let normalize (Distrib lst) =
        let rec nub = function
            | [] -> []
            | [a] -> [a]
            | (x,p)::(y,q)::tl ->
                if x=y then nub ((x,p+q) :: tl)
                else (x,p) :: nub ((y,q)::tl)
        Distrib (List.sort lst |> nub)

    (* Distribution definitions *)

    /// Bernoulli experiment
    let Bernoulli p = Distrib [ (1, p); (0, 1.0 - p) ]

    /// A uniform choice from the list
    let Choice lst =
        let p = 1.0 / (float (List.length lst))
        Distrib (List.map (fun x -> (x,p)) lst)

    /// A throw of a fair dice with n sides
    let Dice n =
        let lst = [for x in 1 .. n -> x]
        Choice lst

    /// Represents a throw of a coin
    let Coin () = Choice [Head; Tail]

    /// Represents a draw from a yarn with black and white discs
    let Yarn black white =
        let p = (float black) / (float ( black + white))
        Distrib [(Black, p); (White, 1.0 - p)]

    /// Binomial distribution
    let Binomial n p =
        Distrib [for k in 0 .. n -> (k, p**(float k) * (float (DistribUtils.bin n k)))]

(* Distribution monad *)
module private DistribMonad =
    let Bind (Distrib lst) f =

        let b = List.map (fun (x,p) ->
                            let (Distrib fx) = f x
                            List.map (fun (y,q) -> (y,p*q)) fx)
                         lst
                |> DistribUtils.flatten 

        Distrib b |> Distrib.normalize

    let Return x = Distrib [ (x, 1.0) ]

(* Computational expressions builder *)
[<AutoOpen>]
module DistribBuilderModule =

    type DistribBuilder () =
        member x.Bind(comp, func) = DistribMonad.Bind comp func
        member x.Return(value) = DistribMonad.Return value

    let distrib = new DistribBuilder()
