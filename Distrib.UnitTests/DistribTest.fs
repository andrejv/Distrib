module UnitTests

open NUnit.Framework
open Distrib

let epsilon = 1e-10

[<SetUp>]
let Setup () =
    ()

[<Test>]
let DiceExpectedValue () =
    let d = Distrib.Dice 6
    let e = Distrib.expected_value d
    Assert.That(e, Is.EqualTo(3.5).Within(epsilon))

[<Test>]
let Dice2ExpectedValue() =
    let d = distrib {
        let! d1 = Distrib.Dice 6
        let! d2 = Distrib.Dice 6
        let s = d1 + d2
        return s
    }
    let e = Distrib.expected_value d
    Assert.That(e, Is.EqualTo(7).Within(epsilon))

[<Test>]
let Yarn1 () =
    let d = distrib {
        let! b1 = Distrib.Yarn 3 5
        let! b2 = Distrib.Yarn 5 3
        let s = b1 = b2
        return s
    }
    let p = Distrib.prob (fun x -> x = true) d
    Assert.That(p, Is.EqualTo(3.0/8.0 * 5.0/8.0 + 5.0/8.0 * 3.0/8.0).Within(epsilon))

[<Test>]
let Phases1 () =
    let d = distrib {
        let! f1 = Distrib.Yarn 2 3
        let! f2 = if f1 = Black then Distrib.Yarn 3 4 else Distrib.Yarn 2 5
        return f2
    }
    let p = Distrib.prob (fun x -> x = Black) d
    Assert.That(p, Is.EqualTo(2.0 / 5.0 * 3.0 / 7.0 + 3.0 /5.0 * 2.0 / 7.0).Within(epsilon))
