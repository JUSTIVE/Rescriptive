module type RegexGroupParserTarget = {
  type t
  let make: string => option<t>
}

// module RegexGroupParserTarget2 = {
//   type t ='a
// }

module RegexGroupParserTargetString: RegexGroupParserTarget = {
  type t = string
  let make = (s: string) => s->Some
}

module RegexGroupParserTargetInt: RegexGroupParserTarget = {
  type t = int
  let make = (s: string) => s->Belt.Int.fromString
}

module Make = (ParserTarget: RegexGroupParserTarget) => {
  type t = {
    re: Js.Re.t,
    groupNumber: array<int>,
  }
  let make = (re, groupNumber): t => {
    re: re,
    groupNumber: groupNumber,
  }
  let parse = (t, value) => {
    t.re
    ->Js.Re.exec_(value)
    ->Belt.Option.map(Js.Re.captures)
    ->Belt.Option.map(Js.Array.map(Js.Nullable.toOption))
    ->Belt.Option.flatMap(Rscv_Array.everyO)
    ->Belt.Option.flatMap(x => x->Rscv_Array.take(t.groupNumber))
    ->Belt.Option.map(x => x->Belt.Array.map(x => ParserTarget.make(x)))
  }
}

module String = Make(RegexGroupParserTargetString)
module Int = Make(RegexGroupParserTargetInt)

String.make(%re("/([a-z]+)(\d+)/"), [1, 2])->String.parse("abc123")->Js.log
