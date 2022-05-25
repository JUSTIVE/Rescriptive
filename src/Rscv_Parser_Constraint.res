module type ConstraintParserTarget = {
  type t
  type rule
  let make: array<rule> => array<rule>
  let parseRule: (rule, t) => option<t>
  let parse: (array<rule>, t) => option<t>
}

let commonParseRuleReducer: array<option<'t>> => option<'t> = (
  parseRuleResults: array<option<'t>>,
) => {
  switch parseRuleResults->Js.Array2.every(Belt.Option.isSome) {
  | true => parseRuleResults->Belt.Array.get(0)->Rscv_Option.flatten
  | false => None
  }
}

module StringConstraint: ConstraintParserTarget = {
  type t = string
  type rule =
    | Length(int)
    | Kind(array<string>)
    | StartsWith(string)
    | Contains(string)

  let make = (s: array<rule>) => s

  let parseRule = (rule, value) =>
    switch rule {
    | Length(length) if value->Js.String.length == length => Some(value)
    | Kind(kinds) if kinds->Rscv_Array.has(value) => Some(value)
    | StartsWith(startsWith) if value->Js.String2.startsWith(startsWith) => Some(value)
    | _ => None
    }
  let parse = (rules: array<rule>, value) =>
    rules->Belt.Array.map(rule => rule->parseRule(value))->commonParseRuleReducer
}

module IntConstraint: ConstraintParserTarget = {
  type t = int
  type rule =
    | DigitLength(int)
    | Range(int, int)

  let make = (s: array<rule>) => s

  let parseRule = (rule, value) =>
    switch rule {
    | DigitLength(length) => value->Rscv_Math.Int.digitLength == length ? Some(value) : None
    | Range(min, max) =>
      switch value {
      | x if x->Rscv_Math.Int.isInRange((min, max)) => Some(x)
      | _ => None
      }
    }
  //   | NumericRange(min, max) =>
  //     switch value {
  //     | x if x->MSUtil_Math.Int.isInRange((min, max)) => Some(x)
  //     | _ => None
  //     }
  let parse = (rules: array<rule>, value) =>
    rules->Belt.Array.map(rule => rule->parseRule(value))->commonParseRuleReducer
}

module Make = (ParserTarget: ConstraintParserTarget) => {
  // type stringParserRule =
  //   | StringLength(int)
  //   | StringKind(array<string>)
  //   | StringStartsWith(string)
  //   | StringContains(string)

  // type stringParser = {stringRules: array<stringParserRule>}
  // type numericParserRule =
  //   | NumericLength(int)
  //   | NumericRange(int, int)
  // type numericParser = {
  //   radix: int,
  //   numericRules: array<numericParserRule>,
  // }
  // type t =
  //   | NumericParser(numericParser)
  //   | StringParser(stringParser)

  // let parseNumberConstraint = (value, numericParserRule) =>
  //   switch numericParserRule {
  //   | NumericLength(length) if value->MSUtil_Math.Int.length == length => Some(value)
  //   | NumericRange(min, max) =>
  //     switch value {
  //     | x if x->MSUtil_Math.Int.isInRange((min, max)) => Some(x)
  //     | _ => None
  //     }
  //   | _ => None
  //   }

  // let parseStringConstraint = (value, stringParserRule: stringParserRule) =>
  //   switch stringParserRule {
  //   | StringLength(length) if value->Js.String.length == length => Some(value)
  //   | StringKind(kinds) if kinds->MSUtil_Array.has(value) => Some(value)
  //   | StringStartsWith(startsWith) if value->Js.String2.startsWith(startsWith) => Some(value)
  //   | _ => None
  //   }

  // let parse = (parser, value) => {
  //   switch parser {
  //   | NumericParser({radix, numericRules}) =>
  //     numericRules
  //     ->Belt.Array.map(x => parseNumberConstraint(_, x))
  //     ->Belt.Array.reduce(value->MSUtil_String.parseInt(radix), (x, y) =>
  //       x->Belt.Option.flatMap(x => y(x))
  //     )
  //     ->Belt.Option.map(x => x->ParserResult.ParseResInt)
  //   | StringParser({stringRules}) =>
  //     stringRules
  //     ->Belt.Array.map(x => parseStringConstraint(_, x))
  //     ->Belt.Array.reduce(Some(value), (x, y) => x->Belt.Option.flatMap(x => y(x)))
  //     ->Belt.Option.map(x => x->ParserResult.ParseResString)
  //   }
  // }
  let parse = (parser, value) => ParserTarget.parse(parser, value)
  let make = ParserTarget.make
}

module String = Make(StringConstraint)

// StringConstraint.Length(3)

// IntConstraint.make([
//   IntConstraint.DigitLength(3),
//   IntConstraint.Range(1, 10),
// ])
// let x:IntConstraint.t = 3
// String.make([StringConstraint.StringLength(3)])->String.parse("123adsfvcadsf")
