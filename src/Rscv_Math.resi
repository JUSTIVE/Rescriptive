module Int: {
  let sum: array<int> => int
  let max: array<int> => int
  let ma: (int, int, int) => int
  let product: array<int> => int
  let isInRange: ('a, ('a, 'a)) => bool
  let digitLength: int => int
}
module Float: {
  let sum: array<float> => float
  let max: array<float> => float
  let ma: (float, float, float) => float
  let product: array<float> => float
  let isInRange: ('a, 'a, 'a) => bool
}
