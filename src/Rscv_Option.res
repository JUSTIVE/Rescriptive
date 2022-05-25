let flatten: option<option<'a>> => option<'a> = v =>
  switch v {
  | Some(Some(x)) => x->Some
  | _ => None
  }
