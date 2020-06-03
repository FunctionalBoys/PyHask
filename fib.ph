let a : int := 0
let b : int := 1

let limit : int
limit := read

for i : int := 0 : i < limit : i := i + 1:
  let c : int := a
  a := b
  b := c + b

print(b)