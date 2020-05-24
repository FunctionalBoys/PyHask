while True:
  let a : int := 1
  if a > 1:
    break
  else
    continue

for i : int := 0 : i < 5 : i := i + 1:
  let a : int := 2
  while True:
    continue
    print(a)
  if a < 1:
    continue
  else
    break
