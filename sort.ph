let arr : int[10]
let n : int
for i : int := 0 : i < 10 : i := i + 1:
  n := read
  arr[i] := n

for i : int := 0 : i < 10 : i := i + 1:
  for j : int := 0 : j < 10 - i - 1 : j := j + 1:
    if arr[j] > arr[j+1]:
      let x : int := arr[j]
      arr[j] := arr[j+1]
      arr[j+1] := x

print('😊')
for i : int := 0 : i < 10 : i := i + 1:
  print(arr[i])