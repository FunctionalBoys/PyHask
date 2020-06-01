def fact(i:int) -> int:
  if i == 0:
    return 1
  return i * fact(i-1)

def fib(i:int) -> int:
  if i == 0:
    return 0
  elif i == 1:
    return 1
  return fib(i-1) + fib(i-2)

def f(i: int) -> int:
  let j : int := 0
  let fl : float := 1.0
  let k : int := i * 2 - j
  return k

def g(i:int) -> int:
  return i * 2

def f_void() -> void:
  return

let i: int := fact(5)
print(i)
let j: int := fib(10)
print(j)
let a : int[5]
for k : int := 0 : k < 5 : k := k + 1:
  a[k] := k

for m : int := 0 : m < 5 : m := m + 1:
  print(a[m])
