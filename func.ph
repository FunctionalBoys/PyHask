def fact(i:int) -> int:
  if i == 0:
    return 0
  return i * fact(i-1)

def f(i: int) -> int:
  let j : int := 0
  let fl : float := 1.0
  let k : int := i * 2 - j
  return k

def f_void() -> void:
  return

let i: int := f(1+2)
print(i)