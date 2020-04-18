let a : int := 1

def h() -> void:
  pass

class Hola:
  init:
    Hola():
      super() # Comment
      pass

  def f(d : float) -> int:
    let de : string := 'hola'
    pass

def g(i : int) -> int:
  let b : bool := True
  if i > 100:
    return 5
  for j :int := 0 : j < 10 : j := j + 1:
    if i < 5 and j > 2:
      i := 200
      break
    else
      continue
  return 2

main:
  create a : Hola()
  pass
  while True:
    print(1)
