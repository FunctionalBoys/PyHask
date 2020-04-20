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
  if True:
    return 5
  for j :int := 0 : True : j := j + 1:
    if False:
      i := 200
      break
    else
      continue
  return 2

main:
  create a : Hola()
  g(2)
  while True:
    print(1)
