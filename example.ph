let x : int := 1
let y : float := 2.0
let o: int := x

def h() -> void:
  pass

class Hola:
  init:
    Hola(i: int):
      pass

  def f(d : float) -> int:
    pass

class MemberedClass:
  init:
    let member1 : int
    let member2 : float

    MemberedClass(i: int, f: float):
      self.member1 := i
      self.member2 := f
 
  def f(i: int) -> void:
    self.member1 := i

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
  
  let c : int := 5
  if c == 5:
    return 1

  return 2

def p(f: float) -> float:
  return 5.0

let l : char[5]
create a : Hola(5)
create m : MemberedClass(5+2,1.0+p(1.0))
m.f(1)
p(2+1.0)
let abc, perro : float := y * x
let result : float := (x + y * abc / perro) ** 2
print(result)
while x + y == 3:
  print(1)
  if 3 > 2:
    print(1)
  elif True:
    print(2)
  elif 3 >= 2:
    print(3)
  else
    print(4)
for i: int := 0 : i < 10 : i := i + 1:
   print(i)
