
List(1,2,3,4,5).foldLeft(0)((acc, v) => acc + v)

List(1,2,3,5).reduce((v1 , v2) => v1 + v2)

class superclass(){
  var b: Boolean = true
}

class class1() extends superclass{
  var c: Int = 0
}
class class2() extends superclass {
  var c: String = "s"
}


def foo(i: Int): superclass ={
  if(i == 1){
    return new class1()
  }
  else {
    return new class2()
  }
}

val e: superclass = foo(1)

e.