
List(1,2,3,4,5).foldLeft(0)((acc, v) => acc + v)

List(1,2,3,5).reduce((v1 , v2) => v1 + v2)

val map: Map[(Int, Int), String] = Map((1,3) -> "1",(1,4) -> "2", (1,3) -> "3")

map.updated((1,3),"123")

map.updated((23,12), "1234")

val list = List((1,4), (23,12), (22,52))

for(i <- 1 to 5) yield map.get(i,i)

val x = list.max._1
