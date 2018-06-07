val list = List(1, 2, 3)

val result = list.flatMap(x => List(x, -x))
