import funsets.FunSets._

//List(1,3,4,5,7,1000) funsets.FunSets.map((n: Int) => n - 1)
val s1 = singletonSet(1)
val s3 = singletonSet(3)
val s4 = singletonSet(4)
val s5 = singletonSet(5)
val s7 = singletonSet(7)
val s1000 = singletonSet(1000)

val mySets = union(s1, s3)
val mySets2 = union(union(s1, s3), s4)
//val mySets = (union(union(union(union(s1, s3), s4), s5), s7), s1000)
//val mySets = union(mySets, s4)
//val mySets = union(mySets, s5)
//val mySets = union(mySets, s7)
//val mySets = union(mySets, s1000)

printSet(mySets2)
printSet(diff(mySets2, s3))
printSet(union(mySets2, s7))
printSet(filter(mySets2, union(s3, s4)))
forall(s1, (n: Int) => n > 0)
//printSet(map(mySets, (n: Int) => n))
//val x = toString(s1)
//printSet(forall(mySets, (n: Int) => n > 0))