@g1 = global i32 0, partition "part4"
@a1 = alias i32, i32* @g1, partition "part5"
@i1 = ifunc void(), void()* ()* @f1, partition "part6"
