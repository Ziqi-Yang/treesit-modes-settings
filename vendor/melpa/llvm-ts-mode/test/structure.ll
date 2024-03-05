declare float @llvm.sqrt(float) nounwind readnone
module asm "inline asm code goes here"
module asm "more can go here"

@G = addrspace(5) constant float 1.0, section "foo", align 4

@G = external global i32

@"complex@Name" = thread_local(initialexec) global i32 0, align 4

$foo = comdat largest
@foo = global i32 2, comdat($foo)

define void @bar() comdat($foo) {
  ret void
}
