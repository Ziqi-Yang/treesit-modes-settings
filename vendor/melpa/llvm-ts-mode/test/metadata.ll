;; function metadata
define void @foo(i1 %f0, i1 %f1, i1 %f2) !prof !{!"function_entry_count", i64 0} {}

;; specialized metadata angle brackets
!0 = distinct !DICompileUnit(language: <LANG1>, file: !1, producer: "clang")

;; specialized metadata with function type
!16 = !DITemplateValueParameter(name: "b", type: !17, value: i8 ()* @_Z1av)

;; specialized metadata with constant expression
!11 = !DITemplateValueParameter(
  type: !12,
  value: %Tricky.1* bitcast (i8* @templateValueParam to %Tricky.1*)
)

;; specialized metadata string with backslash before quote end
!9 = !DIFile(filename: "./test2.cpp", directory: "C:\")
!12 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
