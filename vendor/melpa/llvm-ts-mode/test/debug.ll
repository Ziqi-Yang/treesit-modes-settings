!0 = !DILocation(line: 2900, column: 42, scope: !1, inlinedAt: !2)

!0 = !DICompileUnit(
  language: DW_LANG_C99, file: !1, producer: "clang",
  isOptimized: true, flags: "-O2", runtimeVersion: 2,
  splitDebugFilename: "abc.debug", emissionKind: FullDebug,
  enums: !2, retainedTypes: !3, globals: !4, imports: !5,
  macros: !6, dwoId: 0x0abcd)

!0 = !DIFile(filename: "path/to/file", directory: "/path/to/dir",
  checksumkind: CSK_MD5,
  checksum: "000102030405060708090a0b0c0d0e0f")

!0 = !DIBasicType(name: "unsigned char", size: 8, align: 8,
  encoding: DW_ATE_unsigned_char)
!1 = !DIBasicType(tag: DW_TAG_unspecified_type, name: "decltype(nullptr)")

!0 = !BasicType(name: "int", size: 32, align: 32, DW_ATE_signed)
!1 = !BasicType(name: "char", size: 8, align: 8, DW_ATE_signed_char)
!2 = !DISubroutineType(types: !{null, !0, !1}) ; void (int, char)

!0 = !DIBasicType(name: "unsigned char", size: 8, align: 8,
  encoding: DW_ATE_unsigned_char)
!1 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !0, size: 32,
  align: 32)

!0 = !DIEnumerator(name: "SixKind", value: 7)
!1 = !DIEnumerator(name: "SevenKind", value: 7)
!2 = !DIEnumerator(name: "NegEightKind", value: -8)
!3 = !DICompositeType(tag: DW_TAG_enumeration_type, name: "Enum", file: !12,
  line: 2, size: 32, align: 32, identifier: "_M4Enum",
  elements: !{!0, !1, !2})

!0 = !DISubrange(count: 5, lowerBound: 0) ; array counting from 0
!1 = !DISubrange(count: 5, lowerBound: 1) ; array counting from 1
!2 = !DISubrange(count: -1) ; empty array.

@foo = global i32, !dbg !0
!0 = !DIGlobalVariableExpression(var: !1, expr: !DIExpression())
!1 = !DIGlobalVariable(name: "foo", linkageName: "foo", scope: !2,
  file: !3, line: 7, type: !4, isLocal: true,
  isDefinition: false, declaration: !5)

@lower = global i32, !dbg !0
@upper = global i32, !dbg !1
!0 = !DIGlobalVariableExpression(
  var: !2,
  expr: !DIExpression(DW_OP_LLVM_fragment, 0, 32)
)
!1 = !DIGlobalVariableExpression(
  var: !2,
  expr: !DIExpression(DW_OP_LLVM_fragment, 32, 32)
)
!2 = !DIGlobalVariable(name: "split64", linkageName: "split64", scope: !3,
  file: !4, line: 8, type: !5, declaration: !6)

!0 = distinct !DISubprogram(name: "foo", linkageName: "_Zfoov", scope: !1,
  file: !2, line: 7, type: !3, isLocal: true,
  isDefinition: true, scopeLine: 8,
  containingType: !4,
  virtuality: DW_VIRTUALITY_pure_virtual,
  virtualIndex: 10, flags: DIFlagPrototyped,
  isOptimized: true, unit: !5, templateParams: !6,
  declaration: !7, retainedNodes: !8,
  thrownTypes: !9)

!0 = distinct !DILexicalBlock(scope: !1, file: !2, line: 7, column: 35)
