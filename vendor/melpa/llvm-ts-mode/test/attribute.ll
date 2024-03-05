; function with attribute
@a = global [6 x void ()*] [void ()* no_cfi @f1,
  void ()* @f1,
  void ()* @f2,
  void ()* no_cfi @f2,
  void ()* @f3,
  void ()* no_cfi @f3
]

; summary with attribute
!8 = !DITemplateValueParameter(
  tag: DW_TAG_GNU_template_template_param,
  name: "param",
  type: !1,
  value: !"template"
)

^2 = gv: (guid: 1,
  summaries: (function: (module: ^0,
    flags: (linkage: external,
      visibility: default,
      notEligibleToImport: 0,
      live: 0,
      dsoLocal: 0),
    insts: 10,
    calls: ((callee: ^15,
      hotness: hot),
      (callee: ^17,
        hotness: cold),
      (callee: ^16,
        hotness: none)),
    refs: (writeonly ^14,
      readonly ^13,
      ^11)
  )))

^11 = gv: (guid: 10,
  summaries: (variable: (module: ^0,
    flags: (linkage: common,
      visibility: default,
      notEligibleToImport: 0,
      live: 0,
      dsoLocal: 0),
    varFlags: (readonly: 0)
  )))

^2 = flags: 97

; comdat
$"??_C@_0BC@CABPINND@Exception?5caught?$AA?$AA@" = comdat any

;; Attribute group
; Target-independent attributes:
attributes #0 = { alwaysinline alignstack=4 }

; Target-dependent attributes:
attributes #1 = { "no-sse" }

; Function @f has attributes: alwaysinline, alignstack=4, and "no-sse".
define void @f() #0 #1 { }
