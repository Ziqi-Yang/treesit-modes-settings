^0 = module: (
  path: "/path/to/file.o",
  hash: (
    2468601609,
    1329373163,
    1565878005,
    638838075,
    3148790418
  ))

^1 = gv: (name: "f") ; guid = 14740650423002898831
^2 = variable: (module: ^0,
  flags: (linkage: external,
    notEligibleToImport: 0,
    live: 0,
    dsoLocal: 0))
^3 = alias: (module: ^0,
  flags: (linkage: external,
    notEligibleToImport: 0,
    live: 0,
    dsoLocal: 0),
  aliasee: ^2)
^4 = funcFlags: (
  readNone: 0,
  readOnly: 0,
  noRecurse: 0,
  returnDoesNotAlias: 0,
  noInline: 0,
  alwaysInline: 0,
  noUnwind: 1,
  mayThrow: 0,
  hasUnknownCall: 0
)
^5 = vFuncId: (
  TypeIdRef,
  offset: 16
)
^6 = typeid: (name: "_ZTS1A",
  summary: (typeTestRes: (kind: allOnes,
    sizeM1BitWidth: 7)))
