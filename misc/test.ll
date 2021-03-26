; ModuleID = 'test'
source_filename = "test"

declare double @print(double)

declare double @read()

define double @fibonacci(double %i) {
entry:
  %eqexpr = fcmp oeq double %i, 0.000000e+00
  %eqexpr1 = fcmp oeq double %i, 1.000000e+00
  %orexpr = or i1 %eqexpr, %eqexpr1
  br i1 %orexpr, label %thenblock, label %elseblock

thenblock:                                        ; preds = %entry
  br label %fiblock

elseblock:                                        ; preds = %entry
  %subexpr = fsub double %i, 1.000000e+00
  %callexpr = call double @fibonacci(double %subexpr)
  %subexpr2 = fsub double %i, 2.000000e+00
  %callexpr3 = call double @fibonacci(double %subexpr2)
  %addexpr = fadd double %callexpr, %callexpr3
  br label %fiblock

fiblock:                                          ; preds = %elseblock, %thenblock
  %phi = phi double [ %i, %thenblock ], [ %addexpr, %elseblock ]
  ret double %phi
}

define double @main() {
entry:
  %callexpr = call double @read()
  %callexpr1 = call double @fibonacci(double %callexpr)
  %callexpr2 = call double @print(double %callexpr1)
  ret double %callexpr2
}

