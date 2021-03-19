; ModuleID = 'test'
source_filename = "test"

define double @ifthenelse(double %i) {
entry:
  br i1 true, label %thenblock, label %elseblock

thenblock:                                        ; preds = %entry
  %callexpr = call double @HOFF_LOCAL0(double %i)
  br label %fiblock

elseblock:                                        ; preds = %entry
  %callexpr1 = call double @HOFF_LOCAL1(double %i)
  br label %fiblock

fiblock:                                          ; preds = %elseblock, %thenblock
  %phi = phi double [ %callexpr, %thenblock ], [ %callexpr1, %elseblock ]
  ret double %phi
}

define double @HOFF_LOCAL0(double %i) {
entry:
  %addexpr = fadd double %i, 1.000000e+00
  ret double %addexpr
}

define double @HOFF_LOCAL1(double %j) {
entry:
  %callexpr = call double @HOFF_LOCAL2(double 1.000000e+00, double 2.000000e+00, double 3.000000e+00)
  %addexpr = fadd double %callexpr, 3.000000e+00
  ret double %addexpr
}

define double @HOFF_LOCAL2(double %a, double %b, double %c) {
entry:
  %callexpr = call double @HOFF_LOCAL0(double 1.000000e+00)
  %addexpr = fadd double 1.000000e+00, %callexpr
  %ifblock = fcmp one double %addexpr, 0.000000e+00
  br i1 %ifblock, label %thenblock, label %elseblock

thenblock:                                        ; preds = %entry
  %addexpr1 = fadd double %a, %b
  %addexpr2 = fadd double %addexpr1, %c
  br label %fiblock

elseblock:                                        ; preds = %entry
  %mulexpr = fmul double %a, %b
  %mulexpr3 = fmul double %mulexpr, %c
  br label %fiblock

fiblock:                                          ; preds = %elseblock, %thenblock
  %phi = phi double [ %addexpr2, %thenblock ], [ %mulexpr3, %elseblock ]
  ret double %phi
}

define double @something(double %i) {
entry:
  %divexpr = fdiv double %i, 3.000000e+00
  ret double %divexpr
}

