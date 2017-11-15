; ModuleID = 'English'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

define i32 @main() {
entry:
  %a = alloca float
  %b = alloca float
  store float 0x3FF3333340000000, float* %a
  store float 3.500000e+00, float* %b
  ret i32 0
}
