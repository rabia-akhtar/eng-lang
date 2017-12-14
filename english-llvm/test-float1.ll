; ModuleID = 'English'

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

declare i8* @fopen(i8*, i8*)

declare i32 @fclose(i8*)

declare i32 @fputs(i8*, i8*)

declare i32 @fread(i8*, i32, i32, i8*)

declare i32 @strlen(i8*)

declare i32 @strcmp(i8*, i8*)

declare i8 @char_lower(i8)

define i32 @main() {
entry:
  %a = alloca double
  store double 0.000000e+00, double* %a
  store double 1.200000e+00, double* %a
  ret i32 0
}
