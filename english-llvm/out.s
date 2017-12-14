; ModuleID = 'English'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"

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
  %a = alloca { i32, i32* }
  store { i32, i32* } zeroinitializer, { i32, i32* }* %a
  %malloccall = tail call i8* @malloc(i32 mul (i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32), i32 3))
  %tmp = bitcast i8* %malloccall to i32*
  %tmp2 = getelementptr i32, i32* %tmp, i32 0
  store i32 3, i32* %tmp2
  %tmp21 = getelementptr i32, i32* %tmp, i32 1
  store i32 2, i32* %tmp21
  %tmp22 = getelementptr i32, i32* %tmp, i32 2
  store i32 1, i32* %tmp22
  %malloccall.3 = tail call i8* @malloc(i32 ptrtoint ({ i32, i32* }* getelementptr ({ i32, i32* }, { i32, i32* }* null, i32 1) to i32))
  %arr_literal = bitcast i8* %malloccall.3 to { i32, i32* }*
  %first = getelementptr inbounds { i32, i32* }, { i32, i32* }* %arr_literal, i32 0, i32 0
  %second = getelementptr inbounds { i32, i32* }, { i32, i32* }* %arr_literal, i32 0, i32 1
  store i32 3, i32* %first
  store i32* %tmp, i32** %second
  %actual_arr_literal = load { i32, i32* }, { i32, i32* }* %arr_literal
  store { i32, i32* } %actual_arr_literal, { i32, i32* }* %a
  %a4 = load { i32, i32* }, { i32, i32* }* %a
  %extract_ptr = extractvalue { i32, i32* } %a4, 1
  %extract_value = getelementptr i32, i32* %extract_ptr, i32 0
  %value = load i32, i32* %extract_value
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i32 %value)
  ret i32 0
}

declare noalias i8* @malloc(i32)
