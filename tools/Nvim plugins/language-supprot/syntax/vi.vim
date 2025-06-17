" Vim syntax file

" quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim



syn cluster viNonCode contains=viCommentLn,viCommentEx,viString,viCharacter



" Comments
syn match   viCommentLn   "//.*" contains=@Spell
syn region  viCommentEx   start="/{" end="/}" contains=viCommentEx,@Spell



" Literals
syn match   viEscapeSimple  contained /\v\\([abtnfrv?"'\\'])/ display
syn match   viEscapeOctal   contained /\v\\[0-7]{1,3}/ display
syn match   viEscapeHex     contained /\v\\x[0-9a-fA-F]+/ display
syn match   viEscapeUnicode contained /\v\\(u[0-9a-fA-F]{4}|U[0-9a-fA-F]{8})/ display
syn cluster viEscapeAll contains=viEscapeSimple,viEscapeOctal,viEscapeHex

syn region  viString      start=/"/ end=/"/ contains=@viEscapeAll,@Spell,viTodo containedin=viCommentEx,viCommentLn concealends
syn region  viCharacter   start=/'/ end=/'/ contains=@viEscapeAll,@Spell,viTodo containedin=viCommentEx,viCommentLn concealends

syn keyword viBoolean     true false containedin=ALLBUT,@viNonCode
syn keyword viConstant    null       containedin=ALLBUT,@viNonCode

syn match   viFloat       /\v<((([0-9](_?[0-9])*)?\.[0-9](_?[0-9])*) | ([0-9](_?[0-9])*\.))([fF])?\>/ display containedin=ALLBUT,@viNonCode
syn match   viFloat       /\v<[0-9](_?[0-9])*[fF]\>/ display containedin=ALLBUT,@viNonCode
syn match   viNumber      /\v<0[xX][0-9a-fA-F](_?[0-9a-fA-F])*>/ display containedin=ALLBUT,@viNonCode
syn match   viNumber      /\v<0[bB][01](_?[01])*>/ display containedin=ALLBUT,@viNonCode
syn match   viNumber      /\v<0[0-7](_?[0-7])*>/ display containedin=ALLBUT,@viNonCode
syn match   viNumber      /\v<([1-9](_?[0-9])*|0)>/ display containedin=ALLBUT,@viNonCode



" Keywords
syn keyword viStructure     def enum error fcn namespace struct union
syn keyword viStatement     break continue return goto
syn keyword viConditional   if else switch case when
syn keyword viRepeat        while for loop do
syn keyword viType          int i8 u8 i16 u16 i32 u32 i64 u64 f32 f64 string
syn keyword viModifier      const embed local
syn keyword viControl       using import as alloc catch free from



" Operators
syn match   viOperator      /::/ display containedin=ALLBUT,@viNonCode
syn match   viOperator      /\.\./ display containedin=ALLBUT,@viNonCode
syn match   viOperator      /<<=/ display containedin=ALLBUT,@viNonCode
syn match   viOperator      />>=/ display containedin=ALLBUT,@viNonCode
syn match   viOperator      /<=/ display containedin=ALLBUT,@viNonCode
syn match   viOperator      />=/ display containedin=ALLBUT,@viNonCode
syn match   viOperator      /==/ display containedin=ALLBUT,@viNonCode
syn match   viOperator      /!=/ display containedin=ALLBUT,@viNonCode
syn match   viOperator      /&&/ display containedin=ALLBUT,@viNonCode
syn match   viOperator      /\v\|\|/ display containedin=ALLBUT,@viNonCode
syn match   viOperator      /<</ display containedin=ALLBUT,@viNonCode
syn match   viOperator      />>/ display containedin=ALLBUT,@viNonCode
syn match   viOperator      /++/ display containedin=ALLBUT,@viNonCode
syn match   viOperator      /--/ display containedin=ALLBUT,@viNonCode

syn match   viOperator      / \/ / display containedin=ALLBUT,@viNonCode nextgroup=viCommentLn,viCommentEx
syn match   viOperator      /\v[=<>!&|%~^*+?-]/ display containedin=ALLBUT,@viNonCode

syn match   viOperator      /:/ display containedin=ALLBUT,@viNonCode " Colon (např. ternární)
syn match   viAccessor      /\./ display containedin=ALLBUT,@viNonCode " Dot Accessor (může být linkován jinak)



" Preprocessor-like directives
syn match   viPreProc       /#\s*\k\+/ contains=viInclude,viIdentifier " General #directive
syn match   viInclude       /#\s*import\>/ contained " Specific #import



" Identifiers and Function Calls
syn match   viFunctionCall  /\v\k+\s*\(/me=e-1 contains=viIdentifier display containedin=ALLBUT,@viNonCode
" syn match   viIdentifier    /\v<[a-zA-Z_][a-zA-Z0-9_]*>/ display containedin=ALLBUT,@viNonCode



" Highlighting Links
hi def link viCommentLn     Comment
hi def link viCommentEx     Comment

hi def link viString        String
hi def link viCharacter     Character
hi def link viEscapeSimple  SpecialChar
hi def link viEscapeOctal   SpecialChar
hi def link viEscapeHex     SpecialChar
hi def link viEscapeUnicode SpecialChar
hi def link viNumber        Number
hi def link viBoolean       Boolean
hi def link viFloat         Float
hi def link viConstant      Constant

hi def link viStructure     Structure
hi def link viStatement     Statement
hi def link viConditional   Conditional
hi def link viRepeat        Repeat
hi def link viLabel         Label
hi def link viType          Type
hi def link viModifier      StorageClass
hi def link viControl       Keyword

hi def link viOperator      Operator
hi def link viAccessor      Operator

hi def link viPreProc       PreProc
hi def link viInclude       Include

hi def link viIdentifier    Identifier
hi def link viFunctionCall  Function



let b:current_syntax = "vi"

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: ts=4 sw=4 et