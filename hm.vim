" Vim syntax file
" Language: CS540 Compilers class
" Maintainer: Ben Sherman
" Latest Revision: 3 November 2014

if exists("b:current_syntax")
  finish
endif


" keywords
syn keyword hmKeywords data case of let in

" regions
syn region hmBlock start="{" end="}" fold transparent

" matches
syn match hmComment "--.*$"
syn match hmVariable '[a-z]\(\a|\d|\'\)*'
syn match hmNumber '\d\+'

syn match hmConstructor "\<[A-Z][a-zA-Z0-9_']*\>"
syn match hmVariable "\<[a-z][a-zA-Z0-9_']*\>"

syn region hmString start='"' end='"'
syn match hmEquals '\(=>\|[=|;:]\)'
syn match hmSpecial '[{}(),\[\]]'

let b:current_syntax = "hm"
hi def link hmVariable Identifier
hi def link hmConstructor Type
hi def link hmComment Comment
hi def link hmKeywords Statement
hi def link hmNumber Constant
hi def link hmString Constant
hi def link hmEquals Statement
hi def link hmSpecial Special
