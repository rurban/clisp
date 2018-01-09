// Assembly language support for hppa CPU.
// Bruno Haible 2017-01-23

// Copyright (C) 2017-2018 Bruno Haible <bruno@clisp.org>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// In order not to have to maintain several copies of the assembly language
// code, we use some macros which expand into the correct syntax.
// These macros are:
//   IMPORT_MILLICODE(symbol)
//           This expands to an import from the MILLICODE segment, if needed.
//   IMPORT_DATA(symbol)
//           This expands to an import from the DATA segment, if needed.
//   TEXT1(), TEXT2()
//           These expand to two lines that switch to the TEXT section/segment.
//   GLOBL(symbol)
//           This expands to a declaration that the given symbol, defined
//           in this file, shall have global visibility.
//   DEF(symbol)
//           This expands to the declaration of a symbol or label.
//   L(label)
//           This expands to the name of a local label, having the name `label'.
//   DECLARE_FUNCTION(name)
//           Declare `name' to be a global function. When assembly language
//           code is compiled into a shared library, ELF linkers need to know
//           which symbols are functions.
//   FUNEND(name)
//           End the assembly language code for the C function 'name'.

#if defined(__ELF__)
/* Linux */
#define IMPORT_MILLICODE(symbol)
#define IMPORT_DATA(symbol)
#define TEXT1() .text
#define TEXT2()
#define GLOBL(symbol) .globl symbol
#define DEF(symbol) symbol:
#define L(label) .L##label
#define DECLARE_FUNCTION(name) .type name,%function
#define FUNEND(name) .size name,.-name
#else
/* HP-UX */
#define IMPORT_MILLICODE(symbol) .IMPORT symbol,MILLICODE
#define IMPORT_DATA(symbol) .IMPORT symbol,DATA
#if 1
#define TEXT1() .SPACE $TEXT$
#define TEXT2() .SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
#else
#define TEXT1() .code
#define TEXT2()
#endif
#define GLOBL(symbol) .EXPORT symbol,ENTRY,PRIV_LEV=3
#if 1
#define DEF(symbol) symbol
#else
#define DEF(symbol) .label symbol
#endif
#define L(label) L$##label
#define DECLARE_FUNCTION(name)
#define FUNEND(name)
#endif
