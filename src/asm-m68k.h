// Assembly language support for m68k CPU.
// Bruno Haible 1999-05-29

// Copyright (C) 1999-2017 Bruno Haible <bruno@clisp.org>
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
//   C(name)
//           This expands to the name of the C variable or function `name'.
//           On Unix BSD systems, this prepends an underscore.
//   L(label)
//           This expands to the name of a local label, having the name `label'.
//           On Unix ELF systems, where there is no underscore, names beginning
//           with an alphabetic character are automatically exported, so this
//           prepends a dot. Note that when defining a label, the `:' must
//           be inside the parentheses, not outside, because otherwise some
//           ANSI C preprocessor inserts a space between the label and the `:',
//           and some assemblers don't like this.
//   DECLARE_FUNCTION(name)
//           Declare `name' to be a global function. When assembly language
//           code is compiled into a shared library, ELF linkers need to know
//           which symbols are functions.
//   FUNBEGIN(name)
//           Start the assembly language code for the C function `name'.
//   FUNEND(name)
//           End the assembly language code for the C function 'name'.

#ifdef ASM_UNDERSCORE
// SunOS, NetBSD, OpenBSD, Linux/a.out
#define C(entrypoint) _##entrypoint
#define L(label) L##label
#else
// SVR4, A/UX, AMIX, Atari, Linux/ELF
#define C(entrypoint) entrypoint
#define L(label) .L##label
#endif

// When assembly language code is compiled into a shared library, ELF linkers
// need to know which symbols are functions.
#if defined(__NetBSD__) || defined(__OpenBSD__) || defined(__ELF__) || defined(__svr4__)
#define DECLARE_FUNCTION(name) .type C(name),@function
#define FUNEND(name) .size C(name),.-C(name)
#else
#define DECLARE_FUNCTION(name)
#define FUNEND(name)
#endif
#define FUNBEGIN(name) C(name):
