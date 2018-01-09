// Assembly language support for mips CPU.
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

// When assembly language code is compiled into a shared library, ELF linkers
// need to know which symbols are functions.
#if defined(__ELF__) || defined(__NetBSD__)
#define DECLARE_FUNCTION(name) .type name,@function
#else
#define DECLARE_FUNCTION(name)
#endif
