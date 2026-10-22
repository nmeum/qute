// SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
//
// SPDX-License-Identifier: GPL-3.0-only

#include <stdarg.h>

int
myprintf(int n, const char *fmt, ...)
{
	va_list args;
	va_start(args, n);

	va_list copy;
	va_copy(copy, args);

	va_end(copy);
	va_end(args);

	return 0;
}

int
main(void)
{
	return myprintf(0, "hello, world!");
}
