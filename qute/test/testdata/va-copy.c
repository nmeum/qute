// SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
//
// SPDX-License-Identifier: GPL-3.0-only

#include <stdarg.h>
#include <stdio.h>

int
vsum(int count, ...)
{
	va_list args;
	va_start(args, count);

	va_list copy;
	va_copy(copy, args);

	int sum = 0;
	for (int i = 0; i < count; i++) {
		sum += va_arg(copy, int);
	}

	va_end(copy);
	va_end(args);

	return sum;
}

int
main(void)
{
	return vsum(4, 10, 20, 30, 40);
}
