// SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
//
// SPDX-License-Identifier: GPL-3.0-only

#include <stddef.h>

extern void qute_exit(int);
extern void qute_make_symbolic(void *ptr, size_t nelem, size_t elsiz, const char *name);

#define assert(x) \
	((void)((x) || (__assert_fail(),0)))

_Noreturn void __assert_fail(void) {
	__builtin_unreachable();
}

int main(void) {
	int a;
	qute_make_symbolic(&a, 1, sizeof(a), "a");

	if (a <= 0)
		qute_exit(0);
	assert(a > 0);
	return 0;
}
