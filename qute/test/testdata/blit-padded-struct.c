// SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
//
// SPDX-License-Identifier: GPL-3.0-only

#include <stdlib.h>

// (gdb) pt/o PaddedStruct
// type = struct PaddedStruct {
// /*      0      |       4 */    int status;
// /* XXX  4-byte hole      */
// /*      8      |       8 */    void *stamps;
// /*     16      |       8 */    void **arguments;
// /*     24      |       4 */    int argumentsN;
// /* XXX  4-byte padding   */
// /* total size:        32 */
// }
typedef struct PaddedStruct {
  int status;
  void *stamps;
  void **arguments;
  int argumentsN;
} PaddedStruct;

PaddedStruct somefunc(int status) {
	return (PaddedStruct) {
		.status = status,
		.stamps = NULL,
		.arguments = NULL,
		.argumentsN = 0,
	};
}

int main(void) {
	PaddedStruct s = somefunc(23);
	return s.status;
}
