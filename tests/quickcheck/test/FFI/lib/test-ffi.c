#include "test-ffi.h"
#include "libdill.h"
#include "tests/assert.h"

coroutine void sender(int ch, int val) {
    int rc = chsend(ch, &val, sizeof(val), -1);
    errno_assert(rc == 0);
}

int ffi_go_sender(int ch, int val) {
    return go(sender(ch, val));
}

static coroutine void coroutine_proxy_func(Callback func) {
    func();
}

int go_coroutine(Callback func) {
    return go(coroutine_proxy_func(func));
}
