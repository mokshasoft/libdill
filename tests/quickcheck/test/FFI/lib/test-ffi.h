int ffi_go_sender(int ch, int val);

typedef void (*Callback)(void);
int go_coroutine(Callback func);
