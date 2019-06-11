int ffi_go_sender(int ch, int val);
int ffi_go_sender_unblocked(int ch, int val);
int ffi_go_receiver(int ch, int expected);

typedef void (*Callback)(void);
int go_coroutine(Callback func);
