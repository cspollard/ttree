#ifdef __cplusplus
extern "C" {
#endif

    void* tfileOpen(const char*);
    void* tfileGet(void*, const char*);
    void tfileClose(void*);

#ifdef __cplusplus
}
#endif
