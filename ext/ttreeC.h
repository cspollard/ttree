#ifdef __cplusplus
extern "C" {
#endif
    void* ttree(const char*, const char*);

    void ttreeFree(void*);
    int ttreeGetBranchEntry(void*, const char*, int, void*);
    void ttreeResetBranchAddress(void *, const char*);

    unsigned int vectorSizeC(void* vp);
    unsigned int vectorSizeI(void* vp);
    unsigned int vectorSizeF(void* vp);
    unsigned int vectorSizeD(void* vp);

    char*   vectorDataC(void* vp);
    int*    vectorDataI(void* vp);
    float*  vectorDataF(void* vp);
    double* vectorDataD(void* vp);

    void vectorFreeC(void** vp);
    void vectorFreeI(void** vp);
    void vectorFreeF(void** vp);
    void vectorFreeD(void** vp);
#ifdef __cplusplus
}
#endif
