#ifdef __cplusplus
extern "C" {
#endif
    void* ttree(const char*, const char*);

    void ttreeFree(void*);
    void ttreeResetBranchAddresses(void*);
    int ttreeGetBranchEntry(void*, const char*, int, void*);

    unsigned int vectorSizeI(void* vp);
    unsigned int vectorSizeC(void* vp);
    unsigned int vectorSizeD(void* vp);
    unsigned int vectorSizeF(void* vp);

    int*    vectorDataI(void* vp);
    char*   vectorDataC(void* vp);
    double* vectorDataD(void* vp);
    float*  vectorDataF(void* vp);
#ifdef __cplusplus
}
#endif
