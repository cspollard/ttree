#ifdef __cplusplus
extern "C" {
#endif
    void* tchain(const char*);
    int tchainAdd(void*, const char*);
    int tchainGetEntry(void*, int);

    void tchainSetBranchAddress(void*, const char*, void**);
    void tchainFree(void*);

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
