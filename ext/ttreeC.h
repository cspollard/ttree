#ifdef __cplusplus
extern "C" {
#endif
    void* tchain(const char*);
    int tchainAdd(void*, const char*);
    int tchainGetEntry(void*, int);

    void tchainFree(void*);
    void tchainSetBranchAddress(void* vp, const char* bn, void* bp);

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
