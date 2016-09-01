
#ifdef __cplusplus
extern "C" {
#endif
    void* ttree(const char*, const char*);

    void ttreeFree(void*);
    int ttreeGetBranchEntry(void*, const char*, int, void*);
    void ttreeResetBranchAddress(void *, const char*);

    unsigned int vectorSizeC(void*);
    unsigned int vectorSizeI(void*);
    unsigned int vectorSizeF(void*);
    unsigned int vectorSizeD(void*);

    char*   vectorDataC(void*);
    int*    vectorDataI(void*);
    float*  vectorDataF(void*);
    double* vectorDataD(void*);

    void vectorFreeC(void**);
    void vectorFreeI(void**);
    void vectorFreeF(void**);
    void vectorFreeD(void**);

    void* vectorNewVD();
    int vectorSizeVD(void*);
    void* vectorDataVD(void*);
    void* vectorBPtrVD(void*);
    void vectorFreeVD(void*);

#ifdef __cplusplus
}
#endif
