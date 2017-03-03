#define VECH(T,C)                  \
unsigned int vectorSize##C(void*); \
T* vectorData##C(void*);           \
void vectorFree##C(void**);        \

#define VVECH(T,C)      \
void* vvRead##C(void*); \
void vvFree##C(void**); \


#ifdef __cplusplus
extern "C" {
#endif
    void* ttree(void*, const char*);

    int ttreeGetBranchEntry(void*, const char*, int, void*);
    long ttreeLoadTree(void*, int);
    void ttreeFree(void*);

    VECH(char, C)
    VECH(int, I)
    VECH(float, F)
    VECH(double, D)
    VECH(void*, P)

    VVECH(char, C)
    VVECH(int, I)
    VVECH(float, F)
    VVECH(double, D)

#ifdef __cplusplus
}
#endif
