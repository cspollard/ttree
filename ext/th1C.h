#define DECTH1(T,C)                      \
void* th1##C(const char*, const char*);  \
unsigned int nbins##C(void*);            \
double binedge##C(void*, unsigned int);  \
T entry##C(void*, unsigned int);         \
T uncert##C(void*, unsigned int);        \


#ifdef __cplusplus
extern "C" {
#endif
    DECTH1(float, F)
    DECTH1(double, D)

#ifdef __cplusplus
}
#endif
