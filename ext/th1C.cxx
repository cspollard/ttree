#include "th1C.h"
#include "TFile.h"
#include "TH1.h"

using namespace std;

#define TH1(T,C)                                \
                                                \
unsigned int nbins##C(void* vp) {               \
    return ((TH1##C *) vp)->GetNbinsX();        \
}                                               \
                                                \
double binedge##C(void* vp, unsigned int i) {   \
    return ((TH1##C *) vp)->GetBinLowEdge(i);   \
}                                               \
                                                \
T entry##C(void* vp, unsigned int i) {          \
    return ((TH1##C *) vp)->GetBinContent(i);   \
}                                               \
                                                \
T uncert##C(void* vp, unsigned int i) {         \
    return ((TH1##C *) vp)->GetBinError(i);     \
}                                               \


TH1(float, F)
TH1(double, D)
