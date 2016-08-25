#include "ttreeC.h"
#include "TChain.h"
#include <vector>

using namespace std;

void* tchain(const char* tn) {
    TChain *cp = new TChain(tn);
    cp->SetBranchStatus("*", 0);
    return (void*) cp;
}

int tchainAdd(void* vp, const char* fn) {
    TChain* cp = (TChain*) vp;
    return cp->Add(fn);
}

int tchainGetEntry(void* vp, int i) {
    TChain* cp = (TChain*) vp;
    return cp->GetEntry(i);
}

void tchainSetBranchAddress(void* vp, const char* bn, void** p) {
    TChain* cp = (TChain*) vp;
    cp->SetBranchStatus(bn, 1);
    cp->SetBranchAddress(bn, p);
    return;
}


template <typename T>
vector<T>* castVec(void* vp) {
    return (vector<T>*) vp;
}


unsigned int vectorSizeI(void* vp) {
    return castVec<int>(vp)->size();
}

unsigned int vectorSizeC(void* vp) {
    return castVec<char>(vp)->size();
}

unsigned int vectorSizeD(void* vp) {
    return castVec<double>(vp)->size();
}

unsigned int vectorSizeF(void* vp) {
    return castVec<float>(vp)->size();
}


int* vectorDataI(void* vp) {
    return castVec<int>(vp)->data();
}

char* vectorDataC(void* vp) {
    return castVec<char>(vp)->data();
}

double* vectorDataD(void* vp) {
    return castVec<double>(vp)->data();
}

float* vectorDataF(void* vp) {
    return castVec<float>(vp)->data();
}
