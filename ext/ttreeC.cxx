#include "ttreeC.h"
#include "TTree.h"
#include "TChain.h"
#include <vector>

using namespace std;

void* tchain(const char* tn) {
    return (void*) new TChain(tn);
}

void tchainAdd(void* vp, const char* fn) {
    TChain* cp = (TChain*) vp;
    cp->Add(fn);
    return;
}

int ttreeGetEntry(void* vp, int i) {
    TChain* cp = (TChain*) vp;
    return cp->GetEntry(i);
}



void ttreeSetBranchAddress(void* tp, const char* bn, void** p) {
    TTree* t = (TTree*) tp;
    t->SetBranchAddress(bn, p);
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
