#include "ttreeC.h"
#include "TFile.h"
#include "TTree.h"
#include <vector>
#include <iostream>

using namespace std;

void* ttree(const char* tn, const char* fn) {
    TFile* f = TFile::Open(fn);
    TTree* tp = (TTree*) f->Get(tn);
    if (!tp)
        return NULL;

    tp->SetBranchStatus("*", false);
    return (void*) tp;
}

int ttreeGetBranchEntry(void* vp, const char* bn, int i, void* bp) {
    TTree* tp = (TTree*) vp;
    tp->SetBranchStatus(bn, true);
    TBranch *b = tp->GetBranch(bn);
    b->SetAddress(bp);
    return b->GetEntry(i);
}

long ttreeLoadTree(void* vp, int i) {
    TTree* tp = (TTree*) vp;
    long n = tp->LoadTree(i);
    if (n >= 0)
        tp->GetEntry(i);
    return n;
}

void ttreeFree(void* vp) {
    TTree* tp = (TTree*) vp;
    delete tp;
    return;
}


template <typename T>
vector<T>* castVec(void* vp) {
    return (vector<T>*) vp;
}

#define VECFUNCS(T,C)                  \
                                       \
unsigned int vectorSize##C(void* vp) { \
    return castVec<T>(vp)->size();     \
}                                      \
                                       \
T* vectorData##C(void* vp) {           \
    return castVec<T>(vp)->data();     \
}                                      \
                                       \
void vectorFree##C(void** vp) {        \
    vector<T>** p = (vector<T>**) vp;  \
    delete *p;                         \
    delete p;                          \
    return;                            \
}


VECFUNCS(char, C)
VECFUNCS(int, I)
VECFUNCS(float, F)
VECFUNCS(double, D)
VECFUNCS(void*, P)


#define VVECFUNCS(T,C)                                             \
                                                                   \
void* vvRead##C(void* vp) {                                        \
    vector<vector<T> >* vvp = castVec<vector<T> >(vp);             \
    vector<vector<T>*>* vpp = new vector<vector<T>*>(vvp->size()); \
                                                                   \
    for (unsigned int i = 0; i < vvp->size(); i++)                 \
        (*vpp)[i] = &((*vvp)[i]);                                  \
                                                                   \
    return (void*) vpp;                                            \
}                                                                  \
                                                                   \
void vvFree##C(void** vp) {                                        \
    vector<vector<T> >** p = (vector<vector<T> >**) vp;            \
    delete *p;                                                     \
    delete p;                                                      \
    return;                                                        \
}

VVECFUNCS(char, C)
VVECFUNCS(int, I)
VVECFUNCS(float, F)
VVECFUNCS(double, D)
