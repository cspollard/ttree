#include "ttreeC.h"
#include "TFile.h"
#include "TTree.h"
#include <vector>
#include <iostream>

using namespace std;

void* ttree(const char* tn, const char* fn) {
    TFile* f = TFile::Open(fn);
    TTree* tp = (TTree*) f->Get(tn);
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

void ttreeResetBranchAddress(void *vp, const char* bn) {
    TTree* tp = (TTree*) vp;
    TBranch *b = tp->GetBranch(bn);
    b->ResetAddress();
    return;
}

void ttreeFree(void* vp) {
    TTree* cp = (TTree*) vp;
    delete cp;
    return;
}


template <typename T>
vector<T>* castVec(void* vp) {
    return (vector<T>*) vp;
}


// TODO
// macro-ize this.

unsigned int vectorSizeC(void* vp) {
    return castVec<char>(vp)->size();
}

unsigned int vectorSizeI(void* vp) {
    return castVec<int>(vp)->size();
}

unsigned int vectorSizeF(void* vp) {
    return castVec<float>(vp)->size();
}

unsigned int vectorSizeD(void* vp) {
    return castVec<double>(vp)->size();
}


char* vectorDataC(void* vp) {
    return castVec<char>(vp)->data();
}

int* vectorDataI(void* vp) {
    return castVec<int>(vp)->data();
}

float* vectorDataF(void* vp) {
    return castVec<float>(vp)->data();
}

double* vectorDataD(void* vp) {
    return castVec<double>(vp)->data();
}


void vectorFreeC(void** vp) {
    vector<char>** p = (vector<char>**) vp;
    delete *p;
    delete p;
    return;
}

void vectorFreeI(void** vp) {
    vector<int>** p = (vector<int>**) vp;
    delete *p;
    delete p;
    return;
}

void vectorFreeF(void** vp) {
    vector<float>** p = (vector<float>**) vp;
    delete *p;
    delete p;
    return;
}

void vectorFreeD(void** vp) {
    vector<double>** p = (vector<double>**) vp;
    delete *p;
    delete p;
    return;
}

template <typename T>
int vectorSizeV(void* vp) {
cout << "int vectorSizeV(void* vp) {" << endl;
    vv<T>* vvp = (vv<T>*) vp;
    return vvp->vecs->size();
}

template <typename T>
void* vectorBPtrV(void* vp) {
cout << "void* vectorBPtrV(void* vp) {" << endl;
    vv<T>* vvp = (vv<T>*) vp;
    return (void*) vvp->vecs;
}

template <typename T>
void* vectorDataV(void* vp) {
cout << "void* vectorDataV(void* vp) {" << endl;
    vv<T>* vvp = (vv<T>*) vp;
    vvp->ptrs->resize(vvp->vecs->size());

    for (unsigned int i = 0; i < vvp->vecs->size(); i++)
        (*vvp->ptrs)[i] = &((*vvp->vecs)[i]);

    return (void*) vvp->ptrs;
}

template <typename T>
void vectorFreeV(void* vp) {
cout << "void vectorFreeV(void* vp) {" << endl;
    vv<T>* vvp = (vv<T>*) vp;

    delete vvp->vecs;
    delete vvp->ptrs;

    return;
}


#define VVFUNCS(T,C)            \
typedef vv<T> vv##C;            \
                                \
void* vectorNewV##C() {         \
    return vectorNewV<T>();     \
}                               \
                                \
int vectorSizeV##C(void* vp) {  \
    return vectorSizeV<T>(vp);  \
}                               \
                                \
void* vectorBPtrV##C(void* vp) {\
    return vectorBPtrV<T>(vp);  \
}                               \
                                \
void* vectorDataV##C(void* vp) {\
    return vectorDataV<T>(vp);  \
}                               \
                                \
void vectorFreeV##C(void* vp) { \
    return vectorFreeV<T>(vp);  \
}

VVFUNCS(double,D)
