#include "ttreeC.h"
#include "TFile.h"
#include "TTree.h"
#include <vector>

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
