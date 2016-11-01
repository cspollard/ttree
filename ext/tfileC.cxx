#include "tfileC.h"
#include "TFile.h"


void* tfileOpen(const char* fn) {
    return (void*) TFile::Open(fn);
}

void* tfileGet(void* fp, const char* on) {
    return ((TFile*) fp)->Get(on);
}

void tfileClose(void* fp) {
    return ((TFile*) fp)->Close();
}
