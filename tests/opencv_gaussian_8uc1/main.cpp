//
// Copyright (c) 2012, University of Erlangen-Nuremberg
// Copyright (c) 2012, Siemens AG
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met: 
// 
// 1. Redistributions of source code must retain the above copyright notice, this
//    list of conditions and the following disclaimer. 
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution. 
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR 
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND 
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

#include <iostream>
#include <vector>

#include <float.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>

#ifdef OpenCV
#include "opencv2/gpu/gpu.hpp"
#include "opencv2/imgproc/imgproc_c.h"
#endif

#include "hipacc.hpp"

// variables set by Makefile
//#define SIZE_X 5
//#define SIZE_Y 5
//#define WIDTH 4096
//#define HEIGHT 4096
//#define CPU
//#define CONST_MASK
#define USE_LAMBDA
//#define RUN_UNDEF
//#define NO_SEP

using namespace hipacc;


// get time in milliseconds
double time_ms () {
    struct timeval tv;
    gettimeofday (&tv, NULL);

    return ((double)(tv.tv_sec) * 1e+3 + (double)(tv.tv_usec) * 1e-3);
}


// Gaussian blur filter reference
void gaussian_filter(unsigned char *in, unsigned char *out, float *filter, int
        size_x, int size_y, int width, int height) {
    int anchor_x = size_x >> 1;
    int anchor_y = size_y >> 1;
#ifdef OpenCV
    int upper_x = width-size_x+anchor_x;
    int upper_y = height-size_y+anchor_y;
#else
    int upper_x = width-anchor_x;
    int upper_y = height-anchor_y;
#endif

    for (int y=anchor_y; y<upper_y; ++y) {
        for (int x=anchor_x; x<upper_x; ++x) {
            float sum = 0.5f;

            for (int yf = -anchor_y; yf<=anchor_y; yf++) {
                for (int xf = -anchor_x; xf<=anchor_x; xf++) {
                    sum += filter[(yf+anchor_y)*size_x + xf+anchor_x]*in[(y+yf)*width + x + xf];
                }
            }
            out[y*width + x] = (unsigned char) (sum);
        }
    }
}
void gaussian_filter_row(unsigned char *in, float *out, float *filter, int
        size_x, int width, int height) {
    int anchor_x = size_x >> 1;
#ifdef OpenCV
    int upper_x = width-size_x+anchor_x;
#else
    int upper_x = width-anchor_x;
#endif

    for (int y=0; y<height; ++y) {
        //for (int x=0; x<anchor_x; x++) out[y*width + x] = in[y*width + x];
        for (int x=anchor_x; x<upper_x; ++x) {
            float sum = 0;

            for (int xf = -anchor_x; xf<=anchor_x; xf++) {
                sum += filter[xf+anchor_x]*in[(y)*width + x + xf];
            }
            out[y*width + x] = sum;
        }
        //for (int x=upper_x; x<width; x++) out[y*width + x] = in[y*width + x];
    }
}
void gaussian_filter_column(float *in, unsigned char *out, float *filter, int
        size_y, int width, int height) {
    int anchor_y = size_y >> 1;
#ifdef OpenCV
    int upper_y = height-size_y+anchor_y;
#else
    int upper_y = height-anchor_y;
#endif

    //for (int y=0; y<anchor_y; y++) {
    //    for (int x=0; x<width; ++x) {
    //        out[y*width + x] = (unsigned char) in[y*width + x];
    //    }
    //}
    for (int y=anchor_y; y<upper_y; ++y) {
        for (int x=0; x<width; ++x) {
            float sum = 0.5f;

            for (int yf = -anchor_y; yf<=anchor_y; yf++) {
                sum += filter[yf + anchor_y]*in[(y + yf)*width + x];
            }
            out[y*width + x] = (unsigned char) (sum);
        }
    }
    //for (int y=upper_y; y<height; y++) {
    //    for (int x=0; x<width; ++x) {
    //        out[y*width + x] = (unsigned char) in[y*width + x];
    //    }
    //}
}


// Gaussian blur filter in HIPAcc
#ifdef NO_SEP
class GaussianBlurFilterMask : public Kernel<unsigned char> {
    private:
        Accessor<unsigned char> &Input;
        Mask<float> &cMask;
        const int size_x, size_y;

    public:
        GaussianBlurFilterMask(IterationSpace<unsigned char> &IS,
                Accessor<unsigned char> &Input, Mask<float> &cMask, const int
                size_x, const int size_y) :
            Kernel(IS),
            Input(Input),
            cMask(cMask),
            size_x(size_x),
            size_y(size_y)
        { addAccessor(&Input); }

        #ifdef USE_LAMBDA
        void kernel() {
            float sum = 0.5f;
            sum += convolve(cMask, HipaccSUM, [&] () -> float {
                    return cMask() * Input(cMask);
                    });
            output() = (unsigned char) (sum);
        }
        #else
        void kernel() {
            const int anchor_x = size_x >> 1;
            const int anchor_y = size_y >> 1;
            float sum = 0.5f;

            for (int yf = -anchor_y; yf<=anchor_y; yf++) {
                for (int xf = -anchor_x; xf<=anchor_x; xf++) {
                    sum += cMask(xf, yf)*Input(xf, yf);
                }
            }

            output() = (unsigned char) sum;
        }
        #endif
};
#else
class GaussianBlurFilterMaskRow : public Kernel<float> {
    private:
        Accessor<unsigned char> &Input;
        Mask<float> &cMask;
        const int size;

    public:
        GaussianBlurFilterMaskRow(IterationSpace<float> &IS, Accessor<unsigned
                char> &Input, Mask<float> &cMask, const int size) :
            Kernel(IS),
            Input(Input),
            cMask(cMask),
            size(size)
        { addAccessor(&Input); }

        #ifdef USE_LAMBDA
        void kernel() {
            float sum = convolve(cMask, HipaccSUM, [&] () -> float {
                    return cMask() * Input(cMask);
                    });
            output() = sum;
        }
        #else
        void kernel() {
            const int anchor = size >> 1;
            float sum = 0;

            for (int xf = -anchor; xf<=anchor; xf++) {
                sum += cMask(xf, 0)*Input(xf, 0);
            }

            output() = sum;
        }
        #endif
};
class GaussianBlurFilterMaskColumn : public Kernel<unsigned char> {
    private:
        Accessor<float> &Input;
        Mask<float> &cMask;
        const int size;

    public:
        GaussianBlurFilterMaskColumn(IterationSpace<unsigned char> &IS,
                Accessor<float> &Input, Mask<float> &cMask, const int size) :
            Kernel(IS),
            Input(Input),
            cMask(cMask),
            size(size)
        { addAccessor(&Input); }

        #ifdef USE_LAMBDA
        void kernel() {
            float sum = 0.5f;
            sum += convolve(cMask, HipaccSUM, [&] () -> float {
                    return cMask() * Input(cMask);
                    });
            output() = (unsigned char) (sum);
        }
        #else
        void kernel() {
            const int anchor = size >> 1;
            float sum = 0.5f;

            for (int yf = -anchor; yf<=anchor; yf++) {
                sum += cMask(0, yf)*Input(0, yf);
            }

            output() = (unsigned char) (sum);
        }
        #endif
};
#endif


/*************************************************************************
 * Main function                                                         *
 *************************************************************************/
int main(int argc, const char **argv) {
    double time0, time1, dt, min_dt;
    const int width = WIDTH;
    const int height = HEIGHT;
    const int size_x = SIZE_X;
    const int size_y = SIZE_Y;
    const int offset_x = size_x >> 1;
    const int offset_y = size_y >> 1;
    std::vector<float> timings;
    float timing = 0.0f;

    // filter coefficients
#ifdef CONST_MASK
    // only filter kernel sizes 3x3, 5x5, and 7x7 implemented
    if (size_x != size_y || !(size_x == 3 || size_x == 5 || size_x == 7)) {
        fprintf(stderr, "Wrong filter kernel size. Currently supported values: 3x3, 5x5, and 7x7!\n");
        exit(EXIT_FAILURE);
    }

    // convolution filter mask
    const float filter_x[] = {
        #if SIZE_X == 3
        0.238994f, 0.522011f, 0.238994f
        #endif
        #if SIZE_X == 5
        0.070766f, 0.244460f, 0.369546f, 0.244460f, 0.070766f
        #endif
        #if SIZE_X == 7
        0.028995f, 0.103818f, 0.223173f, 0.288026f, 0.223173f, 0.103818f, 0.028995f
        #endif
    };
    const float filter_y[] = {
        #if SIZE_X == 3
        0.238994f, 0.522011f, 0.238994f
        #endif
        #if SIZE_X == 5
        0.070766f, 0.244460f, 0.369546f, 0.244460f, 0.070766f
        #endif
        #if SIZE_X == 7
        0.028995f, 0.103818f, 0.223173f, 0.288026f, 0.223173f, 0.103818f, 0.028995f
        #endif
    };
    const float filter_xy[] = {
        #if SIZE_X == 3
        0.057118f, 0.124758f, 0.057118f,
        0.124758f, 0.272496f, 0.124758f,
        0.057118f, 0.124758f, 0.057118f
        #endif
        #if SIZE_X == 5
        0.005008f, 0.017300f, 0.026151f, 0.017300f, 0.005008f,
        0.017300f, 0.059761f, 0.090339f, 0.059761f, 0.017300f,
        0.026151f, 0.090339f, 0.136565f, 0.090339f, 0.026151f,
        0.017300f, 0.059761f, 0.090339f, 0.059761f, 0.017300f,
        0.005008f, 0.017300f, 0.026151f, 0.017300f, 0.005008f
        #endif
        #if SIZE_X == 7
        0.000841, 0.003010, 0.006471, 0.008351, 0.006471, 0.003010, 0.000841,
        0.003010, 0.010778, 0.023169, 0.029902, 0.023169, 0.010778, 0.003010,
        0.006471, 0.023169, 0.049806, 0.064280, 0.049806, 0.023169, 0.006471,
        0.008351, 0.029902, 0.064280, 0.082959, 0.064280, 0.029902, 0.008351,
        0.006471, 0.023169, 0.049806, 0.064280, 0.049806, 0.023169, 0.006471,
        0.003010, 0.010778, 0.023169, 0.029902, 0.023169, 0.010778, 0.003010,
        0.000841, 0.003010, 0.006471, 0.008351, 0.006471, 0.003010, 0.000841
        #endif
    };
#else
    float *filter_x = (float *)malloc(sizeof(float)*size_x);
    float *filter_y = (float *)malloc(sizeof(float)*size_y);
    float *filter_xy = (float *)malloc(sizeof(float)*size_x*size_y);
    const double sigma1 = ((size_x-1)*0.5 - 1)*0.3 + 0.8;
    const double sigma2 = ((size_y-1)*0.5 - 1)*0.3 + 0.8;

    double scale2X = -0.5/(sigma1*sigma1);
    double scale2Y = -0.5/(sigma2*sigma2);
    double sum_x = 0.;
    double sum_y = 0.;

    for (int i=0; i < size_x; i++) {
        double x = i - (size_x-1)*0.5;
        double t = exp(scale2X*x*x);

        filter_x[i] = (float)t;
        sum_x += filter_x[i];
    }
    for (int i=0; i < size_y; i++) {
        double x = i - (size_y-1)*0.5;
        double t = exp(scale2Y*x*x);

        filter_y[i] = (float)t;
        sum_y += filter_y[i];
    }

    sum_x = 1./sum_x;
    sum_y = 1./sum_y;
    for (int i=0; i < size_x; i++) {
        filter_x[i] = (float)(filter_x[i]*sum_x);
    }
    for (int i=0; i < size_y; i++) {
        filter_y[i] = (float)(filter_y[i]*sum_y);
    }

    for (int y=0; y < size_y; y++) {
        for (int x=0; x < size_x; x++) {
            filter_xy[y*size_x + x] = filter_x[x]*filter_y[y];
        }
    }
#endif

    // host memory for image of of width x height pixels
    unsigned char *host_in = (unsigned char *)malloc(sizeof(unsigned char)*width*height);
    unsigned char *host_out = (unsigned char *)malloc(sizeof(unsigned char)*width*height);
    unsigned char *reference_in = (unsigned char *)malloc(sizeof(unsigned char)*width*height);
    unsigned char *reference_out = (unsigned char *)malloc(sizeof(unsigned char)*width*height);
    float *reference_tmp = (float *)malloc(sizeof(float)*width*height);

    // initialize data
    for (int y=0; y<height; ++y) {
        for (int x=0; x<width; ++x) {
            host_in[y*width + x] = (unsigned char)(y*width + x) % 256;
            reference_in[y*width + x] = (unsigned char)(y*width + x) % 256;
            host_out[y*width + x] = 0;
            reference_out[y*width + x] = 0;
            reference_tmp[y*width + x] = 0;
        }
    }


    // input and output image of width x height pixels
    Image<unsigned char> IN(width, height);
    Image<unsigned char> OUT(width, height);
    Image<float> TMP(width, height);

    // filter mask
    Mask<float> M(size_x, size_y);
    Mask<float> MX(size_x, 1);
    Mask<float> MY(1, size_y);
    M = filter_xy;
    MX = filter_x;
    MY = filter_y;

    IterationSpace<unsigned char> IsOut(OUT);
    IterationSpace<float> IsTmp(TMP);

    IN = host_in;
    OUT = host_out;


#ifndef OpenCV
    fprintf(stderr, "Calculating HIPAcc Gaussian filter ...\n");

    // BOUNDARY_UNDEFINED
    #ifdef RUN_UNDEF
    #ifdef NO_SEP
    BoundaryCondition<unsigned char> BcInUndef2(IN, size_x, size_y, BOUNDARY_UNDEFINED);
    Accessor<unsigned char> AccInUndef2(BcInUndef2);
    GaussianBlurFilterMask GFU(IsOut, AccInUndef2, M, size_x, size_y);

    GFU.execute();
    timing = hipaccGetLastKernelTiming();
    #else
    BoundaryCondition<unsigned char> BcInUndef(IN, size_x, 1, BOUNDARY_UNDEFINED);
    Accessor<unsigned char> AccInUndef(BcInUndef);
    GaussianBlurFilterMaskRow GFRU(IsTmp, AccInUndef, MX, size_x);

    BoundaryCondition<float> BcTmpUndef(TMP, 1, size_y, BOUNDARY_UNDEFINED);
    Accessor<float> AccTmpUndef(BcTmpUndef);
    GaussianBlurFilterMaskColumn GFCU(IsOut, AccTmpUndef, MY, size_y);

    GFRU.execute();
    timing = hipaccGetLastKernelTiming();
    GFCU.execute();
    timing += hipaccGetLastKernelTiming();
    #endif
    #endif
    timings.push_back(timing);
    fprintf(stderr, "HIPACC (UNDEFINED): %.3f ms, %.3f Mpixel/s\n", timing, (width*height/timing)/1000);


    // BOUNDARY_CLAMP
    #ifdef NO_SEP
    BoundaryCondition<unsigned char> BcInClamp2(IN, size_x, size_y, BOUNDARY_CLAMP);
    Accessor<unsigned char> AccInClamp2(BcInClamp2);
    GaussianBlurFilterMask GFC(IsOut, AccInClamp2, M, size_x, size_y);

    GFC.execute();
    timing = hipaccGetLastKernelTiming();
    #else
    BoundaryCondition<unsigned char> BcInClamp(IN, size_x, 1, BOUNDARY_CLAMP);
    Accessor<unsigned char> AccInClamp(BcInClamp);
    GaussianBlurFilterMaskRow GFRC(IsTmp, AccInClamp, MX, size_x);

    BoundaryCondition<float> BcTmpClamp(TMP, 1, size_y, BOUNDARY_CLAMP);
    Accessor<float> AccTmpClamp(BcTmpClamp);
    GaussianBlurFilterMaskColumn GFCC(IsOut, AccTmpClamp, MY, size_y);

    GFRC.execute();
    timing = hipaccGetLastKernelTiming();
    GFCC.execute();
    timing += hipaccGetLastKernelTiming();
    #endif
    timings.push_back(timing);
    fprintf(stderr, "HIPACC (CLAMP): %.3f ms, %.3f Mpixel/s\n", timing, (width*height/timing)/1000);


    // BOUNDARY_REPEAT
    #ifdef NO_SEP
    BoundaryCondition<unsigned char> BcInRepeat2(IN, size_x, size_y, BOUNDARY_REPEAT);
    Accessor<unsigned char> AccInRepeat2(BcInRepeat2);
    GaussianBlurFilterMask GFR(IsOut, AccInRepeat2, M, size_x, size_y);

    GFR.execute();
    timing = hipaccGetLastKernelTiming();
    #else
    BoundaryCondition<unsigned char> BcInRepeat(IN, size_x, 1, BOUNDARY_REPEAT);
    Accessor<unsigned char> AccInRepeat(BcInRepeat);
    GaussianBlurFilterMaskRow GFRR(IsTmp, AccInRepeat, MX, size_x);

    BoundaryCondition<float> BcTmpRepeat(TMP, 1, size_y, BOUNDARY_REPEAT);
    Accessor<float> AccTmpRepeat(BcTmpRepeat);
    GaussianBlurFilterMaskColumn GFCR(IsOut, AccTmpRepeat, MY, size_y);

    GFRR.execute();
    timing = hipaccGetLastKernelTiming();
    GFCR.execute();
    timing += hipaccGetLastKernelTiming();
    #endif
    timings.push_back(timing);
    fprintf(stderr, "HIPACC (REPEAT): %.3f ms, %.3f Mpixel/s\n", timing, (width*height/timing)/1000);


    // BOUNDARY_MIRROR
    #ifdef NO_SEP
    BoundaryCondition<unsigned char> BcInMirror2(IN, size_x, size_y, BOUNDARY_MIRROR);
    Accessor<unsigned char> AccInMirror2(BcInMirror2);
    GaussianBlurFilterMask GFM(IsOut, AccInMirror2, M, size_x, size_y);

    GFM.execute();
    timing = hipaccGetLastKernelTiming();
    #else
    BoundaryCondition<unsigned char> BcInMirror(IN, size_x, 1, BOUNDARY_MIRROR);
    Accessor<unsigned char> AccInMirror(BcInMirror);
    GaussianBlurFilterMaskRow GFRM(IsTmp, AccInMirror, MX, size_x);

    BoundaryCondition<float> BcTmpMirror(TMP, 1, size_y, BOUNDARY_MIRROR);
    Accessor<float> AccTmpMirror(BcTmpMirror);
    GaussianBlurFilterMaskColumn GFCM(IsOut, AccTmpMirror, MY, size_y);

    GFRM.execute();
    timing = hipaccGetLastKernelTiming();
    GFCM.execute();
    timing += hipaccGetLastKernelTiming();
    #endif
    timings.push_back(timing);
    fprintf(stderr, "HIPACC (MIRROR): %.3f ms, %.3f Mpixel/s\n", timing, (width*height/timing)/1000);


    // BOUNDARY_CONSTANT
    #ifdef NO_SEP
    BoundaryCondition<unsigned char> BcInConst2(IN, size_x, size_y, BOUNDARY_CONSTANT, '1');
    Accessor<unsigned char> AccInConst2(BcInConst2);
    GaussianBlurFilterMask GFConst(IsOut, AccInConst2, M, size_x, size_y);

    GFConst.execute();
    timing = hipaccGetLastKernelTiming();
    #else
    BoundaryCondition<unsigned char> BcInConst(IN, size_x, 1, BOUNDARY_CONSTANT, '1');
    Accessor<unsigned char> AccInConst(BcInConst);
    GaussianBlurFilterMaskRow GFRConst(IsTmp, AccInConst, MX, size_x);

    BoundaryCondition<float> BcTmpConst(TMP, 1, size_y, BOUNDARY_CONSTANT, 1.0f);
    Accessor<float> AccTmpConst(BcTmpConst);
    GaussianBlurFilterMaskColumn GFCConst(IsOut, AccTmpConst, MY, size_y);

    GFRConst.execute();
    timing = hipaccGetLastKernelTiming();
    GFCConst.execute();
    timing += hipaccGetLastKernelTiming();
    #endif
    timings.push_back(timing);
    fprintf(stderr, "HIPACC (CONSTANT): %.3f ms, %.3f Mpixel/s\n", timing, (width*height/timing)/1000);


    // get results
    host_out = OUT.getData();
#endif



#ifdef OpenCV
#ifdef CPU
    fprintf(stderr, "\nCalculating OpenCV Gaussian filter on the CPU ...\n");
#else
    fprintf(stderr, "\nCalculating OpenCV Gaussian filter on the GPU ...\n");
#endif


    cv::Mat cv_data_in(height, width, CV_8UC1, host_in);
    cv::Mat cv_data_out(height, width, CV_8UC1, host_out);
    cv::Size ksize(size_x, size_y);

    for (int brd_type=0; brd_type<5; brd_type++) {
#ifdef CPU
        min_dt = DBL_MAX;
        for (int nt=0; nt<10; nt++) {
            time0 = time_ms();

            cv::GaussianBlur(cv_data_in, cv_data_out, ksize, sigma1, sigma2, brd_type);

            time1 = time_ms();
            dt = time1 - time0;
            if (dt < min_dt) min_dt = dt;
        }
#else
        cv::gpu::GpuMat gpu_in, gpu_out;
        gpu_in.upload(cv_data_in);

        min_dt = DBL_MAX;
        for (int nt=0; nt<10; nt++) {
            time0 = time_ms();

            cv::gpu::GaussianBlur(gpu_in, gpu_out, ksize, sigma1, sigma2, brd_type);

            time1 = time_ms();
            dt = time1 - time0;
            if (dt < min_dt) min_dt = dt;
        }

        gpu_out.download(cv_data_out);
#endif

        fprintf(stderr, "OpenCV(");
        switch (brd_type) {
            case IPL_BORDER_CONSTANT:
                fprintf(stderr, "CONSTANT");
                break;
            case IPL_BORDER_REPLICATE:
                fprintf(stderr, "CLAMP");
                break;
            case IPL_BORDER_REFLECT:
                fprintf(stderr, "MIRROR");
                break;
            case IPL_BORDER_WRAP:
                fprintf(stderr, "REPEAT");
                break;
            case IPL_BORDER_REFLECT_101:
                fprintf(stderr, "MIRROR_101");
                break;
            default:
                break;
        }
        timings.push_back(min_dt);
        fprintf(stderr, "): %.3f ms, %.3f Mpixel/s\n", min_dt, (width*height/min_dt)/1000);
    }
#endif

    // print statistics
    for (unsigned int i=0; i<timings.size(); i++) {
        fprintf(stderr, "\t%.3f", timings.data()[i]);
    }
    fprintf(stderr, "\n\n");


    fprintf(stderr, "\nCalculating reference ...\n");
    min_dt = DBL_MAX;
    for (int nt=0; nt<3; nt++) {
        time0 = time_ms();

        // calculate reference
        #ifdef NO_SEP
        gaussian_filter(reference_in, reference_out, (float *)filter_xy, size_x, size_y, width, height);
        #else
        gaussian_filter_row(reference_in, reference_tmp, (float *)filter_x, size_x, width, height);
        gaussian_filter_column(reference_tmp, reference_out, (float *)filter_y, size_y, width, height);
        #endif

        time1 = time_ms();
        dt = time1 - time0;
        if (dt < min_dt) min_dt = dt;
    }
    fprintf(stderr, "Reference: %.3f ms, %.3f Mpixel/s\n", min_dt, (width*height/min_dt)/1000);

    fprintf(stderr, "\nComparing results ...\n");
#ifdef OpenCV
    int upper_y = height-size_y+offset_y;
    int upper_x = width-size_x+offset_x;
#else
    int upper_y = height-offset_y;
    int upper_x = width-offset_x;
#endif
    // compare results
    for (int y=offset_y; y<upper_y; y++) {
        for (int x=offset_x; x<upper_x; x++) {
            if (reference_out[y*width + x] != host_out[y*width + x]) {
                fprintf(stderr, "Test FAILED, at (%d,%d): %hhu vs. %hhu\n", x,
                        y, reference_out[y*width + x], host_out[y*width + x]);
                exit(EXIT_FAILURE);
            }
        }
    }
    fprintf(stderr, "Test PASSED\n");

    // memory cleanup
    free(host_in);
    //free(host_out);
    free(reference_in);
    free(reference_tmp);
    free(reference_out);

    return EXIT_SUCCESS;
}

