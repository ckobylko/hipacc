//
// Copyright (c) 2013, University of Erlangen-Nuremberg
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice,
//    this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//

#include <iostream>
#include <vector>

#include <float.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>

#include "hipacc.hpp"

// variables set by Makefile
//#define SIZE_X 5
//#define SIZE_Y 5
//#define WIDTH 4096
//#define HEIGHT 4096
#define USE_LAMBDA

using namespace hipacc;

class Gaussian : public Kernel<char> {
  private:
    Accessor<char> &Input;
    Mask<float> &cMask;
    int size_x;
    int size_y;

  public:
    Gaussian(IterationSpace<char> &IS, Accessor<char> &Input,
             Mask<float> &cMask, int size_x, int size_y)
        : Kernel(IS),
          Input(Input),
          cMask(cMask),
          size_x(size_x),
          size_y(size_y) {
      addAccessor(&Input);
    }

    void kernel() {
#ifdef USE_LAMBDA
      output() = convolve(cMask, HipaccSUM, [&] () {
        return Input(cMask) * cMask();
      });
#else
      const int anchor_x = size_x >> 1;
      const int anchor_y = size_y >> 1;
      float sum = 0.0f;

      for (int yf = -anchor_y; yf<=anchor_y; yf++) {
        for (int xf = -anchor_x; xf<=anchor_x; xf++) {
          sum += cMask(xf, yf)*Input(xf, yf);
        }
      }

      output() = (unsigned char) sum;
#endif
    }
};

class Subsample : public Kernel<char> {
  private:
    Accessor<char> &Input;

  public:
    Subsample(IterationSpace<char> &IS, Accessor<char> &Input)
        : Kernel(IS),
          Input(Input) {
      addAccessor(&Input);
    }

    void kernel() {
      output() = Input();
    }
};

class DifferenceOfGaussian : public Kernel<char> {
  private:
    Accessor<char> &Input1;
    Accessor<char> &Input2;

  public:
    DifferenceOfGaussian(IterationSpace<char> &IS, Accessor<char> &Input1,
                         Accessor<char> &Input2)
        : Kernel(IS),
          Input1(Input1),
          Input2(Input2) {
      addAccessor(&Input1);
      addAccessor(&Input2);
    }

    void kernel() {
      output() = Input1() - Input2();
    }
};

class Collapse : public Kernel<char> {
  private:
    Accessor<char> &Input1;
    Accessor<char> &Input2;

  public:
    Collapse(IterationSpace<char> &IS, Accessor<char> &Input1,
             Accessor<char> &Input2)
        : Kernel(IS),
          Input1(Input1),
          Input2(Input2) {
      addAccessor(&Input1);
      addAccessor(&Input2);
    }

    void kernel() {
      output() = Input1() + Input2();
    }
};

/*************************************************************************
 * Main function                                                         *
 *************************************************************************/
int main(int argc, const char **argv) {
    const int width = WIDTH;
    const int height = HEIGHT;
    const int size_x = SIZE_X;
    const int size_y = SIZE_Y;

    // filter coefficients
    // only filter kernel sizes 3x3, 5x5, and 7x7 implemented
    if (size_x != size_y || !(size_x == 3 || size_x == 5 || size_x == 7)) {
        fprintf(stderr, "Wrong filter kernel size. Currently supported values: 3x3, 5x5, and 7x7!\n");
        exit(EXIT_FAILURE);
    }

    // convolution filter mask
    const float filter_xy[SIZE_Y][SIZE_X] = {
#if SIZE_X == 3
        { 0.057118f, 0.124758f, 0.057118f },
        { 0.124758f, 0.272496f, 0.124758f },
        { 0.057118f, 0.124758f, 0.057118f }
#endif
#if SIZE_X == 5
        { 0.005008f, 0.017300f, 0.026151f, 0.017300f, 0.005008f },
        { 0.017300f, 0.059761f, 0.090339f, 0.059761f, 0.017300f },
        { 0.026151f, 0.090339f, 0.136565f, 0.090339f, 0.026151f },
        { 0.017300f, 0.059761f, 0.090339f, 0.059761f, 0.017300f },
        { 0.005008f, 0.017300f, 0.026151f, 0.017300f, 0.005008f }
#endif
#if SIZE_X == 7
        { 0.000841, 0.003010, 0.006471, 0.008351, 0.006471, 0.003010, 0.000841 },
        { 0.003010, 0.010778, 0.023169, 0.029902, 0.023169, 0.010778, 0.003010 },
        { 0.006471, 0.023169, 0.049806, 0.064280, 0.049806, 0.023169, 0.006471 },
        { 0.008351, 0.029902, 0.064280, 0.082959, 0.064280, 0.029902, 0.008351 },
        { 0.006471, 0.023169, 0.049806, 0.064280, 0.049806, 0.023169, 0.006471 },
        { 0.003010, 0.010778, 0.023169, 0.029902, 0.023169, 0.010778, 0.003010 },
        { 0.000841, 0.003010, 0.006471, 0.008351, 0.006471, 0.003010, 0.000841 }
#endif
    };

    // host memory for image of width x height pixels
    char *host_in = (char *)malloc(sizeof(char)*width*height);
    char *host_out = (char *)malloc(sizeof(char)*width*height);

    // initialize data
    for (int y=0; y<height; ++y) {
        for (int x=0; x<width; ++x) {
            host_in[y*width + x] = (char)(y*width + x) % 256;
            host_out[y*width + x] = 0;
        }
    }

    // input and output image of width x height pixels
    Image<char> GAUS(width, height);
    Image<char> TMP(width, height);
    Image<char> LAP(width, height);
    Mask<float> cMask(filter_xy);

    GAUS = host_in;
    TMP = host_out;
    LAP = host_out;

    int depth = 1;
    {
      int w = width/2;
      int h = height/2;
      while (w > 0 && h > 0) {
        depth++;
        w /= 2;
        h /= 2;
      }
    }

    Pyramid<char> PGAUS(GAUS, depth);
    Pyramid<char> PTMP(TMP, depth);
    Pyramid<char> PLAP(LAP, depth);

    traverse(PGAUS, PTMP, PLAP, [&] () {
        if (!PGAUS.isTopLevel()) {
          // Construct gaussian pyramid
          BoundaryCondition<char> BC(PGAUS(-1), cMask, BOUNDARY_CLAMP);
          Accessor<char> Acc1(BC);
          IterationSpace<char> IS1(PTMP(-1));
          Gaussian Gaus(IS1, Acc1, cMask, size_x, size_y);
          printf("Level %d: Gaussian\n", PGAUS.getLevel()-1);
          Gaus.execute();

          AccessorNN<char> Acc2(PTMP(-1));
          IterationSpace<char> IS2(PGAUS(0));
          Subsample Sub(IS2, Acc2);
          printf("Level %d: Subsample\n", PGAUS.getLevel()-1);
          Sub.execute();

          // Construct lapacian pyramid
          Accessor<char> Acc3(PGAUS(-1));
          AccessorLF<char> Acc4(PGAUS(0));
          IterationSpace<char> IS3(PLAP(-1));
          DifferenceOfGaussian DoG(IS3, Acc3, Acc4);
          printf("Level %d: DifferenceOfGaussian\n", PGAUS.getLevel()-1);
          DoG.execute();
        }

        traverse();

        // Collapse laplacian pyramid
        if (!PGAUS.isBottomLevel()) {
          AccessorLF<char> Acc1(PGAUS(1));
          Accessor<char> Acc2(PLAP(0));
          IterationSpace<char> IS(PGAUS(0));
          Collapse Col(IS, Acc1, Acc2);
          printf("Level %d: Collapse\n", PGAUS.getLevel());
          Col.execute();
        }
    });

    host_out = GAUS.getData();

    for (int y = 0; y < height; ++y) {
      for (int x = 0; x < width; ++x) {
        if (host_in[y*height+x] != host_out[y*height+x]) {
          fprintf(stderr, "Test FAILED, at (%d,%d): %hhu vs. %hhu\n",
                  x, y, host_in[y*width + x], host_out[y*width + x]);
          exit(EXIT_FAILURE);
        }
      }
    }

    fprintf(stderr, "Test PASSED\n");

    free(host_in);
    free(host_out);

    return EXIT_SUCCESS;
}

