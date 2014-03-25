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

//===--- AcceleratorDeviceBase.h - Base for accelerator device backends. -------------===//
//
// This file contains the base class for accelerator device backends (like CUDA, OpenCL etc.).
//
//===---------------------------------------------------------------------------------===//

#ifndef _BACKEND_ACCELERATOR_DEVICE_BASE_H_
#define _BACKEND_ACCELERATOR_DEVICE_BASE_H_

#include "hipacc/Device/TargetDevices.h"
#include "BackendExceptions.h"
#include "CommonDefines.h"
#include <string>

namespace clang
{
namespace hipacc
{
namespace Backend
{
	class AcceleratorDeviceBase
	{
	protected:

		class AcceleratorDeviceSwitches final
		{
		public:

			struct EmitPadding final
			{
				inline static std::string Key()					{ return "-emit-padding"; }
				inline static std::string AdditionalOptions()	{ return "<n>"; }
				inline static std::string Description()			{ return "Emit CUDA/OpenCL/Renderscript image padding, using alignment of <n> bytes for GPU devices"; }


				typedef CommonDefines::OptionParsers::Integer	OptionParser;
			};

			struct ExploreConfig final
			{
				inline static std::string Key()					{ return "-explore-config"; }
				inline static std::string AdditionalOptions()	{ return ""; }
				inline static std::string Description()			{ return "Emit code that explores all possible kernel configuration and print its performance"; }
			};

			struct PixelsPerThread final
			{
				inline static std::string Key()					{ return "-pixels-per-thread"; }
				inline static std::string AdditionalOptions()	{ return "<n>"; }
				inline static std::string Description()			{ return "Specify how many pixels should be calculated per thread"; }


				typedef CommonDefines::OptionParsers::Integer	OptionParser;
			};

			struct Target final
			{
				inline static std::string Key()					{ return "-target"; }
				inline static std::string AdditionalOptions()	{ return "<n>"; }
				inline static std::string Description()
				{
					std::string strDescription("");

					strDescription += "Generate code for GPUs with code name <n>.\n";
					strDescription += "Code names for CUDA/OpenCL on NVIDIA devices are:\n";
					strDescription += "  'Tesla-10', 'Tesla-11', 'Tesla-12', and 'Tesla-13' for Tesla architecture.\n";
					strDescription += "  'Fermi-20' and 'Fermi-21' for Fermi architecture.\n";
					strDescription += "  'Kepler-30' and 'Kepler-35' for Kepler architecture.\n";
					strDescription += "Code names for for OpenCL on AMD devices are:\n";
					strDescription += "  'Evergreen'      for Evergreen architecture (Radeon HD5xxx).\n";
					strDescription += "  'NorthernIsland' for Northern Island architecture (Radeon HD6xxx).\n";
					strDescription += "Code names for for OpenCL on ARM devices are:\n";
					strDescription += "  'Midgard' for Mali-T6xx' for Mali.\n";
					strDescription += "Code names for for OpenCL on Intel Xeon Phi devices are:\n";
					strDescription += "  'KnightsCorner' for Knights Corner Many Integrated Cores architecture.";

					return strDescription;
				}
			
			
				struct OptionParser final
				{
					typedef ::clang::hipacc::TargetDevice	ReturnType;

					inline static ReturnType Parse(std::string strOption)
					{
						if		(strOption == "Tesla-10")			return ::clang::hipacc::TESLA_10;
						else if	(strOption == "Tesla-11")			return ::clang::hipacc::TESLA_11;
						else if	(strOption == "Tesla-12")			return ::clang::hipacc::TESLA_12;
						else if	(strOption == "Tesla-13")			return ::clang::hipacc::TESLA_13;
						else if	(strOption == "Fermi-20")			return ::clang::hipacc::FERMI_20;
						else if	(strOption == "Fermi-21")			return ::clang::hipacc::FERMI_21;
						else if	(strOption == "Kepler-30")			return ::clang::hipacc::KEPLER_30;
						else if	(strOption == "Kepler-35")			return ::clang::hipacc::KEPLER_35;
						else if	(strOption == "Evergreen")			return ::clang::hipacc::EVERGREEN;
						else if	(strOption == "NorthernIsland")		return ::clang::hipacc::NORTHERN_ISLAND;
						else if	(strOption == "Midgard")			return ::clang::hipacc::MIDGARD;
						else if	(strOption == "KnightsCorner")		return ::clang::hipacc::KNIGHTSCORNER;
						else
						{
							throw InvalidOptionException(Key(), strOption);
						}
					}
				};
			};

			struct TimeKernels final
			{
				inline static std::string Key()					{ return "-time-kernels"; }
				inline static std::string AdditionalOptions()	{ return ""; }
				inline static std::string Description()			{ return "Emit code that executes each kernel multiple times to get accurate timings"; }
			};

			struct UseConfig final
			{
				inline static std::string Key()					{ return "-use-config"; }
				inline static std::string AdditionalOptions()	{ return "<nxm>"; }
				inline static std::string Description()			{ return "Emit code that uses a configuration of nxm threads, e.g. 128x1"; }


				struct OptionParser final
				{
					typedef std::pair< int, int >	ReturnType;

					inline static ReturnType Parse(std::string strOption)
					{
						int x = 0, y = 0;
						if (sscanf(strOption.c_str(), "%dx%d", &x, &y) != 2)
						{
							throw InvalidOptionException(Key(), strOption);
						}

						return ReturnType(x, y);
					}
				};
			};

			struct UseLocal final
			{
				inline static std::string Key()					{ return "-use-local"; }
				inline static std::string AdditionalOptions()	{ return "<o>"; }
				inline static std::string Description()
				{
					std::string strDescription("");

					strDescription += "Enable/disable usage of shared/local memory in CUDA/OpenCL to stage image pixels to scratchpad\n";
					strDescription += "Valid values: 'on' and 'off'";

					return strDescription;
				}


				typedef CommonDefines::OptionParsers::OnOff	OptionParser;
			};

			struct UseTextures final
			{
				inline static std::string Key()					{ return "-use-textures"; }
				inline static std::string AdditionalOptions()	{ return "<o>"; }
				inline static std::string Description()
				{
					std::string strDescription("");

					strDescription += "Enable/disable usage of textures (cached) in CUDA/OpenCL to read/write image pixels - for GPU devices only\n";
					strDescription += "Valid values for CUDA on NVIDIA devices: 'off', 'Linear1D', 'Linear2D', 'Array2D', and 'Ldg'\n";
					strDescription += "Valid values for OpenCL: 'off' and 'Array2D'";

					return strDescription;
				}


				struct OptionParser final
				{
					typedef ::clang::hipacc::TextureType	ReturnType;

					inline static ReturnType Parse(std::string strOption)
					{
						if		(strOption == "off")		return ::clang::hipacc::NoTexture;
						else if	(strOption == "Linear1D")	return ::clang::hipacc::Linear1D;
						else if	(strOption == "Linear2D")	return ::clang::hipacc::Linear2D;
						else if	(strOption == "Array2D")	return ::clang::hipacc::Array2D;
						else if	(strOption == "Ldg")		return ::clang::hipacc::Ldg;
						else
						{
							throw InvalidOptionException(Key(), strOption);
						}
					}
				};
			};

			struct Vectorize final
			{
				inline static std::string Key()					{ return "-vectorize"; }
				inline static std::string AdditionalOptions()	{ return "<o>"; }
				inline static std::string Description()
				{
					std::string strDescription("");

					strDescription += "Enable/disable vectorization of generated CUDA/OpenCL code\n";
					strDescription += "Valid values: 'on' and 'off'";

					return strDescription;
				}


				typedef CommonDefines::OptionParsers::OnOff	OptionParser;
			};
		};
	};
} // end namespace Backend
} // end namespace hipacc
} // end namespace clang


#endif  // _BACKEND_ACCELERATOR_DEVICE_BASE_H_

// vim: set ts=2 sw=2 sts=2 et ai:

