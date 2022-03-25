# FFI auxiliary makefile script

ffi_libdir = $(shell $(PKG_CONFIG) --variable=libdir intro_c-practice || echo .)
ffi_incdir = $(shell $(PKG_CONFIG) --variable=includedir intro_c-practice || echo .)
LD_LIBRARY_PATH := $(LD_LIBRARY_PATH):$(ffi_libdir)
export LD_LIBRARY_PATH

ifeq ($(shell sh -c 'uname -s 2>/dev/null || echo not'),Darwin)
sosuffix = dylib
else
sosuffix = so
LDFLAGS := $(LDFLAGS) -Wl,--enable-new-dtags
endif

ifdef DEBUG
CPPFLAGS := $(CPPFLAGS) -DDEBUG -UNDEBUG
CFLAGS := $(CFLAGS) -g3 -O0 --coverage
LDFLAGS := $(LDFLAGS) --coverage
else
CPPFLAGS := $(CPPFLAGS) -DNDEBUG -UDEBUG
CFLAGS := $(CFLAGS) -O3
endif

CC = clang		# clang | gcc
CPPFLAGS := $(CPPFLAGS) -I$(ffi_incdir)
CFLAGS := $(CFLAGS) -Wall -pedantic -std=c99
ARFLAGS = rvcs
LDFLAGS := $(LDFLAGS) -Wl,-rpath,'$$ORIGIN/:$(ffi_libdir)' -L$(ffi_libdir)
LDLIBS := $(LDLIBS) -lintro_c-practice

$(OUTPUTPATH)/lib$(proj)_stubs.$(sosuffix) : build/ClassicC.c

$(OUTPUTPATH)/%.so : 
	-$(LINK.c) -fPIC -shared $^ -o $@ $(LDLIBS)
$(OUTPUTPATH)/%.dylib : 
	-$(LINK.c) -fPIC -dynamiclib -undefined suppress -flat_namespace $^ -o $@ $(LDLIBS)

.PHONY: prep_swig clean_swig auxffi
auxffi : prep_swig $(OUTPUTPATH)/lib$(proj)_stubs.a(build/ClassicC.o) $(OUTPUTPATH)/lib$(proj)_stubs.$(sosuffix) ## compile FFI auxiliary products
prep_swig : src/cs/$(namespace_path)/ClassicC.i ## prepare Swig files
	-swig -csharp -v -I$(ffi_incdir) -namespace $(proj) -dllimport $(proj)_stubs -outdir src/cs/$(namespace_path) -o build/ClassicC.c src/cs/$(namespace_path)/ClassicC.i
	-sed -i '' 's|CSharp_||g' src/cs/$(namespace_path)/ClassicCPINVOKE.cs
	-sed -i '' 's|CSharp_||g' build/ClassicC.c
clean_swig : ## clean Swig files
	-rm -fr src/cs/$(namespace_path)/ClassicC*.cs src/cs/$(namespace_path)/SWIGTYPE*.cs
